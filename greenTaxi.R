library(jsonlite)
library(leaflet)
library(sp)
library(tidyverse)
library(rgdal)
library(hexbin)
library(data.table)
library(raster)
library(rgeos)

##taxis <- fread("~/Downloads/2016_Green_Taxi_Trip_Data.csv")

boros <- readOGR("~/Downloads/nybb_18d/","nybb")
boros <- boros[boros$BoroName != "Staten Island",]
boros <- spTransform(boros,CRS("+init=epsg:4326"))

writeOGR(boros, "boros.GeoJSON", layer="boros", driver="GeoJSON")

borosUTM <- CRS("+proj=utm +zone=18N +datum=WGS84 +units=km +no_defs") %>%
  spTransform(boros, .)


make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter,
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

hexNoClip <- make_grid(borosUTM, cell_area = .4, clip = FALSE)
plot(boros, col = "grey50", bg = "light blue", axes = FALSE)
plot(hexNoClip, border = "orange", add = TRUE)
box()

spdf <- SpatialPolygonsDataFrame(hex_grid,data=data.frame("color"=rep(brewer.pal(5,"Dark2"),2005/5)))
spdf <- spTransform(spdf,CRS("+init=epsg:4326"))
writeOGR(spdf, "hexGrid.GeoJSON", layer="hex", driver="GeoJSON")




taxisShort <- taxis[1:500000,c("Pickup_longitude","Pickup_latitude","lpep_pickup_datetime","Lpep_dropoff_datetime")]

pHours <- unlist(lapply(taxisShort$lpep_pickup_datetime,function(x) as.numeric(substr(x,12,13))))
pHoursAM <- ifelse(grepl("PM",taxisShort$lpep_pickup_datetime),pHours+12,pHours)
taxisShort$pHour <- pHoursAM

dHours <- unlist(lapply(taxisShort$lpep_pickup_datetime,function(x) as.numeric(substr(x,12,13))))
dHoursAM <- ifelse(grepl("PM",taxisShort$Lpep_dropoff_datetime),dHours+12,dHours)
taxisShort$dHour <- dHoursAM

taxisShort$wday <- weekdays(as.Date(substr(taxisShort$lpep_pickup_datetime,1,10),format="%M/%d/%Y"))

coords <- taxisShort[!is.na(taxisShort$Pickup_longitude)&
                     !is.na(taxisShort$Pickup_latitude),
                     c("Pickup_longitude","Pickup_latitude","pHour","wday")]
coordinates(coords) <- ~Pickup_longitude+Pickup_latitude


spdf <- SpatialPointsDataFrame(coords=coords, data=data.frame("phour"=coords$pHour,"wday"=coords$wday),proj4string=CRS("+init=epsg:4326"))
proj4string(spdf) <- CRS("+init=epsg:4326")

borosUTM <- CRS("+proj=utm +zone=18N +datum=WGS84 +units=km +no_defs") %>%
  spTransform(boros, .)
hexClip <- make_grid(borosUTM, cell_area = .4, clip = FALSE)
hex <- SpatialPolygonsDataFrame(hexClip,data=data.frame("index"=1:1925))
hex <- spTransform(hex,proj4string(spdf))
hex$index <- 1:nrow(hex)
overDF <- over(spdf,hex)
spdf$index <- overDF$index
puHrCount <- table(spdf$index,spdf$phour)
puHrCount <- as.data.frame.matrix(puHrCount) %>% rownames_to_column("index")
puHrCount$index <- as.numeric(as.character(puHrCount$index))
hex@data <- left_join(hex@data,puHrCount)
hex@data[is.na(hex@data)] <- 0
colorFunction <- colorNumeric("BuPu",domain=c(0,max(hex@data[,2:25])))

for (i in 1:24) {
    colName <- paste0("col",i)
    hex@data[colName] <- colorFunction(hex@data[,as.character(i)])
}

hex@data$color <- colorFunction(hex@data$freq)

bkUTM <- CRS("+proj=utm +zone=18N +datum=WGS84 +units=km +no_defs") %>%
  spTransform(bk, .)
bkNoClip <- make_grid(bkUTM, cell_area = .4, clip = FALSE)
bkHex <- SpatialPolygonsDataFrame(bkNoClip,data=data.frame("index"=1:559))
bkHex <- spTransform(bkHex,CRS("+init=epsg:4326"))
bkHex <- spTransform(bkHex,proj4string(spdf))
bkHex$index <- 1:nrow(bkHex)
overDF <- over(spdf,bkHex)
spdf$index <- overDF$index
puHrCount <- table(spdf$index,spdf$coords.pHour)
puHrCount <- as.data.frame.matrix(puHrCount) %>% rownames_to_column("index")
puHrCount$index <- as.numeric(as.character(puHrCount$index))
bkHex@data <- left_join(bkHex@data,puHrCount)
bkHex@data[is.na(bkHex@data)] <- 0
bkCentHex <- centroid(bkHex)
colnames(bkCentHex) <- c("Lat","Lon")
bkCentHex <- cbind(bkCentHex,bkHex@data[,-1])
names(bkCentHex)[-c(1:2)] <- paste0("num",1:24)

mylist <- list()
mylist_ <- list()
for(i in 1:nrow(hex@data)) {
    for(j in 1:24) {
        name  <- paste0("col",j)
        mylist[[name]] <- hex@data[i,name]
 }
mylist_[[i]] <- mylist
}

mylist <- list()
mylist_ <- list()
for(i in 1:7) {
    for(j in 1:24) {
        name  <- paste0("col",j)
        mylist[[name]] <- hex@data[i,name]
 }
mylist_[[i]] <- mylist
}

dayHr <- as.data.frame(table(spdf$index,spdf$wday,spdf$phour))
names(dayHr) <- c("Index","Day","Hour","Value")

freqdf <- function(i) {
    if (i %in% spdf$index) {
        spdf@data$phour <- as.factor(spdf@data$phour)
        df <- spdf@data[which(spdf$index==i),]
        df$phour <- as.factor(df$phour)
        tbl <- as.data.frame(table(df$wday,df$phour))
    } else {
        tbl <- data.frame("day"=rep(levels(spdf$wday),24),
                          "hour"=unlist(lapply(1:24,function(x) rep(x,7))),
                          "value"=0)
    }
    names(tbl) <- c("day","hour","value")
    tbl$hour <- as.numeric(as.character(tbl$hour))
    ##print(str(tbl))
    return(toJSON(tbl))
}


rm(json)

for (i in head(hex@data$index)) {
    json <- paste0('{"index":', i, ',"children":', freqdf(i), '}')
    write(json,paste0("index",i,".json"))
}

json <- paste0('{"data":',json,"}")
write(json,"test.json")
