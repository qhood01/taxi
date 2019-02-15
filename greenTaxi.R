library(jsonlite)
library(leaflet)
library(sp)
library(tidyverse)
library(rgdal)
library(hexbin)
library(data.table)
library(raster)
library(rgeos)

yellowTaxis <- fread("~/Downloads/yellow_tripdata_2016-06.csv")
greenTaxis <- fread("~/Downloads/green_tripdata_2016-06.csv")
names(greenTaxis)[c(6,7,2,3)] <- c("pickup_longitude","pickup_latitude","tpep_pickup_datetime","tpep_dropoff_datetime")

taxisShort <- greenTaxis[,c("pickup_longitude","pickup_latitude","tpep_pickup_datetime","tpep_dropoff_datetime")]

pHours <- unlist(lapply(taxisShort$tpep_pickup_datetime,function(x) as.numeric(substr(x,12,13))))
pHoursAM <- ifelse(grepl("PM",taxisShort$tpep_pickup_datetime)&pHours<12,pHours+12,pHours)
pHoursAM[which(grepl("AM",taxisShort$tpep_pickup_datetime)&pHours==12)] <- 0
taxisShort$pHour <- pHoursAM

dHours <- unlist(lapply(taxisShort$tpep_dropoff_datetime,function(x) as.numeric(substr(x,12,13))))
dHoursAM <- ifelse(grepl("PM",taxisShort$tpep_dropoff_datetime)&dHours<12,dHours+12,dHours)
dHoursAM[which(grepl("AM",taxisShort$tpep_dropoff_datetime)&dHours==12)] <- 0
taxisShort$dHour <- dHoursAM

taxisShort$pickupDate <- as.Date(substr(taxisShort$tpep_pickup_datetime,1,10),format="%Y-%m-%d")
taxisShort$wday <- weekdays(taxisShort$pickupDate)

boros <- readOGR("~/Downloads/nybb_18d/","nybb")
boros <- boros[boros$BoroName != "Staten Island",]
boros <- spTransform(boros,CRS("+init=epsg:4326"))

make_grid <- function(x, cell_diameter, cell_area, clip = TRUE) {
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

coords <- taxisShort[!is.na(taxisShort$pickup_longitude)&
                     !is.na(taxisShort$pickup_latitude),
                     c("pickup_longitude","pickup_latitude","pHour","wday")]
coordinates(coords) <- ~pickup_longitude+pickup_latitude


spdf <- SpatialPointsDataFrame(coords=coords, data=data.frame("phour"=coords$pHour,"wday"=coords$wday),proj4string=CRS("+init=epsg:4326"))
proj4string(spdf) <- CRS("+init=epsg:4326")

borosUTM <- CRS("+proj=utm +zone=18N +datum=WGS84 +units=km +no_defs") %>%
  spTransform(boros, .)
hexClip <- make_grid(borosUTM, cell_area = .4, clip = TRUE)
hex <- SpatialPolygonsDataFrame(hexClip,data=data.frame("index"=1:2012))
hex <- spTransform(hex,proj4string(spdf))
hex$index <- 1:nrow(hex)
overDF <- over(spdf,hex)
spdf$index <- overDF$index
puHrCount <- table(spdf$index,spdf$phour)
puHrCount <- as.data.frame.matrix(puHrCount) %>% rownames_to_column("index")
puHrCount$index <- as.numeric(as.character(puHrCount$index))
hex@data <- left_join(hex@data,puHrCount)
hex@data[is.na(hex@data)] <- 0
colorFunction <- colorNumeric("YlGnBu",domain=c(0,max(hex@data[,2:25])))

for (i in 0:23) {
    colName <- paste0("col",i)
    hex@data[colName] <- colorFunction(hex@data[,as.character(i)])
}

hex@data$color <- rep(c('#7fc97f','#beaed4','#fdc086','#ffff99'),2012/4)
names(hex@data)[2:25] <- paste0("num",0:23)
#writeOGR(hex, "hexGrid.GeoJSON", layer="hex", driver="GeoJSON",overwrite_layer=TRUE)
writeOGR(hex, "greenHexGrid.GeoJSON", layer="hex", driver="GeoJSON",overwrite_layer=TRUE)

uniqueDates <- unique(taxisShort[,c("wday","pickupDate")])
uniqueDates$wday <- factor(uniqueDates$wday,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
uniqueDates$wday <- as.factor(as.numeric(uniqueDates$wday)-1)
uniqueDates$phour <- as.factor(uniqueDates$phour)
daysFreq <- as.data.frame(table(uniqueDates$wday))
names(daysFreq) <- c("day","freq")

spdf$wday <- factor(spdf$wday,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
spdf$wday <- as.factor(as.numeric(spdf$wday)-1)
spdf$phour <- as.factor(spdf$phour)

freqdf <- function(i) {
    if (i %in% spdf$index) {
        df <- spdf@data[which(spdf$index==i),]
        tbl <- as.data.frame(table(df$wday,df$phour))
        names(tbl) <- c("day","hour","value")
        tbl <- left_join(tbl,daysFreq)
        tbl$avg <- tbl$value/tbl$freq
        tbl$hour <- as.numeric(as.character(tbl$hour))
        tbl <- tbl[,c("day","hour","avg")]
    } else {
        tbl <- data.frame("day"=rep(0:6,24),
                          "hour"=unlist(lapply(0:23,function(x) rep(x,7))),
                          "avg"=0)
    }
    return(jsonlite::toJSON(tbl,dataframe="rows"))
}

rm(json)
for (i in hex@data$index) {
    if(exists("json")) {
        json <- paste0('{"index":', i, ',"children":', freqdf(i), '},',json)
    } else {
        json <- paste0('{"index":', i, ',"children":', freqdf(i), '}')
    }
}
json <- paste0('{"data": [',json,"]}")
#write(json,"data.json")
write(json,"greenData.json")
