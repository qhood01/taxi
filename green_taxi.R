library(RSocrata)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)

green_taxi <- read.socrata("https://data.cityofnewyork.us/Transportation/2016-Green-Taxi-Trip-Data/hvrh-b6nb")

taxi_subset <- green_taxi[1:500000,]

taxi <- taxi_subset %>% select(lpep_pickup_datetime, Lpep_dropoff_datetime, Pickup_longitude,Pickup_latitude,Dropoff_longitude,Dropoff_latitude)

hex <- readOGR("~/Desktop/hexGrid.GeoJSON")
plot(hex)

str(hex)

names(taxi)

#create spatial points dataframe
x <- taxi$Pickup_longitude
y <- taxi$Pickup_latitude

coords <- cbind(x,y)
sp <- SpatialPoints(coords)

latlong = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
spdf <- SpatialPointsDataFrame(sp,taxi, proj4string = CRS(latlong))
spdf <- spTransform(spdf,latlong)


#get rid of color column and create column with unique IDs
hex@data <- select(hex@data,-color)
hex@data$ID <- seq.int(nrow(hex@data))

#use over function to overlay spatial points df(spdf) and spatial polygon df (hex)
xx <- over(spdf,hex)
 
#cbind the taxi data

taxi_hex <- cbind(taxi,xx)

# transform pickup date time to hour using substring

taxi_hex$lpep_pickup_datetime <- substr(taxi_hex$lpep_pickup_datetime,11,14)
taxi_hex$lpep_pickup_datetime <- substr(taxi_hex$lpep_pickup_datetime,1,3)

# frequency of hexagon by hour
freq.df <- as.data.frame.matrix(table(taxi_hex$ID,taxi_hex$lpep_pickup_datetime))
freq.df$ID <- rownames(freq.df)

# merge with spatial polygon
hex@data <- merge(freq.df,hex@data, by="ID", all.y=TRUE)


#create color function
max(hex@data[,-1], na.rm=TRUE)

colors <-   colorNumeric("BuPu",domain=0:1278)

new.df <- unlist(apply(hex@data[,-1], MARGIN=2, FUN=colors),recursive=TRUE)

#cbind colors to full dataframe

hex@data <- cbind(hex@data,new.df)

plot(hex)
plot(hex, col=hex[["03"]])  
  

