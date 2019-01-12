library(tidyverse)
library(ggplot2)

#read in data
taxi_df <- read.csv("/Users/cecilialow-weiner/Desktop/Taxi Data/2017_Green_Taxi_Trip_Data.csv")

# explore data structure and names
head(taxi_df)
str(taxi_df)
head(taxi_df$PULocationID)
names(taxi_df)

# create frequency table for distance
df <- as.data.frame(table(sort(taxi_df$trip_distance)))
colnames(df) <- c("Distance", "Frequency")

# filter for trips over a mile
df$Distance <- as.numeric(df$Distance)
mile_trips <- df %>% filter(Distance >= 1)
sum(mile_trips)
