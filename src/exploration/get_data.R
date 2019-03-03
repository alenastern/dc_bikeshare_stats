library(lubridate)
library(tidyverse)
library(readxl)
library(scales)
library(devtools)
library(aidtools)
library(here)
library(dplyr)
library(treemapify)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggmap)
library(zipcode)
library(geojsonR)
library(geojsonsf)
library(sf)


### Exploring new PPD database + Geocoded World Bank data ###
bl <- read.csv(here("data","Basic_Business_Licenses.csv"), header = TRUE, sep= ",", fill = TRUE)

# we use start date as this is presumably the date that citizens would be able to patronizeinst the business
bl$start_date <- substr(bl$LICENSE_START_DATE, 1, 10)
bl$start_date <- as.Date(bl$start_date) 
bl$start_year <- year(bl$start_date)
bl_gp <- bl %>% group_by(start_year) %>% summarise(count = n())
bl_dc <- bl %>% filter(DC_ADDR_FLAG == "Y")
bl_dc <- bl_dc %>% rename(zip = ZIP)
bl_dc$zip <- as.character(bl_dc$zip)
bl_dc$zip <-substr(bl_dc$zip, 1, 5)

# assigning coordinates to businesses where coord missing by zipcode
data(zipcode)
bl_merge <- merge(x=bl_dc,y=zipcode, on = "zip", all.x = TRUE)
bl_merge$LATITUDE[is.na(bl_merge$LATITUDE)] <- bl_merge$latitude[is.na(bl_merge$LATITUDE)]
bl_merge$LONGITUDE[is.na(bl_merge$LONGITUDE)] <- bl_merge$longitude[is.na(bl_merge$LONGITUDE)]

# download blockgroup file
file_bg = paste0('https://opendata.arcgis.com/datasets/c143846b7bf4438c954c5bb28e5d1a21_2.geojson')
download.file(file_bg,destfile='neighb_bg.geojson')
blockgroup_sf =  st_read('neighb_bg.geojson')
blockgroup_sf <- st_as_sf(blockgroup_sf)
blockgroup_sf <- blockgroup_sf %>% select(OBJECTID, TRACT, BLKGRP, GEOID, geometry)

bl_merge_coord <- bl_merge %>% filter(!is.na(LATITUDE))
bl_sf = st_as_sf(bl_merge_coord, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")

bl_bg <- st_join(bl_sf, blockgroup_sf, left = TRUE, join = st_within)
  
### Get Bikeshare Data ###
data = NULL

# edit range in for loop for different dates
for(i in 2010){ # the data goes up to 2017, but the files are extremely large from 2011 onwards - you can decide to just use a subset
  file = paste0('https://s3.amazonaws.com/capitalbikeshare-data/',i,'-capitalbikeshare-tripdata.zip')
  download.file(file,destfile='bikedata.zip')
  unzip('bikedata.zip')
  data = rbind(data,read.csv(paste0(i,'-capitalbikeshare-tripdata.csv')))
}

n = dim(data)[1]

starttime = as.numeric(strsplit(toString(data[,2]),split='[-:, ]')[[1]][-7*(1:n)]) # start time of ride #i
dim(starttime) = c(6,n); starttime = t(starttime) # row i = year/month/date/hour/minute/second for ride #i
duration = data[,1] # duration of the ride in seconds
station_start = data[,4] # station ID where the bike was checked out
station_end = data[,6] # station ID where the bike was returned
bikenum =  as.numeric((strsplit(toString(data[,8]),'[?wW, ]')[[1]][3*(1:n)-1])) # some are NA, the data is messy for this one
member = (data[,9]=='Member') # member (1) or nonmember (0)

stations = NULL # stations[i,1] = station ID for the i-th station, stations[i,2] = station location for the i-th station
for(i in unique(c(station_start,station_end))){
  if(any(data[,4]==i)){
    ind = min(which(data[,4]==i))
    location = toString(data[ind,5])
  }else{
    ind = min(which(data[,6]==i))
    location = toString(data[ind,7])
  }
  stations = rbind(stations,c(i,location))
}
# note that stations get added to the program over time


bs_2015_3 <- read.csv(here("data","2015Q3-capitalbikeshare-tripdata.csv"), header = TRUE, sep= ",", fill = TRUE)