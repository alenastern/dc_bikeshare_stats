library(lubridate)
library(reshape)
library(tidyverse)
library(readxl)
library(scales)
library(devtools)
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
library(rjson)
library(zoo)

# Working directory 

setwd("~/Desktop/UChi/Classes/Stats/MultipleTesting_ModernInference/project_bikeshare/dc_bikeshare_stats/src/exploration/") #Cris' directory

# ---- 1. Download block group geographies ---- #
file_bg = paste0('https://opendata.arcgis.com/datasets/c143846b7bf4438c954c5bb28e5d1a21_2.geojson')
download.file(file_bg,destfile='neighb_bg.geojson')
blockgroup_sf =  st_read('neighb_bg.geojson')
blockgroup_sf <- st_as_sf(blockgroup_sf)
blockgroup_sf <- blockgroup_sf %>% select(OBJECTID, TRACT, BLKGRP, GEOID, geometry)

# ---- 2. Read in Basic Business License Data ---- #

#as geojson
file_t = paste0('https://opendata.arcgis.com/datasets/85bf98d3915f412c8a4de706f2d13513_0.geojson')
download.file(file_t,destfile='business_licenses.geojson')
bl_raw =  st_read('business_licenses.geojson')
bl <- bl_raw
st_geometry(bl) <- NULL

# as csv
#bl <- read.csv(here("data","Basic_Business_Licenses.csv"), header = TRUE, sep= ",", fill = TRUE)

# we use start date as this is presumably the date that citizens would be able to patronizeinst the business
bl$start_date <- substr(bl$LICENSE_ISSUE_DATE, 1, 10)
bl$start_date <- as.Date(bl$start_date) 
bl$start_year <- year(bl$start_date)
bl$start_month = month(bl$start_date)

#bl_gp <- bl %>% group_by(start_year) %>% summarise(count = n())
bl_dc <- bl %>% filter(DC_ADDR_FLAG == "Y")
bl_dc <- bl_dc %>% dplyr::rename(zip = ZIP)
bl_dc$zip <- as.character(bl_dc$zip)
bl_dc$zip <-substr(bl_dc$zip, 1, 5)

# assigning coordinates to businesses where coord missing by zipcode
data(zipcode)
bl_merge <- merge(x=bl_dc,y=zipcode, on = "zip", all.x = TRUE)
bl_merge$LATITUDE[is.na(bl_merge$LATITUDE)] <- bl_merge$latitude[is.na(bl_merge$LATITUDE)]
bl_merge$LONGITUDE[is.na(bl_merge$LONGITUDE)] <- bl_merge$longitude[is.na(bl_merge$LONGITUDE)]

# ---- 3. These are all the bike stations, georeferrenced ---- #

file_location = paste0('https://opendata.arcgis.com/datasets/a1f7acf65795451d89f0a38565a975b3_5.geojson')
download.file(file_location, destfile='location_bikes.geojson')
locationbikes_sf =  st_read('location_bikes.geojson')
locationbikes_sf = subset(locationbikes_sf, select = c(LATITUDE, LONGITUDE, TERMINAL_NUMBER, ADDRESS))  
locationbikes_sf <- locationbikes_sf %>% rename("station_id" = "TERMINAL_NUMBER")

# ---- 4. Read in bike rides information  ---- #
# 4.1 Note: running Rina's code 
source(("getdata_bikeshare.R"))
#source(here("getdata_bikeshare.R")) 

# 4.2 Collapse at the month - year level
data$date = as.Date(data$Start.date, "%Y-%m-%d %H:%M:%S")
data$start_day = day(data$date)
data$start_month = month(data$date)
data$start_year = year(data$date)
data$month_yr <- format(as.Date(data$date),"%Y-%m")
data_collapsed <- data %>% group_by(Start.station.number, End.station.number, start_month, start_year, month_yr) %>% summarise(n_rides = n(), avg_duration = mean(Duration))

# 4.3 Used stations
stations_fr <- data.frame(stations) 
stations_fr <- stations_fr %>% rename("station_id" = "X1") %>% rename("address" = "X2")
stations_latlon = left_join(stations_fr, locationbikes_sf, by = "station_id")
stations_latlon <- st_as_sf(stations_latlon)

# ---- 5. Join Datasets  ---- #

# 4.1 Buisness Licenses with bloc group areas: 
bl_merge_coord <- bl_merge %>% filter(!is.na(LATITUDE))
bl_sf = st_as_sf(bl_merge_coord, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")

bl_bg <- st_join(bl_sf, blockgroup_sf, left = TRUE, join = st_within)
bl_bg_grouped <- bl_bg %>% group_by(GEOID, start_month, start_year) %>% summarise(n_bl = n())

# 4.2 Stations with bike trips:
stations_latlon$station_id = as.numeric(stations_latlon$station_id)
# For now, only joining on the "end-station"
biketrips_collapsed <- left_join(data_collapsed, stations_latlon, by = c("End.station.number" = "station_id"), suffix = c(".start", ".end"))
biketrips_collapsed <- st_as_sf(biketrips_collapsed)

# 4.3 Bike trips with block group levels. 
biketrips_bg <- st_join(biketrips_collapsed, blockgroup_sf, left = TRUE, join = st_within)
rm(biketrips_collapsed)

# 4.4 Bike info with Business licenses info; each station is connected to the number of business licences that were provided in the 
# corresponding geography (within the same bloc group)

biketrips_bg <- as.data.frame(biketrips_bg)
bl_bg_grouped <- as.data.frame(bl_bg_grouped)

total_data <- left_join(biketrips_bg, bl_bg_grouped,  by = c("GEOID", "start_month", "start_year"))
total_data$date <- as.yearmon(paste(total_data$start_year, total_data$start_month), "%Y %m")
total_data_panel <- subset(total_data, select = c(start_year, start_month, date, n_rides, n_bl, GEOID))
total_data_panel <- total_data_h2  %>% group_by(date, GEOID) %>% summarise(n_rides_tot = sum(n_rides), n_bl_tot = sum(n_bl))
# we have 1 row in bl_bg_grouped that is NA and ~7600 rows in biketrips_bg that are null, seems a couple of stationsd didn't merge

# RESHAPING 

# We're looking at how many people ride to a station, so keep only End_station as identifier, and group at this level

total_data_temp <- subset(total_data, select = c(date, n_rides, n_bl, GEOID))
total_data_temp_rides <- total_data_temp %>% group_by(date, GEOID) %>% summarise(n_rides_tot = sum(n_rides) )
total_data_temp_bl <- total_data_temp %>% group_by(date, GEOID) %>% summarise(n_bl_tot = sum(n_bl))

reshape_nrides <- spread(total_data_temp_rides, key = date, value = n_rides_tot, fill = 0)
reshape_bl <- spread(total_data_temp_bl, key = date, value = n_bl_tot, fill = 0)
reshaped_all<- left_join(reshape_nrides, reshape_bl, by = c("GEOID"), suffix = c(".nrides", ".nbl"))

#left_join()
#reshaped_all has 86 rows when we keep GEOID. This matches the number of unique GEOIDs in biketrips_bg

# Clean up environment: 
rm(biketrips_bg)
rm(bikenum)
rm(duration)
rm(member)
rm(station_end)
rm(station_start)
rm(blockgroup_sf)
rm(data_collapsed)
rm(bl)
rm(bl_bg)
rm(bl_bg_grouped)
rm(bl_dc)
rm(bl_gp)
rm(bl_merge)
rm(bl_merge_coord)
rm(bl_raw)
rm(bl_sf)
rm(stations_fr)
rm(stations_latlon)
rm(zipcode)
rm(file_location)
rm(file_t)
rm(file_bg)

# 4.3 Bike trips & stations with Business Licenses 


# to think about
# how to incorporate License Status
# how to incorporate license category



