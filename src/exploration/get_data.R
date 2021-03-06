################################################################################################################################################
# Notes - change working directory to the root of our git repository
#       - change working path of variables file on ACS data file, called by this file. 

# Working directories: 
#setwd("/mnt/dm-3/alix/Documents/Multiple Testing/dc_bikeshare_stats/") #Alix's directory
#setwd("~/Desktop/UChi/Classes/Stats/MultipleTesting_ModernInference/project_bikeshare/dc_bikeshare_stats/")
setwd("/Users/alenastern/Documents/Win2019/MultiTesting/dc_bikeshare_stats/")

# Install the following libraries: 
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
################################################################################################################################################

#Run external scripts
source("src/exploration/getdata_bikeshare.R") #Based on Rina's code, this is re-factored to get data up to 2015
source("src/exploration/ACS data.R")

#--------------------------------- --------------------------------- --------------------------------- #
#-----------------------------------------------Get data-----------------------------------------------#
#--------------------------------- --------------------------------- --------------------------------- #

# ---- 1. Download block group geographies ---- #
file_bg = paste0('https://opendata.arcgis.com/datasets/c143846b7bf4438c954c5bb28e5d1a21_2.geojson')
download.file(file_bg,destfile='neighb_bg.geojson')
blockgroup_sf =  st_read('neighb_bg.geojson')
blockgroup_sf <- st_as_sf(blockgroup_sf)
blockgroup_sf <- blockgroup_sf %>% dplyr::select(OBJECTID, TRACT, BLKGRP, GEOID, geometry)

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
locationbikes_sf <- locationbikes_sf %>% dplyr::rename("station_id" = "TERMINAL_NUMBER")

# ---- 4. Read in bike rides information  ---- #

# Get geographic information about stations on the data for the rides. 
stations_fr <- data.frame(stations) 
stations_fr <- stations_fr %>% dplyr::rename("station_id" = "X1") %>% dplyr::rename("address" = "X2")
stations_latlon = left_join(stations_fr, locationbikes_sf, by = "station_id")
stations_latlon <- st_as_sf(stations_latlon)

# ---- 5. Join Datasets  ---- #

# 5.1 Buisness Licenses with bloc group areas: 
bl_merge_coord <- bl_merge %>% filter(!is.na(LATITUDE))
bl_sf = st_as_sf(bl_merge_coord, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")
bl_bg <- st_join(bl_sf, blockgroup_sf, left = TRUE, join = st_within)
bl_bg <- dplyr::rename(bl_bg, l_name = LICENSECATEGORY, l_cat = LICENSE_CATEGORY_TEXT )

# set geometry to NULL so spread collapses month/year correctly
st_geometry(bl_bg) <- NULL

# see codes here: https://dcra.dc.gov/node/514522
bl_bg_name <- bl_bg %>% group_by(GEOID, start_month, start_year,l_name) %>% summarise(nbl_name = n())
bl_bg_cat <- bl_bg %>% group_by(GEOID, start_month, start_year, l_cat) %>% summarise(nbl_cat = n())

bl_bg_name <- spread(bl_bg_name, l_name, nbl_name, fill = 0, sep = "_")
bl_bg_cat <- spread(bl_bg_cat, l_cat, nbl_cat, fill = 0, sep = "_")

bl_bg_all <- merge(bl_bg_cat, bl_bg_name, by = c("GEOID", "start_month", "start_year"))
names(bl_bg_all) <- gsub(" ", "_", names(bl_bg_all))
bl_bg_all <- bl_bg_all %>% mutate(total_bl = dplyr::select(., l_cat_Employment_Services:l_cat_Public_Health_Public_Accomm) %>% rowSums(na.rm = TRUE))

# 5.2 Geo-referrenced stations with bike trips:
stations_latlon$station_id = as.numeric(stations_latlon$station_id)
# We join only with end-station. 
biketrips_collapsed <- left_join(data.grouped, stations_latlon, by = c("End.station.number" = "station_id"), suffix = c(".start", ".end"))
biketrips_collapsed <- st_as_sf(biketrips_collapsed)

# 5.3 Geo-referrenced Bike trips at the block group level 
biketrips_bg <- st_join(biketrips_collapsed, blockgroup_sf, left = TRUE, join = st_within)
rm(biketrips_collapsed)

# 5.4 Bike info with Business licenses info; each station is connected to the number of business licences that were provided in the 
# corresponding geography (within the same bloc group)

biketrips_bg <- as.data.frame(biketrips_bg) # We don't use them as a geometry type anymore.
bl_bg_grouped <- as.data.frame(bl_bg_all) # We don't use them as a geometry type anymore.

biketrips_bg_panel <- subset(biketrips_bg, select = -c(LATITUDE, LONGITUDE, ADDRESS, OBJECTID, TRACT, BLKGRP, geometry, avg_duration))
biketrips_bg_panel <- biketrips_bg_panel %>% group_by(start_month, start_year, GEOID) %>% summarise(n_rides_tot = sum(n_rides))
total_data_panel <- left_join(biketrips_bg_panel,bl_bg_grouped,  by = c("GEOID", "start_month", "start_year"))
total_data_panel  <- total_data_panel %>% filter(!is.na(GEOID))
total_data_panel$date <- as.yearmon(paste(total_data_panel$start_year, total_data_panel$start_month), "%Y %m")

# we have 1 row in bl_bg_grouped that is NA and ~7600 rows in biketrips_bg that are null, seems a couple of stationsd didn't merge
# we believe these stations represent stations outside of metro DC (eg. in VA or MD) 

# create date and geoid dummies to ensure we have all geoids with bikes in them
df.date.dummies <- unique(total_data_panel$date)
df.geoid.dummies <- unique(total_data_panel$GEOID)
dummies <- expand.grid(date = df.date.dummies, GEOID = df.geoid.dummies)

# 5.5 Inclusion of ACS data

# Merging total data panel and acs data, keeping all geoids. 
df.merge.1 <- merge(dummies, total_data_panel, by=c("GEOID", "date"), all.x = TRUE)
df.final <- merge(df.merge.1, df.acs, by=c("GEOID"), all.x = TRUE)

# Changing all nas in the file to 0
df.final[is.na(df.final)] <- 0

# Creating thresholds for gauging critical mass. 
find.quants <- subset(df.final, total_bl > 0 & total_bl < 2*(mean(df.final$total_bl) + sd(df.final$total_bl)))

q <- as.data.frame(quantile(find.quants$total_bl))

df.final$thresh1 <- 1*(df.final$total_bl > q[1,])
df.final$thresh2 <- 1*(df.final$total_bl > q[2,])
df.final$thresh3 <- 1*(df.final$total_bl > q[3,])
df.final$thresh4 <- 1*(df.final$total_bl > q[4,])
df.final$thresh5 <- 1*(df.final$total_bl > q[5,])

#write.csv(df.final, "df_final.csv")
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
rm(df.acs)
rm(df.merge.1)
rm(dummies)
rm(find.quants)
rm(locationbikes_sf) # Use this one if we want to plot something geographically
rm(bl_bg_all)
rm(bl_bg_cat)
rm(biketrips_bg_panel)
rm(bl_bg_name)
rm(data)
rm(data.grouped)
