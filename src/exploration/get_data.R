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
library(rjson)


### Read in Basic Business License Data ###


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

bl_bg_grouped <- bl_bg %>% group_by(GEOID, start_month, start_year) %>% summarise(n_bl = n())


# to think about
# how to incorporate License Status
# how to incorporate license category

  