library(lubridate)
library(tidyverse)
library(reshape)
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

### 

source(here("get_data.R"))

# create dataframe where treatment = licenses in past 2 months


Nov_10<- reshaped_all %>% select(GEOID, "Nov 2010.nrides",  "Oct 2010.nbl", "Sep 2010.nbl") %>% rename(nrides = "Nov 2010.nrides") %>%
  add_column(month = "Nov", year = 2010) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Dec_10<- reshaped_all %>% select(GEOID, "Dec 2010.nrides",  "Nov 2010.nbl", "Oct 2010.nbl") %>% rename(nrides = "Dec 2010.nrides") %>%
  add_column(month = "Dec", year = 2010) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Jan_11<- reshaped_all %>% select(GEOID, "Jan 2011.nrides",  "Dec 2010.nbl", "Nov 2010.nbl") %>% rename(nrides = "Jan 2011.nrides") %>%
  add_column(month = "Jan", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Feb_11<- reshaped_all %>% select(GEOID, "Feb 2011.nrides",  "Jan 2011.nbl", "Dec 2010.nbl") %>% rename(nrides = "Feb 2011.nrides") %>%
  add_column(month = "Feb", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Mar_11<- reshaped_all %>% select(GEOID, "Mar 2011.nrides",  "Feb 2011.nbl", "Jan 2011.nbl") %>% rename(nrides = "Mar 2011.nrides") %>%
  add_column(month = "Mar", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Apr_11<- reshaped_all %>% select(GEOID, "Apr 2011.nrides",  "Mar 2011.nbl", "Feb 2011.nbl") %>% rename(nrides = "Apr 2011.nrides") %>%
  add_column(month = "Apr", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
May_11<- reshaped_all %>% select(GEOID, "May 2011.nrides",  "Apr 2011.nbl", "Mar 2011.nbl") %>% rename(nrides = "May 2011.nrides") %>%
  add_column(month = "May", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Jun_11<- reshaped_all %>% select(GEOID, "Jun 2011.nrides",  "May 2011.nbl", "Apr 2011.nbl") %>% rename(nrides = "Jun 2011.nrides") %>%
  add_column(month = "Jun", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Jul_11<- reshaped_all %>% select(GEOID, "Jul 2011.nrides",  "Jun 2011.nbl", "May 2011.nbl") %>% rename(nrides = "Jul 2011.nrides") %>%
  add_column(month = "Jul", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Aug_11<- reshaped_all %>% select(GEOID, "Aug 2011.nrides",  "Jul 2011.nbl", "Jun 2011.nbl") %>% rename(nrides = "Aug 2011.nrides") %>%
  add_column(month = "Aug", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Sep_11<- reshaped_all %>% select(GEOID, "Sep 2011.nrides",  "Aug 2011.nbl", "Jul 2011.nbl") %>% rename(nrides = "Sep 2011.nrides") %>%
  add_column(month = "Sep", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Oct_11 <- reshaped_all %>% select(GEOID, "Oct 2011.nrides",  "Sep 2011.nbl", "Aug 2011.nbl") %>% rename(nrides = "Oct 2011.nrides") %>%
  add_column(month = "Oct", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Nov_11<- reshaped_all %>% select(GEOID, "Nov 2011.nrides",  "Oct 2011.nbl", "Sep 2011.nbl") %>% rename(nrides = "Nov 2011.nrides") %>%
  add_column(month = "Nov", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)
Dec_11<- reshaped_all %>% select(GEOID, "Dec 2011.nrides",  "Nov 2011.nbl", "Oct 2011.nbl") %>% rename(nrides = "Dec 2011.nrides") %>%
  add_column(month = "Dec", year = 2011) %>% mutate(bl_two = rowSums(.[3:4])) %>% select(GEOID, month, year, bl_two, nrides)

data_analysis <- rbind (Nov_10, Dec_10, Jan_11, Feb_11, Mar_11, Apr_11, May_11, Jun_11, Jul_11, Aug_11, Sep_11, Oct_11, Nov_11, Dec_11)
data_analysis <- data_analysis %>% mutate(year_dummies = factor(year), month_dummies = factor(month), geo_dummies = factor(GEOID))

data_analysis_summer <- rbind(May_11, Jun_11, Jul_11, Aug_11)
data_analysis_summer <- data_analysis_summer %>% mutate(year_dummies = factor(year), month_dummies = factor(month), geo_dummies = factor(GEOID))

lm <- lm(nrides ~ bl_two + year_dummies + month_dummies + geo_dummies, data = data_analysis)
lm_summer <- lm(nrides ~ bl_two + month_dummies + geo_dummies, data = data_analysis_summer)
lm_no_geo <- lm(nrides ~ bl_two + year_dummies + month_dummies, data = data_analysis)

summary(lm)
summary(lm_summer)
summary(lm_no_geo)

# Plots

# Rides
gather_rides <- gather(reshape_nrides, key = "Month_Year", value = "value", -GEOID, factor_key = TRUE )

ggplot(gather_rides, aes(x = Month_Year, y = value)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Monthly Rides by Block Group", x = "Month-Year", y = "Incoming Bike Share Rides by Block Group")

ggsave("Rides.png")

gather_bl <- gather(reshape_bl, key = "Month_Year", value = "value", -GEOID, factor_key = TRUE )
ggplot(gather_bl, aes(x = Month_Year, y = value)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Business Licenses by Block Group", x= "Month-Year", y = "Business Licenses by Block Group")
ggsave("Licenses.png")

gather_bl <- gather_bl %>% filter(value < 75000)
