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
total_data_timelag <- subset(total_data, select = c(start_year, start_month, date, n_rides, n_bl, GEOID))
total_data_timelag <- total_data_timelag  %>% group_by(date, GEOID) %>% summarise(n_rides_tot = sum(n_rides), n_bl_tot = sum(n_bl))
total_data_timelag  <- total_data_timelag %>% mutate(date_m = as.Date(date))
total_data_timelag  <- total_data_timelag %>% mutate(date_1bef = date_m %m-% months(1))
total_data_timelag  <- total_data_timelag %>% mutate(date_2bef = date_m %m-% months(2))
total_data_timelag  <- total_data_timelag %>% mutate(date_3bef = date_m %m-% months(3))
total_data_timelag  <- total_data_timelag %>% mutate(date_4bef = date_m %m-% months(4))
total_data_timelag  <- total_data_timelag %>% mutate(date_5bef = date_m %m-% months(5))
total_data_timelag  <- total_data_timelag %>% mutate(date_6bef = date_m %m-% months(6))
total_data_timelag  <- total_data_timelag %>% mutate(date_7bef = date_m %m-% months(7))
total_data_timelag  <- total_data_timelag %>% mutate(date_8bef = date_m %m-% months(8))
total_data_timelag  <- total_data_timelag %>% mutate(date_9bef = date_m %m-% months(9))
total_data_timelag  <- total_data_timelag %>% mutate(date_10bef = date_m %m-% months(10))
total_data_timelag  <- total_data_timelag %>% mutate(date_11bef = date_m %m-% months(11))
total_data_timelag  <- total_data_timelag %>% mutate(date_12bef = date_m %m-% months(12))
total_data_timelag  <- total_data_timelag %>% filter(!is.na(GEOID))

# 1 month lag: merge  
total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_1bef" = "date_m"), suffix = c("", ".1bef"))

  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.1bef' = 'n_bl_tot_1bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.1bef' = 'n_rides_tot_1bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".1bef"))

  
# 2 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_2bef" = "date_m"), suffix = c("", ".2bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.2bef' = 'n_bl_tot_2bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.2bef' = 'n_rides_tot_2bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".2bef"))
  
# 3 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_3bef" = "date_m"), suffix = c("", ".3bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.3bef' = 'n_bl_tot_3bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.3bef' = 'n_rides_tot_3bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".3bef"))
  
# 4 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_4bef" = "date_m"), suffix = c("", ".4bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.4bef' = 'n_bl_tot_4bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.4bef' = 'n_rides_tot_4bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".4bef"))
  
  
# 5 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_5bef" = "date_m"), suffix = c("", ".5bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.5bef' = 'n_bl_tot_5bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.5bef' = 'n_rides_tot_5bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".5bef"))
  
  
# 6 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_6bef" = "date_m"), suffix = c("", ".6bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.6bef' = 'n_bl_tot_6bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.6bef' = 'n_rides_tot_6bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".6bef"))

# 7 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_7bef" = "date_m"), suffix = c("", ".7bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.7bef' = 'n_bl_tot_7bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.7bef' = 'n_rides_tot_7bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".7bef"))
  
# 8 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_8bef" = "date_m"), suffix = c("", ".8bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.8bef' = 'n_bl_tot_8bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.8bef' = 'n_rides_tot_8bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".8bef"))
    
# 9 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_9bef" = "date_m"), suffix = c("", ".9bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.9bef' = 'n_bl_tot_9bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.9bef' = 'n_rides_tot_9bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".9bef"))

# 10 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_10bef" = "date_m"), suffix = c("", ".10bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.10bef' = 'n_bl_tot_10bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.10bef' = 'n_rides_tot_10bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".10bef"))
  
# 11 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_11bef" = "date_m"), suffix = c("", ".11bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.11bef' = 'n_bl_tot_11bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.11bef' = 'n_rides_tot_11bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".11bef"))
  
    
# 12 month lag: merge  
  total_data_timelag <- left_join(total_data_timelag, total_data_timelag, by = c("GEOID" = "GEOID", "date_12bef" = "date_m"), suffix = c("", ".12bef"))
  
  # This rename allows to delete 
  total_data_timelag <- rename(total_data_timelag, replace = c('n_bl_tot.12bef' = 'n_bl_tot_12bef'))
  total_data_timelag <- rename(total_data_timelag, replace = c('n_rides_tot.12bef' = 'n_rides_tot_12bef'))
  # Dropping all other deleting variables that are not the ones we're interested in. 
  total_data_timelag <- total_data_timelag %>% select(-contains(".12bef"))
  
total_data_timelag <- total_data_timelag %>% select(-contains("date_"))

total_data_timelag %>% mutate( x = as.numeric('n_bl_tot') + as.numeric('n_rides_tot'))



total_data_timelag  <- total_data_timelag %>% mutate(totcum_bl_6m = n_bl_tot_1bef +  n_bl_tot_2bef +  
                                                       n_bl_tot_3bef +  n_bl_tot_4bef +  
                                                       n_bl_tot_5bef +  n_bl_tot_6bef, na.rm = TRUE)

total_data_timelag <- total_data_timelag %>% mutate(totcum_bl_9m = n_bl_tot_1bef +  n_bl_tot_2bef +  
                                                      n_bl_tot_3bef +  n_bl_tot_4bef +  
                                                      n_bl_tot_5bef +  n_bl_tot_6bef + 
                                                      n_bl_tot_7bef +  n_bl_tot_8bef + 
                                                      n_bl_tot_9bef,  na.rm = TRUE)

total_data_timelag <- total_data_timelag %>% mutate(totcum_bl_12m = n_bl_tot_1bef +  n_bl_tot_2bef +  
                                                      n_bl_tot_3bef +  n_bl_tot_4bef +  
                                                      n_bl_tot_5bef +  n_bl_tot_6bef + 
                                                      n_bl_tot_7bef +  n_bl_tot_8bef + 
                                                      n_bl_tot_9bef +  n_bl_tot_10bef + 
                                                      n_bl_tot_11bef +  n_bl_tot_12bef,  na.rm = TRUE)

total_data_timelag  <- total_data_timelag %>% mutate(totcum_rides_6m = n_rides_tot_1bef +  n_rides_tot_2bef +  
                                                       n_rides_tot_3bef +  n_rides_tot_4bef +  
                                                       n_rides_tot_5bef +  n_rides_tot_6bef ,  na.rm = TRUE)

total_data_timelag <- total_data_timelag %>% mutate(totcum_rides_9m = n_rides_tot_1bef +  n_rides_tot_2bef +  
                                                      n_rides_tot_3bef +  n_rides_tot_4bef +  
                                                      n_rides_tot_5bef +  n_rides_tot_6bef + 
                                                      n_rides_tot_7bef +  n_rides_tot_8bef + 
                                                      n_rides_tot_9bef, na.rm = TRUE)

total_data_timelag <- total_data_timelag %>% mutate(totcum_rides_12m = n_rides_tot_1bef +  n_rides_tot_2bef +  
                                                      n_rides_tot_3bef +  n_rides_tot_4bef +  
                                                      n_rides_tot_5bef +  n_rides_tot_6bef + 
                                                      n_rides_tot_7bef +  n_rides_tot_8bef + 
                                                      n_rides_tot_9bef +  n_rides_tot_10bef + 
                                                      n_rides_tot_11bef +  n_rides_tot_12bef,  na.rm = TRUE)