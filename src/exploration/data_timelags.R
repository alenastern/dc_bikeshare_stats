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
setwd("~/Desktop/UChi/Classes/Stats/MultipleTesting_ModernInference/project_bikeshare/dc_bikeshare_stats/") #Cris' directory

### 
df.final.timelag  <- df.final
df.final.timelag  <- df.final.timelag %>% mutate(date_m = as.Date(date))
df.final.timelag  <- df.final.timelag %>% mutate(date_1bef = date_m %m-% months(1))
df.final.timelag  <- df.final.timelag %>% mutate(date_2bef = date_m %m-% months(2))
df.final.timelag  <- df.final.timelag %>% mutate(date_3bef = date_m %m-% months(3))
df.final.timelag  <- df.final.timelag %>% mutate(date_4bef = date_m %m-% months(4))
df.final.timelag  <- df.final.timelag %>% mutate(date_5bef = date_m %m-% months(5))
df.final.timelag  <- df.final.timelag %>% mutate(date_6bef = date_m %m-% months(6))
df.final.timelag  <- df.final.timelag %>% mutate(date_7bef = date_m %m-% months(7))
df.final.timelag  <- df.final.timelag %>% mutate(date_8bef = date_m %m-% months(8))
df.final.timelag  <- df.final.timelag %>% mutate(date_9bef = date_m %m-% months(9))
df.final.timelag  <- df.final.timelag %>% mutate(date_10bef = date_m %m-% months(10))
df.final.timelag  <- df.final.timelag %>% mutate(date_11bef = date_m %m-% months(11))
df.final.timelag  <- df.final.timelag %>% mutate(date_12bef = date_m %m-% months(12))
df.final.timelag  <- df.final.timelag %>% filter(!is.na(GEOID))

# 1 month lag: merge  
df.final.timelag_1 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_1bef" = "date_m"), suffix = c("", ".1bef"))
    new_cols = vector()
    for (i in colnames(df.final)[145:168]){
      new_cols <- c(new_cols, paste(i,'.1bef',sep=""))
      }
    for (i in colnames(df.final)[1:4]){
      new_cols <- c(new_cols, paste(i,'.1bef',sep=""))
      }
  # Dropping all other deleting variables that are not the ones we're interested in. 
    df.final.timelag_1 <- df.final.timelag_1[ , ! colnames(df.final.timelag_1) %in% c(new_cols) ]

  
# 2 month lag: merge  
df.final.timelag_2 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_2bef" = "date_m"), suffix = c("", ".2bef"))
  
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.2bef',sep=""))
  }
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.2bef',sep=""))
  }
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_2 <- df.final.timelag_2[ , ! colnames(df.final.timelag_2) %in% c(new_cols) ]
  
# 3 month lag: merge  
df.final.timelag_3 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_3bef" = "date_m"), suffix = c("", ".3bef"))
  
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.3bef',sep=""))
  }
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.3bef',sep=""))
  }
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_3 <- df.final.timelag_3[ , ! colnames(df.final.timelag_3) %in% c(new_cols) ]
  
# 4 month lag: merge  
df.final.timelag_4 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_4bef" = "date_m"), suffix = c("", ".4bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.4bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.4bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_4 <- df.final.timelag_4[ , ! colnames(df.final.timelag_4) %in% c(new_cols) ]
  
  
# 5 month lag: merge  
df.final.timelag_5 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_5bef" = "date_m"), suffix = c("", ".5bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.5bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.5bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_5 <- df.final.timelag_5[ , ! colnames(df.final.timelag_5) %in% c(new_cols) ]
  
  
# 6 month lag: merge  
df.final.timelag_6 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_6bef" = "date_m"), suffix = c("", ".6bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.6bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.6bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_6 <- df.final.timelag_6[ , ! colnames(df.final.timelag_6) %in% c(new_cols) ]
  
# 7 month lag: merge  
df.final.timelag_7 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_7bef" = "date_m"), suffix = c("", ".7bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.7bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.7bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_7 <- df.final.timelag_7[ , ! colnames(df.final.timelag_7) %in% c(new_cols) ]
  
# 8 month lag: merge  
df.final.timelag_8 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_8bef" = "date_m"), suffix = c("", ".8bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.8bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.8bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_8 <- df.final.timelag_8[ , ! colnames(df.final.timelag_8) %in% c(new_cols) ]
  
# 9 month lag: merge  
df.final.timelag_9 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_9bef" = "date_m"), suffix = c("", ".9bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.9bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.9bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_9 <- df.final.timelag_9[ , ! colnames(df.final.timelag_9) %in% c(new_cols) ]
  
# 10 month lag: merge  
df.final.timelag_10 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_10bef" = "date_m"), suffix = c("", ".10bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.10bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.10bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_10 <- df.final.timelag_10[ , ! colnames(df.final.timelag_10) %in% c(new_cols) ]
  
# 11 month lag: merge  
df.final.timelag_11 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_11bef" = "date_m"), suffix = c("", ".11bef"))
  
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.11bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.11bef',sep=""))}
  # Dropping all other deleting variables that are not the ones we're interested in. 
  df.final.timelag_11 <- df.final.timelag_11[ , ! colnames(df.final.timelag_11) %in% c(new_cols) ]
  
    
# 12 month lag: merge  
df.final.timelag_12 <- left_join(df.final.timelag, df.final.timelag, by = c("GEOID" = "GEOID", "date_12bef" = "date_m"), suffix = c("", ".12bef"))
 
  # Renaming variables we need to drop
  new_cols = vector()
  for (i in colnames(df.final)[145:168]){
    new_cols <- c(new_cols, paste(i,'.12bef',sep=""))}
  for (i in colnames(df.final)[1:4]){
    new_cols <- c(new_cols, paste(i,'.12bef',sep=""))}
  df.final.timelag_12 <- df.final.timelag_12[ , ! colnames(df.final.timelag_12) %in% c(new_cols) ]
  

# Dropping all other deleting variables that are not the ones we're interested in. 
df.final.timelags <- inner_join(df.final.timelag_1, df.final.timelag_2)  
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_3)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_4)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_5)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_6)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_7)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_8)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_9)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_10)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_11)
df.final.timelags <- inner_join(df.final.timelags,  df.final.timelag_12)

df.final.timelags <- df.final.timelags %>% select(-contains("date_"))


df.final.timelags  <- df.final.timelags %>% mutate(totcum_bl_6m =total_bl.1bef + total_bl.2bef +  
                                                      total_bl.3bef + total_bl.4bef +  
                                                      total_bl.5bef + total_bl.6bef, na.rm = TRUE)

df.final.timelags <- df.final.timelags %>% mutate(totcum_bl_9m =total_bl.1bef + total_bl.2bef +  
                                                     total_bl.3bef + total_bl.4bef +  
                                                     total_bl.5bef + total_bl.6bef + 
                                                     total_bl.7bef + total_bl.8bef + 
                                                     total_bl.9bef,  na.rm = TRUE)

df.final.timelags <- df.final.timelags %>% mutate(totcum_bl_12m =total_bl.1bef + total_bl.2bef +  
                                                     total_bl.3bef + total_bl.4bef +  
                                                     total_bl.5bef + total_bl.6bef + 
                                                     total_bl.7bef + total_bl.8bef + 
                                                     total_bl.9bef + total_bl.10bef + 
                                                     total_bl.11bef + total_bl.12bef,  na.rm = TRUE)

df.final.timelags  <- df.final.timelags %>% mutate(totcum_rides_6m = n_rides_tot.1bef +  n_rides_tot.2bef +  
                                                       n_rides_tot.3bef +  n_rides_tot.4bef +  
                                                       n_rides_tot.5bef +  n_rides_tot.6bef ,  na.rm = TRUE)

df.final.timelags <- df.final.timelags %>% mutate(totcum_rides_9m = n_rides_tot.1bef +  n_rides_tot.2bef +  
                                                      n_rides_tot.3bef +  n_rides_tot.4bef +  
                                                      n_rides_tot.5bef +  n_rides_tot.6bef + 
                                                      n_rides_tot.7bef +  n_rides_tot.8bef + 
                                                      n_rides_tot.9bef, na.rm = TRUE)

df.final.timelags <- df.final.timelags %>% mutate(totcum_rides_12m = n_rides_tot.1bef +  n_rides_tot.2bef +  
                                                      n_rides_tot.3bef +  n_rides_tot.4bef +  
                                                      n_rides_tot.5bef +  n_rides_tot.6bef + 
                                                      n_rides_tot.7bef +  n_rides_tot.8bef + 
                                                      n_rides_tot.9bef +  n_rides_tot.10bef + 
                                                      n_rides_tot.11bef +  n_rides_tot.12bef,  na.rm = TRUE)


write.csv(df.final.timelags, file = "df_final_timelags.csv")