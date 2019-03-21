
library(ggplot2)
library(tidyverse)
library(lubridate)

# of the overall average monthly rides across our population
# for poisson/group lasso poisson by time
# train predictive interval
# (6430/mean(s$n_rides_tot))*100
# # val predictive interval
# (8388/mean(s$n_rides_tot))*100

#save the date
s <- df.final$date

# bind the dfs
f <- cbind(df.no.dups, s)

# average by GEOID
avg.geoid <- aggregate(n_rides_tot ~ GEOID, f, mean)
colnames(avg.geoid) <- c("geoid", "average.rides")

# average rides by month
avg.month <- aggregate(n_rides_tot ~ month(s), f, mean)
colnames(avg.month) <- c("month", "average.rides")

# average rides by year
avg.year <- aggregate(n_rides_tot ~ year(s), f, mean)
colnames(avg.year) <- c("year", "average.rides")

# average rides by month, year
avg.date <- aggregate(n_rides_tot ~ s, f, mean)
colnames(avg.date) <- c("date", "average.rides")

# make plots for each
library(ggplot2)

# plot 1
p.geoid <- ggplot(avg.geoid, aes(geoid, average.rides)) +
  geom_point() + labs(title= "Average Rides by GEOID",
                      y="Average Rides", x = "GEOID")

# plot 2
p.month <- ggplot(avg.month, aes(month, average.rides)) +
  geom_point()  + labs(title= "Average Rides by Month, January to December",
                       y="Average Rides", x = "Month")

# plot 3
p.year <- ggplot(avg.year, aes(year, average.rides)) +
  geom_point() + labs(title= "Average Rides by Year, 2010 to 2015",
                      y="Average Rides", x = "Year")

# plot 4
p.date <- ggplot(avg.date, aes(date, average.rides)) +
  geom_point() + labs(title= "Average Rides by Month, Year Dates, Sept 2010 to Dec 2015",
                      y="Average Rides", x = "Month, Year Date")
