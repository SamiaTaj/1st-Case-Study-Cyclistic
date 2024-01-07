install.packages("tidyverse")
library("tidyverse")
library("lubridate")
library("dplyr")
library("tidyr")
library("ggplot2")
library("readr")
library("readxl")
library("janitor")

Jan_2022 <- (X202201_divvy_tripdata)
Feb_2022 <- (X202202_divvy_tripdata)
Mar_2022 <- (X202203_divvy_tripdata)
Apr_2022 <- (X202204_divvy_tripdata)
May_2022 <- (X202205_divvy_tripdata)
Jun_2022 <- (X202206_divvy_tripdata)
Jul_2022 <- (X202207_divvy_tripdata)
Aug_2022 <- (X202208_divvy_tripdata)
Sep_2022 <- (X202209_divvy_tripdata)
Oct_2022 <- (X202210_divvy_tripdata)
Nov_2022 <- (X202211_divvy_tripdata)
Dec_2022 <- (X202212_divvy_tripdata)

data_2022_merged <- rbind(Jan_2022, Feb_2022, Mar_2022, Apr_2022, May_2022, Jun_2022, Jul_2022, Aug_2022, Sep_2022, Oct_2022, Nov_2022, Dec_2022)
View(data_2022_merged)

data_2022_merged <- clean_names(data_2022_merged)

data_2022_merged$weekday <- wday(data_2022_merged$started_at, label = TRUE, week_start = 1)

data_2022_merged$month <- format(as.Date(data_2022_merged$started_at), '%m', label = FALSE, abbr = FALSE)

data_2022_merged$trip_length <- difftime(data_2022_merged$ended_at, data_2022_merged$started_at, units = "auto")

data_2022_merged$started_hour <- format(as.POSIXct(data_2022_merged$started_at), '%H')

write.csv(data_2022_merged, file = "C:\\Users\\User\\OneDrive\\Desktop\\Case_Study_1\\data_2022_merged.csv")

#descriptive analysis

mean(data_2022_merged$trip_length)
max(data_2022_merged$trip_length)
min(data_2022_merged$trip_length)

aggregate(data_2022_merged$trip_length~data_2022_merged$member_casual, FUN = mean)

aggregate(data_2022_merged$trip_length~data_2022_merged$member_casual, FUN = max)

aggregate(data_2022_merged$trip_length~data_2022_merged$member_casual, FUN = min)

aggregate(data_2022_merged$trip_length ~ data_2022_merged$member_casual + data_2022_merged$weekday, FUN = mean)

summarise(data_2022_merged, number_of_rides = n())
#visualizing

data_2022_merged %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(trip_length)) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual))+
           geom_bar(position = "dodge")

#Visual_1
options(scipen = 999)
ggplot(data = data_2022_merged) +
  geom_bar(mapping = aes(x = month, fill = member_casual), position = 'dodge') +
  labs(title = 'Number of rides with respect to the months of 2022', x = 'Month', y = 'Number of rides')

ggplot(data = data_2022_merged) + geom_bar(mapping = aes(x = started_hour, fill = member_casual))+facet_wrap(~ weekday)+ labs(title = 'Use of bike on hour basis', x = 'started hour', y = 'Rides', fill = 'membercasual')
