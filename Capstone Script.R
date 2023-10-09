#install packages
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(skimr)
library(tidyr)
library(dplyr)
library(lubridate)

#upload packages for visualization and create visualization
install.packages("ggplot2")
library(ggplot2)

#Set directory
setwd("/Users/yuspe/Documents/Interview/Project/Capstone project/Data/Rides")

# merge all files from this directory
df <- list.files(path='C:/Users/yuspe/Documents/Interview/Project/Capstone project/Data/Rides') %>% 
lapply(read_csv) %>% 
bind_rows

#show all my data
View(df)

#To see information about missing values, type of values and columns
skim_without_charts(df)

#to see all names of columns and types
glimpse(df)

#calculate new columns trip_time, day_week, month_trip, year_trip and add them to data frame
calc_df <- df %>%
  mutate(trip_time = as.numeric(difftime(ended_at,started_at)/60)) %>%
  mutate(day_week = wday(started_at)) %>%
  mutate(month_trip = month(started_at)) %>%
  mutate(year_trip = year(started_at))

glimpse(calc_df)

#delete rows with date end less or equal than date start
ind <- with(calc_df, (trip_time <= 0))
ind
clear_df <- calc_df[!ind, ]

glimpse(clear_df)

#to see new data frame
View(clear_df)
skim_without_charts(clear_df)

#create new data frame and drop columns which has n/a and we can't use them
part_df <- clear_df %>%
  select(rideable_type, member_casual, trip_time, day_week, month_trip, year_trip) %>%
  mutate(num_trip=1)

glimpse(part_df)

#group
part_df %>% group_by(year_trip, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = sum(trip_time)/sum(num_trip), min_trip = min(trip_time), max_trip = max(trip_time), total_num_trip = sum(num_trip))
part_df %>% group_by(month_trip, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = sum(trip_time)/sum(num_trip), min_trip = min(trip_time), max_trip = max(trip_time), total_num_trip = sum(num_trip))
part_df %>% group_by(day_week, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = sum(trip_time)/sum(num_trip), min_trip = min(trip_time), max_trip = max(trip_time), total_num_trip = sum(num_trip))
part_df %>% group_by(rideable_type, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = sum(trip_time)/sum(num_trip), min_trip = min(trip_time), max_trip = max(trip_time), total_num_trip = sum(num_trip))


#mean trip by day of week and by type of customer
part_df %>% filter(!is.na(trip_time)) %>% group_by(day_week, member_casual) %>% summarise(mean_trip = sum(trip_time)/sum(num_trip)) %>%
  ggplot(aes(x=day_week, y=mean_trip)) + geom_bar(stat = "identity") + facet_wrap(~member_casual)

#max trip by day of week and by type of customer
part_df %>% group_by(day_week, member_casual) %>% summarise(max_trip = max(trip_time)) %>%
  ggplot(aes(x=day_week, y=max_trip)) + geom_bar(stat = "identity") + facet_wrap(~member_casual)

#1.mean duration of trips by day of week and by type of customer
part_df %>% group_by(day_week, member_casual) %>% summarise(mean_trip = sum(trip_time)/sum(num_trip)) %>%
  ggplot(aes(x=day_week, y=mean_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
    label = c(0, 5, 10, 15, 20, 25, 30, 35)
    ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    label = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  )

#2.mean duration of trips by month and by type of customer
part_df %>% group_by(month_trip, member_casual) %>% summarise(mean_trip = sum(trip_time)/sum(num_trip)) %>%
  ggplot(aes(x=month_trip, y=mean_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, 25, 30),
    label = c(0, 5, 10, 15, 20, 25, 30)
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  )

#3.amount of trips by type of bycicle and by type of customer
part_df %>% group_by(rideable_type, member_casual) %>% summarise(new_num_trip = sum(num_trip)) %>%
  ggplot(aes(x=rideable_type, y=new_num_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000),
    label = c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000)
  )
  
#4.amount of trips by day of week and by type of customer
part_df %>% group_by(day_week, member_casual) %>% summarise(new_num_trip = sum(num_trip)) %>%
  ggplot(aes(x=day_week, y=new_num_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 250000, 500000, 750000, 1000000),
    label = c(0, 250000, 500000, 750000, 1000000)
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    label = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  )

#5.amount of trips by month and by type of customer
part_df %>% group_by(month_trip, member_casual) %>% summarise(new_num_trip = sum(num_trip)) %>%
  ggplot(aes(x=month_trip, y=new_num_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 250000, 500000, 750000, 1000000),
    label = c(0, 250000, 500000, 750000, 1000000)
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  )

#6.