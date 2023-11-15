#loading the required libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)

#loading the data sets from October'2022 to September'2023,
#read_csv() for turning the data sets into tibble
oct_2022 <- read_csv("202210-divvy-tripdata.csv") 
nov_2022 <- read_csv("202211-divvy-tripdata.csv")
dec_2022 <- read_csv("202212-divvy-tripdata.csv")
jan_2023 <- read_csv("202301-divvy-tripdata.csv")
feb_2023 <- read_csv("202302-divvy-tripdata.csv")
mar_2023 <- read_csv("202303-divvy-tripdata.csv")
apr_2023 <- read_csv("202304-divvy-tripdata.csv")
may_2023 <- read_csv("202305-divvy-tripdata.csv")
jun_2023 <- read_csv("202306-divvy-tripdata.csv")
jul_2023 <- read_csv("202307-divvy-tripdata.csv")
aug_2023 <- read_csv("202308-divvy-tripdata.csv")
sep_2023 <- read_csv("202309-divvy-tripdata.csv")

#to compare if the data sets have similar number of variables, if it returns 0 you are good to go
compare_df_cols(oct_2022, nov_2022, dec_2022, jan_2023, feb_2023, mar_2023,
                apr_2023, may_2023, jun_2023, jul_2023, aug_2023, sep_2023,
                return = "mismatch")

#moving the data sets into one single data frame with which further analysis will be done
bike_trip_combined <- bind_rows(oct_2022, nov_2022, dec_2022, jan_2023, 
                                feb_2023, mar_2023,apr_2023, may_2023,
                                jun_2023, jul_2023, aug_2023, sep_2023)

#mutate() will add a column named ride_length calculating the difference between started_at and ended_at
#'min' is used as the "units" argument
#ride_length is then rounded to two digits after decimal and converted to numeric 
bike_trip_combined <- bike_trip_combined %>% 
  mutate(ride_length = difftime(bike_trip_combined$ended_at, 
                                bike_trip_combined$started_at, 
                                units = "min"))
bike_trip_combined$ride_length <- round(bike_trip_combined$ride_length, 2)
bike_trip_combined$ride_length <- as.numeric(bike_trip_combined$ride_length)


#to check again the structure of the existing columns
str(bike_trip_combined)


#to drop the missing values and to be on the safe side another data frame is used from the previous bike_trip_combined data
combined_bike_trip <- drop_na(bike_trip_combined)

#taking only the positive ride_length data
combined_bike_trip <- filter(combined_bike_trip, ride_length > 0)

#extracting the geolocation data from the data frame since it's not required for the analysis
locations <- combined_bike_trip %>% select(ride_id, start_station_name,
                                          start_station_id,end_station_name,
                                          end_station_id, start_lat,
                                          start_lng, end_lat, end_lng)
combined_bike_trip <- combined_bike_trip %>% select(-c(start_lat, start_lng,
                                                       end_lat, end_lng))
#creating date, month, day and weekday columns from started_at 
combined_bike_trip$date <- as.Date(combined_bike_trip$started_at)
combined_bike_trip$month <- format(as.Date(combined_bike_trip$date), "%B")
combined_bike_trip$day <- format(as.Date(combined_bike_trip$date), "%d")
combined_bike_trip$week_day <- weekdays(combined_bike_trip$date)

#Ordering by month and days of the week
combined_bike_trip$month <- ordered(combined_bike_trip$month, 
                                    levels = c('October', 'November', 'December','January',
                                               'February', 'March', 'April', 'May', 
                                               'June', 'July','August', 'September'))
combined_bike_trip$week_day <- ordered(combined_bike_trip$week_day, 
                                       levels = c("Sunday", "Monday", "Tuesday",
                                                  "Wednesday", "Thursday", "Friday",
                                                  "Saturday"))
#descriptive analysis
#summarizing number of rides by rider type using group_by function
combined_bike_trip %>% 
  group_by(member_casual) %>% 
  summarise(no_of_rides = n(),
            mean_ride_len = mean(ride_length))

#counting rideable type by rider type
combined_bike_trip %>% 
  group_by(member_casual) %>% 
  count(rideable_type)

#statistical analysis by rider type
combined_bike_trip %>% 
  group_by(member_casual) %>% 
  summarise(no_of_rides = n(),
            mean_ride_len = mean(ride_length),
            min_ride_len = min(ride_length),
            max_ride_len = max(ride_length),
            median_ride_len = median(ride_length))

#number of rides by month wise grouping
combined_bike_trip %>% 
  group_by(member_casual, month) %>% 
  summarise(no_of_rides = n(),
            mean_ride_len = mean(ride_length)) %>% 
  arrange(month)

#number of rides by weekday wise grouping
combined_bike_trip %>% 
  group_by(member_casual, week_day) %>% 
  summarise(no_of_rides = n(),
            mean_ride_len = mean(ride_length)) %>%
  arrange(week_day)

#most popular starting station among the casual riders
combined_bike_trip %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides)) %>% 
  filter(member_casual == "casual") %>% 
  select(start_station_name, no_of_rides)


#most popular starting station among the member riders
combined_bike_trip %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides)) %>% 
  filter(member_casual == "member") %>% 
  select(start_station_name, no_of_rides)

#most popular ending station among the casual riders
combined_bike_trip %>% 
  group_by(end_station_name, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides)) %>% 
  filter(member_casual == "member") %>% 
  select(end_station_name, no_of_rides)


#most popular ending station among the casual riders
combined_bike_trip %>% 
  group_by(end_station_name, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides)) %>% 
  filter(member_casual == "casual") %>% 
  select(end_station_name, no_of_rides)

#searching for top routes
combined_bike_trip1 <- (unite(combined_bike_trip, "route", start_station_name,
                              end_station_name, sep = " to "))

popular_route <- combined_bike_trip1 %>% 
  group_by(route) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides))

top_route_rider <- combined_bike_trip1 %>% 
  group_by(route, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides))

#Visualization of the previous analysis
#1. Visualization of average ride length by rider type
viz_rider_avg_len <- combined_bike_trip1 %>% 
  group_by(member_casual) %>% 
  summarise(mean_ride_len = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = mean_ride_len, fill = member_casual)) +
  geom_col() + labs(title = "Average Ride Length by Rider Type", fill = "Rider Type") + 
  xlab("Rider Type") + ylab("Mean Ride Length")

#2. Visualization of number of rides per month by rider type
viz_ride_per_month <- combined_bike_trip1 %>% 
  group_by(member_casual, month) %>% 
  summarise(no_of_rides = n()) %>% 
  ggplot(aes(x = month, y = no_of_rides, fill = member_casual)) + geom_col(position = "dodge2") +
  labs(title = "Number of Rides per Month", fill = "Rider Type") + xlab("Month") + ylab("No. of Rides") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#3. Visualization of average ride length per month by rider type
viz_avg_ride_month <- combined_bike_trip1 %>% 
  group_by(member_casual, month) %>% 
  summarise(mean_ride_len = mean(ride_length)) %>% 
  ggplot(aes(x = month, y = mean_ride_len, fill = member_casual)) + geom_col(position = "dodge2") +
  labs(title = "Average Ride Length per Month", fill = "Rider Type") + xlab("Month") + ylab("Avg. Ride Length") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#4. Visualization of number of rides per day by rider type
viz_ride_per_day <- combined_bike_trip1 %>% 
  group_by(member_casual, week_day) %>% 
  summarise(no_of_rides = n()) %>% 
  ggplot(aes(x = week_day, y = no_of_rides, fill = member_casual)) + geom_col(position = "dodge2") +
  labs(title = "Number of Rides per Day", fill = "Rider Type") + xlab("Weekdays") + ylab("No. of Rides") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#5. Visualization of average ride length per day by rider type
viz_avg_ride_day <- combined_bike_trip1 %>% 
  group_by(member_casual, week_day) %>% 
  summarise(mean_ride_len = mean(ride_length)) %>% 
  ggplot(aes(x = week_day, y = mean_ride_len, fill = member_casual)) + geom_col(position = "dodge2") +
  labs(title = "Average Ride Length per Day", fill = "Rider Type") + xlab("Weekdays") + ylab("Avg. Ride Length") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#6.Visualization of number of rides by rideable type
viz_ride_rideable_type <- combined_bike_trip1 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  ggplot(aes(x = rideable_type, y = no_of_rides, fill = member_casual)) + geom_col(position = "dodge2") +
  labs(title = "Number of Rides by Rideable Type", fill = "Rider Type") + xlab("Type of Rides") + ylab("No. of Rides") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#7. Visualization of average ride length by rideable type
viz_avg_ride_rideable_type <- combined_bike_trip1 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(mean_ride_len = mean(ride_length)) %>% 
  ggplot(aes(x = rideable_type, y = mean_ride_len, fill = member_casual)) + geom_col(position = "dodge2") +
  labs(title = "Average Ride Length by Rideable Type", fill = "Rider Type") + xlab("Type of Rides") + ylab("Avg. Ride Length") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#8. Visualization of number of rides per month segmented by rider type
viz_ride_member_segment <- combined_bike_trip1 %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarise(no_of_rides = n()) %>% 
  ggplot(aes(x = month, y = no_of_rides, fill = rideable_type)) + geom_col(position = "dodge2") + 
  facet_wrap(~member_casual) + labs(title = "Number of Rides per Month by Rideable Type",
                                    fill = "Rider Type") + 
  xlab("Month") + ylab("No. of Rides") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#9. Visualization of average ride length per month segmented by rider type
viz_avg_ride_member_segment <- combined_bike_trip1 %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarise(mean_ride_len = mean(ride_length)) %>% 
  ggplot(aes(x = month, y = mean_ride_len, fill = rideable_type)) + geom_col(position = "dodge2") + 
  facet_wrap(~member_casual) + labs(title = "Avg. Ride Length per Month by Rideable Type",
                                    fill = "Rider Type") + 
  xlab("Month") + ylab("Avg. Ride Length") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#10. Top 10 popular start stations among casual riders
viz_10_start_station_casual <- combined_bike_trip %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides)) %>% 
  filter(member_casual == "casual", no_of_rides >= 11000) %>% 
  select(start_station_name, no_of_rides) %>% 
  ggplot(aes(x = start_station_name, y = no_of_rides)) + geom_col(fill = 'navy') + coord_flip() +
  labs(title = "Top 10 popular start stations among casual riders", x = "Start Station", y = "No. of Rides")


#11. Top 10 popular end stations among casual riders
viz_10_end_station_casual <- combined_bike_trip %>% 
  group_by(end_station_name, member_casual) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(desc(no_of_rides)) %>% 
  filter(member_casual == "casual", no_of_rides >= 10900) %>% 
  select(end_station_name, no_of_rides) %>% 
  ggplot(aes(x = end_station_name, y = no_of_rides)) + geom_col(fill = "slateblue") + coord_flip() +
  labs(title = "Top 10 popular end stations among casual riders", x = "End Station", y = "No. of Rides")

#Exporting analyzed data into csv files for further visualizations
write_csv(bike_trip_combined, "Bike_Trip_Oct22_Sep23.csv")

write_csv(locations, "Locations_Bike_Trip.csv")

write_csv(popular_route, "Popular_Routes.csv")

write_csv(top_route_rider, "Top_Routes_Riderwise.csv")

write_csv(combined_bike_trip1, "Bike_Trip_with_Routes.csv")
