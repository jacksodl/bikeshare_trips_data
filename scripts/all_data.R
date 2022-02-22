install.packages("tidyverse")
install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("scales")

library(tidyverse)
library(rmarkdown)
library(ggplot2)
library(lubridate)
library(scales)

getwd()
setwd("~/Desktop/trip_data/orginial_data")

##### Load data and define variables
jan22 <- read_csv("202201-divvy-tripdata.csv")
dec21 <- read_csv("202112-divvy-tripdata.csv")
nov21 <- read_csv("202111-divvy-tripdata.csv")
oct21 <- read_csv("202110-divvy-tripdata.csv")
sep21 <- read_csv("202109-divvy-tripdata.csv")
aug21 <- read_csv("202108-divvy-tripdata.csv")
jul21 <- read_csv("202107-divvy-tripdata.csv")
jun21 <- read_csv("202106-divvy-tripdata.csv")
may21 <- read_csv("202105-divvy-tripdata.csv")
apr21 <- read_csv("202104-divvy-tripdata.csv")
mar21 <- read_csv("202103-divvy-tripdata.csv")
feb21 <- read_csv("202102-divvy-tripdata.csv")

##### View data to see what the tables look like and look for patterns if possible.
View(jan22)
View(dec21)
View(nov21)
View(oct21)
View(sep21)
View(aug21)
View(jul21)
View(jun21)
View(may21)
View(apr21)
View(mar21)
View(feb21)

##### Look at each of the columns
colnames(jan22)
colnames(dec21)
colnames(nov21)
colnames(oct21)
colnames(sep21)
colnames(aug21)
colnames(jul21)
colnames(jun21)
colnames(may21)
colnames(apr21)
colnames(mar21)
colnames(feb21)

##### If we need to rename columns
# (q4_2019 <- rename(q4_2019
#                    ,ride_id = trip_id
#                    ,rideable_type = bikeid 
#                    ,started_at = start_time  
#                    ,ended_at = end_time  
#                    ,start_station_name = from_station_name 
#                    ,start_station_id = from_station_id 
#                    ,end_station_name = to_station_name 
#                    ,end_station_id = to_station_id 
#                    ,member_casual = usertype))

##### Inspect each dataframe
str(jan22)
str(dec21)
str(nov21)
str(oct21)
str(sep21)
str(aug21)
str(jul21)
str(jun21)
str(may21)
str(apr21)
str(mar21)
str(feb21)

##### Convert started_at and ended_at in "apr21" to datetime to match other data
apr21 <- apr21 %>%
  mutate(across(c(started_at, ended_at), mdy_hm))

##### Combine each data frame
all_trips <- bind_rows(jan22, dec21, nov21, oct21, sep21, aug21, jul21, jun21,
                       may21, apr21, mar21, feb21)
View(all_trips)
   
##### Remove lat and long 
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

##### Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

##### ***OLD DATA*** There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

##### See how many things fall under each member type
table(all_trips$member_casual)

##### ***OLD DATA***  
#Reassign to the desired values (we will go with the current 2020 labels)
# all_trips <-  all_trips %>% 
#   mutate(member_casual = recode(member_casual
#                                 ,"Subscriber" = "member"
#                                 ,"Customer" = "casual"))

##### Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... 
# before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

##### Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

##### Look at the structure of the columns
str(all_trips)

##### Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


##### Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks 
# and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

##### Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

##### You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

##### Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

##### See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

##### Put days of the week in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", "Saturday"))

##### Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

##### Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  drop_na() %>% 
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

##### Require scales package
require(scales)

##### Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  drop_na() %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma)

##### Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  drop_na() %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma)

# Create a csv file that to visualize data
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, 'avg_ride_length.csv')





