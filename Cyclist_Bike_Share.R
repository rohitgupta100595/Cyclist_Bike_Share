# Installing required packages
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
install.packages("lubridate",repos = "http://cran.us.r-project.org")
install.packages("ggplot2",repos = "http://cran.us.r-project.org")
install.packages("ggthemes",repos = "http://cran.us.r-project.org")

# Loading the packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)

# Importing and combining datasets
df1 <- read.csv('202101-divvy-tripdata.csv')
df2 <- read.csv('202102-divvy-tripdata.csv')
df3 <- read.csv('202103-divvy-tripdata.csv')
df4 <- read.csv('202104-divvy-tripdata.csv')
df5 <- read.csv('202105-divvy-tripdata.csv')
df6 <- read.csv('202106-divvy-tripdata.csv')
df7 <- read.csv('202107-divvy-tripdata.csv')
df8 <- read.csv('202108-divvy-tripdata.csv')
df9 <- read.csv('202109-divvy-tripdata.csv')
df10 <- read.csv('202110-divvy-tripdata.csv')
df11 <- read.csv('202111-divvy-tripdata.csv')
df12 <- read.csv('202112-divvy-tripdata.csv')

bike_ride <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Cleaning the dataset
bike_ride[bike_ride == "" | bike_ride == " "] <- NA
bike_ride <- drop_na(bike_ride)

# Formating the dataset
bike_ride$rideable_type <- as.factor(bike_ride$rideable_type)
bike_ride$member_casual <- as.factor(bike_ride$member_casual)
bike_ride$started_at <- as_datetime(bike_ride$started_at)
bike_ride$ended_at <- as_datetime(bike_ride$ended_at)
bike_ride$start_month <- months.Date(bike_ride$started_at)
bike_ride$ride_length <- difftime(bike_ride$ended_at,bike_ride$started_at,units = c("mins"))

# Analyzin the dataset
bike_ride <- select(bike_ride, ride_id, rideable_type, started_at, ended_at, member_casual, start_month, ride_length)
bike_ride$ride_length <- as.numeric(bike_ride$ride_length)
bike_ride <- subset(bike_ride, ride_length > 0 & ride_length < 160)
bike_ride$ride_length <- round(bike_ride$ride_length,2)
bike_ride$started_at <- as.character(bike_ride$started_at)
bike_ride$start_date <- as.Date(bike_ride$started_at)
bike_ride <- arrange(bike_ride, start_date, decreasing = FALSE)
bike_ride_1 <- bike_ride %>% 
  group_by(member_casual,start_date) %>% 
  summarise(number_of_rides=n(),Average_ride_Length=mean(ride_length))

# Visualizing data
pl <- ggplot(data=bike_ride_1) + 
  geom_point(mapping = aes(x = start_date, y = number_of_rides),alpha = 0.3) + 
  facet_wrap(~member_casual) + 
  geom_smooth(mapping = aes(x = start_date, y = number_of_rides)) + theme_economist() +
  ggtitle("Monthly data of bike rides for the year 2021",subtitle = "Data viz for casual & Member(Paid) Users") +
  labs(x = "Time Period(Months)",y = "Number of rides")
print(pl)

pl2 <- ggplot(data=bike_ride_1) + 
  geom_line(mapping = aes(x = start_date, y = Average_ride_Length,color = member_casual),size = 1) + 
  theme_economist() + 
  ggtitle("Average duration of bike rides for the year 2021",subtitle = "Data Viz for casual & Member(Paid) Users") +
  labs(x = "Time Period(Months)",y = "Average Ride Duration (Mins)")
print(pl2)