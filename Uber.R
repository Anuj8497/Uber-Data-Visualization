# importing libraries
library(ggplot2)
library(ggthemes)
# since the dataset includes various time frames hence might be useful while visualizing data across various time frmaes.
library(lubridate)
install.packages("lubridate")
library(dplyr)
library(tidyr)
library(DT)
library(scales)
# for theme_map install following
install.packages("ggthemes")
library("ggthemes")
install.packages("maps")
library("maps")

# creating a collection of colors, to be used in other plots.
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# importing data
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

# combining all the data
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

# For formating the Date.Time variable we can use POSIXct or POSIXlt format.
# POSIXct and POSIXlt. ”ct” stand for calendar time, it stores the 
# number of seconds since the origin. ”lt”, or local time, keeps the date as a list of time 
# attributes (such as”hour” and ”mon”).

# we will be using POSIXct format.
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

# creating time variable
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

# now to convert numeric dates into POSIXct objects we use following line of code.
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)



# extracting day, month, year, and day of week from the Date.time.
# day() extracts day. label by default is set to FALSE. This means we get 1,2,3... as values
data_2014$day <- as.factor(day(data_2014$Date.Time))
# label = TRUE assigns a character corresponding to the number. For example 1 is assigned January.
data_2014$month <- as.factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- as.factor(year(data_2014$Date.Time))
data_2014$dayofweek <- as.factor(wday(data_2014$Date.Time, label = TRUE))


# extracting hour,min, sec
data_2014$hour <- as.factor(hour(hms(data_2014$Time)))
data_2014$minute <- as.factor(minute(hms(data_2014$Time)))
data_2014$second <- as.factor(second(hms(data_2014$Time)))


# now finding the number of trips by hours.

hour_data <- data_2014 %>%group_by(hour) %>%summarize(frequency = n())
help("ggplot")
ggplot(data = hour_data, aes(hour, Total))+ geom_bar(stat = "identity",fill = "steelblue", color = "red")+ggtitle("no of trips by hour")+scale_y_continuous(labels = comma) 

# no finding the number of trips by month and hours
new_data <- data_2014 %>%group_by(month,hour) %>%summarise(frequency = n())

ggplot(data=new_data, aes(hour, frequency, fill = month))+geom_bar(stat = "identity")+ggtitle("no of trips by hour and month")+scale_y_continuous(labels = comma)   


# no of trips in every day of month
new_data1 <- data_2014 %>%group_by(month,day) %>%summarise(frequency =n())
ggplot(data = new_data1 , aes(day, frequency))+geom_bar(stat = "identity")+ggtitle("no of trips every day")+scale_y_continuous(labels = comma)
ggplot(data = new_data1 , aes(day, frequency, fill = month)) + geom_bar( stat = "identity") +ggtitle("Trips on each day of different Months") +scale_y_continuous(labels = comma) +scale_fill_manual(values = colors)


#no of trips taking place in different months during a year. In group_by year is redundant becuase of data of only 2014 available.
new_data2 <- data_2014 %>%group_by(month,year) %>% summarize(frequency = n())
ggplot(data = new_data2 , aes(month, frequency , fill = month)) + geom_bar(stat = "identity") + ggtitle("No of trips in each month")+scale_y_continuous(labels = comma) +scale_fill_manual(values = colors) 

# no of trips by bases
new_data3 <- data_2014 %>% group_by(Base) %>% summarise(frequency = n())
ggplot(data = new_data3 , aes(Base, frequency)) + geom_bar(stat = "identity")+ggtitle("no of trips by bases")+scale_y_continuous(labels = comma)+scale_fill_manual(values = colors)

# no of trips by bases in different months, position ="dodge" since we are creating side by side plots.
ggplot(data_2014, aes(Base, fill = month)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma) +ggtitle("Trips by Bases and Month") +scale_fill_manual(values = colors)

# no of trips by bases and days of week
ggplot(data_2014, aes(Base, fill = dayofweek)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma) +ggtitle("Trips by Bases and Week") +scale_fill_manual(values = colors)

# creating heat map visualizations

# heat map visualizations by month and day
new_data5 <- data_2014 %>% group_by(month,day) %>% summarise(frequency = n())
ggplot(new_data5, aes(month,day,fill = frequency))+geom_tile(color = "white")+ggtitle("heat map for month and day")


# heat map visualization by month and day of the week
new_data6 <- data_2014 %>% group_by(month, dayofweek) %>% summarise(frequency = n())
ggplot(new_data6, aes(month, dayofweek,fill= frequency))+geom_tile(color = "white")+ggtitle("heat map for month and day of week")

#heat map visualization by bases and day of the week
new_data7 <- data_2014 %>% group_by(Base, dayofweek) %>% summarise(frequency = n())
ggplot(new_data7, aes(Base, dayofweek,fill= frequency))+geom_tile(color = "white")+ggtitle("heat map for bases and day of week")

#creating map visualizations 

# for the visualizations we will be needing the min max co-ordinates for both latitude and longitudes


min_lat<- min(data_2014$Lat)
max_lat <- max(data_2014$Lat)

min_lon <- min(data_2014$Lon)
max_lon <- max(data_2014$Lon)


ggplot(data_2014, aes(x=Lon, y=Lat)) + geom_point(size=1, color = "blue") + scale_x_continuous(limits=c(min_lon, max_lon)) + scale_y_continuous(limits=c(min_lat, max_lat)) + ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +  geom_point(size=1) + scale_x_continuous(limits=c(min_long, max_long)) + scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map() + ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")
