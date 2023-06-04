# Set working directory
setwd("C:/Users/User/Desktop/Projects/Coursera_GoogleCapstoneCaseStudy/CyclisticBS")

# Install necessary packages
install.packages("tidyverse")
install.packages("scales")
install.packages("date")
install.packages("dplyr")
install.packages("pillar")
install.packages("ggplot2")
install.packages("pkgconfig")
install.packages("isoband")
install.packages("janitor")
install.packages('stringi')
install.packages('skimr')


# Load necessary packages
library(date)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(skimr)
library(janitor)
library(scales)
library(readr)

# Read in the data files
trips_2020_12 <- read.csv("td1.csv")
trips_2021_01 <- read.csv("td2.csv")
trips_2021_02 <- read.csv("td3.csv")
trips_2021_03 <- read.csv("td4.csv")
trips_2021_04 <- read.csv("td5.csv")
trips_2021_05 <- read.csv("td6.csv")
trips_2021_06 <- read.csv("td7.csv")
trips_2021_07 <- read.csv("td8.csv")
trips_2021_08 <- read.csv("td9.csv")
trips_2021_09 <- read.csv("td10.csv")
trips_2021_10 <- read.csv("td11.csv")
trips_2021_11 <- read.csv("td12.csv")

# Check the structure of one of the data frames
str(trips_2021_01)
# Compare column names of all data frames
compare_df_cols(trips_2021_01, trips_2021_02, trips_2021_03, trips_2021_04, trips_2021_05, trips_2021_06, trips_2021_07, trips_2021_08, trips_2021_09, trips_2021_10, trips_2021_11, trips_2020_12)

# Combine all data frames into one
Trips2021 <- rbind(trips_2021_01, trips_2021_02, trips_2021_03, trips_2021_04, trips_2021_05, trips_2021_06, trips_2021_07, trips_2021_08, trips_2021_09, trips_2021_10, trips_2021_11, trips_2020_12)

# Check the structure of the combined data frame
str(Trips2021)

# Identify and remove duplicates
duplicates <- duplicated(Trips2021$ride_id)
num_duplicates <- sum(duplicates)
print(paste("Number of duplicates removed: ", num_duplicates))

#Replace blanks or empty spaces with NA
is.na(Trips2021) <- Trips2021 == ""


#Removing all NA values from dataframe
Trips2021 <- na.omit(Trips2021)

#Check for missing values
colSums(is.na(Trips2021))

# Convert date and time columns to POSIXct format
Trips2021$started_at <- as.POSIXct(as.character(Trips2021$started_at), format = "%Y-%m-%d %H:%M", tz = "Africa/Lagos")
Trips2021$ended_at <- as.POSIXct(as.character(Trips2021$ended_at), format = "%Y-%m-%d %H:%M", tz = "Africa/Lagos")

# Create/add a new column to the dataframe and calculate ride length in minutes
Trips2021<-mutate(Trips2021,ride_length=round(difftime(ended_at,started_at,units = "mins"),0))

# Remove rows with negative ride_length
Trips2021 <- Trips2021 %>%
  filter(ride_length > 0)
    

# View the summary of ride_length variable
summary(Trips2021$ride_length)

# Add day_of_week column using the start time variable
Trips2021<- Trips2021 %>%
  mutate(day_of_week=wday(Trips2021$started_at,label=TRUE, abbr=TRUE))

# View dataframe structure
glimpse(Trips2021)

# Add date, month and year columns using the start time variable
Trips2021$date <- as.Date(Trips2021$started_at)
Trips2021$month <- format(as.Date(Trips2021$date), "%B")
Trips2021$day <- format(as.Date(Trips2021$date), "%d")
Trips2021$year <- format(as.Date(Trips2021$date), "%Y")

# View summary of the dataframe
summary(Trips2021)
# View the first 50 rows of the dataframe
head(Trips2021,50)
# View the last 20 rows of the dataframe
tail(Trips2021,20)
# View the structure of the dataframe
str(Trips2021)

# Rename the column member_casual to users
colnames(Trips2021)[colnames(Trips2021)=="member_casual"] <- "users"

# Calculate the mean ride length and print the result
mean_ride_length = round(mean(Trips2021$ride_length),0)
print(mean_ride_length)

# Calculate the maximum ride length and print the result
max_ride_length = max(Trips2021$ride_length)
print(max_ride_length)

# Calculate the minimum ride length and print the result
min_ride_length = min(Trips2021$ride_length)
print(min_ride_length)

# Calculate the median ride length and print the result
median_ride_length = median(Trips2021$ride_length)
print(median_ride_length)

# Define the Mode function for numeric variables
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate the mode of the day_of_week column
mode_day_of_week <- Mode(Trips2021$day_of_week)

# Print the mode
print(mode_day_of_week)


# Compare user type with visuals
# Calculate the mean ride length for each user type
means <- aggregate(ride_length ~ users, data = Trips2021, FUN = mean)

# Extract the values of the ride_length column as a numeric vector By using as.numeric() to convert the ride_length column to a numeric vector, you are ensuring that the data is in a format that is compatible with these types of calculations, plots, and models.
mean_lengths <- as.numeric(means$ride_length)

# Remove missing and infinite values from mean_lengths and means$users
mean_lengths <- mean_lengths[is.finite(mean_lengths)]
means$users <- means$users[is.finite(mean_lengths)]

# Create a bar plot of the mean ride length for each user type
mean_ride_length<-barplot(height = mean_lengths, names.arg = means$users, xlab = "User Type", ylab = "Mean Ride Length")
print(mean_ride_length)
# Calculate the median ride length for each user type
medians <- aggregate(ride_length ~ users, data = Trips2021, FUN = median)

# Extract the values of the ride_length column as a numeric vector
median_lengths <- as.numeric(medians$ride_length)

# Create a bar plot of the median ride length for each user type
median_ride_length<-barplot(height = median_lengths, names.arg = medians$users, xlab = "User Type", ylab = "Median Ride Length")

# Calculate the min ride length for each user type
mins <- aggregate(ride_length ~ users, data = Trips2021, FUN = min)

# Extract the values of the ride_length column as a numeric vector
min_lengths <- as.numeric(mins$ride_length)

# Create a bar plot of the min ride length for each user type
min_ride_length<-barplot(height = min_lengths, names.arg = mins$users, xlab = "User Type", ylab = "Min Ride Length")

# Calculate the max ride length for each user type
maxs <- aggregate(ride_length ~ users, data = Trips2021, FUN = max)

# Extract the values of the ride_length column as a numeric vector
max_lengths <- as.numeric(maxs$ride_length)

# Create a bar plot of the max ride length for each user type
max_ride_length<-barplot(height = max_lengths, names.arg = maxs$users, xlab = "User Type", ylab = "Max Ride Length")

#Visualize user type by the number of ride taken (ride count)
Trips2021 %>% 
  group_by(users) %>% 
  summarise(ride_count = length(ride_id)) %>% 
  arrange(users) %>% 
  ggplot(aes(x = users,y = ride_count,fill = users)) +
  geom_col(position = "dodge")+
  labs(title = "Total rides taken (ride_count) of Members and Casual riders")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

  # This code chunk creates a bar chart showing the number of rides by user type and month.
Trips2021 %>%
  group_by(month, users) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = users, values_from = count) %>%
  ggplot(aes(x = month, y = member, fill = "Member")) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_bar(stat = "identity") +
  geom_col(aes(y = casual, fill = "Casual"), stat = "identity") +
  scale_fill_manual(values = c("Member" = "blue", "Casual" = "orange")) +
  labs(title = "Trips by Users' Type and Month From Dec2020 - Nov2021",
       x = "Month",
       y = "Number of Trips",
       fill = "User Type")

       # This code chunk creates a bar chart showing the total trips by user type vs. day of the week.
Trips2021 %>%
  group_by(users, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(users, day_of_week)  %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = users)) +
  labs(title = "Total trips by User type Vs. Day of the week") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#This code chunk creates a scatter plot of ride length vs. hour of the day, with each point representing a single ride.
# Extract the hour of the day from the started_at column
Trips2021$hour <- format(Trips2021$started_at, format = "%H")
# Create a scatter plot of ride length vs. hour of the day
ggplot(Trips2021, aes(x = hour, y = ride_length)) +
  geom_point() +
  labs(title = "Ride Length vs. Hour of the Day",
       x = "Hour of the Day",
       y = "Ride Length")

 #This code chunk creates a scatter plot of ride length vs. hour of the day against each user type, with each point representing a single ride.
# Create a scatter plot of ride length vs. hour of the day by each user type
ggplot(Trips2021, aes(x = hour, y = ride_length, color = users)) +
  geom_point() +
  labs(title = "Ride Length vs. Hour of the Day by User Type",
       x = "Hour of the Day",
       y = "Ride Length",
       color = "User Type")

# Create a bar chart of ride length by rideable type and user type
 ggplot(Trips2021, aes(x = rideable_type, y = ride_length, fill = users)) +
  geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Ride Length by Rideable Type and User Type",
        x = "Rideable Type",
        y = "Ride Length",
        fill = "User Type") +
   theme_minimal()


#####
library(RColorBrewer)
# Calculate the average ride length by hour of the day
ride_length_hour <- Trips2021 %>%
  mutate(hour = format(started_at, format = "%H")) %>%
  group_by(hour) %>%
  summarise(avg_ride_length = mean(as.numeric(ride_length)))

# Create a heatmap
ggplot(ride_length_hour, aes(x = hour, y = avg_ride_length, fill = avg_ride_length)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Average Ride Length by Hour of the Day Heatmap",
       x = "Hour of the Day",
       y = "Average Ride Length") +
  theme_minimal()

# This code chunk creates a bar chart showing the average ride length by user type vs. day of the week.
Trips2021 %>%
  group_by(users, day_of_week) %>%
  summarise(average_ride_length = mean(as.numeric(ride_length), na.rm = TRUE)) %>%
  ggplot(aes(x = day_of_week, y = average_ride_length/60, fill = users)) + # dividing by 60 to convert seconds to minutes
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Average Ride length by User type Vs. Day of the week")

#Visualize the average ride distance and month of the ride by users
Trips2021 %>% 
  group_by(users,month) %>% 
  summarise(average_ride_length = mean(as.numeric(ride_length))) %>% 
  arrange(month) %>% 
  ggplot(aes(x = month, y = average_ride_length, fill = users)) +
  labs(title = "Average ride distance of members and casual riders by month")+
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


# Convert hour to character
Trips2021$hour <- as.character(Trips2021$hour)

# Create the plot with organized x-axis labels
ggplot(Trips2021, aes(x = hour, y = ride_length, color = users)) +
  geom_point() +
  labs(title = "Ride Length and Hour of the Day by User Type",
       x = "Hour of the Day",
       y = "Ride Length",
       color = "User Type") +
  facet_wrap(~ day, ncol = 3) +
  theme_minimal() +
  scale_x_discrete(breaks = seq(0, 23, by = 2), labels = c("12AM", "2AM", "4AM", "6AM", "8AM", "10AM", "12PM", "2PM", "4PM", "6PM", "8PM", "10PM"))

# Convert the hour column to a factor with ordered levels
Trips2021$hour <- factor(Trips2021$hour, levels = sprintf("%02d", 1:24), ordered = TRUE)

# Define the order of weekdays
weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Create the plot
ggplot(Trips2021, aes(x = hour, y = ride_length, color = users)) +
  geom_point() +
  labs(title = "Ride Length by Day of the Week, Hour of the Day, and User Type",
       x = "Hour of the Day",
       y = "Ride Length",
       color = "User Type") +
  facet_grid(day ~ .) +
  scale_x_discrete(labels = function(x) weekday_order[as.integer(x)]) +
  theme_minimal()












































