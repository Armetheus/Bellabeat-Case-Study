# load libraries.

library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(plotrix)
library(scales)

# Import data
daily_activity <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/daily_activity.csv")
daily_calories <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/daily_calories.csv")
daily_intensities <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/daily_Intensities.csv")
daily_steps <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/daily_steps.csv")
heart_rate <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/heartrate_seconds.csv")
hourly_calories <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/hourly_calories.csv")
hourly_intensities <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/hourly_intensities.csv")
hourly_steps <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/hourly_steps.csv")
weight_log_info <- read_csv("C:/Users/RAZER STEALTH/OneDrive/R_project/Fitabase/weight_log_info.csv")


# to view the data structure and find the number of missing values 

head(daily_activity)
str(daily_activity)

head(daily_calories)
str(daily_calories)

head(daily_intensities)
str(daily_intensities)

head(daily_steps)
str(daily_steps)

# DATA CLEANING

# to check for missing values
missing_values <- sum(is.na(daily_activity))
print(missing_values)

missing_cal <- sum(is.na(daily_calories))
print(missing_cal)

missing_intensity <- sum(is.na(daily_intensities))
print(missing_intensity)

missing_steps <- sum(is.na(daily_steps))
print(missing_steps)


# to check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(daily_steps))

# to change the date column from 'character' to 'date' format
daily_activity <- daily_activity %>% 
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y"))

print(class(daily_activity$ActivityDate))

daily_calories <- daily_calories %>% 
  rename(ActivityDate = ActivityDay) %>% 
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y"))

# to check if the column name was changed from ActivityDay to Activity Date
head(daily_calories)

print(class(daily_calories$ActivityDate))

daily_intensities <- daily_intensities %>% 
  rename(ActivityDate = ActivityDay) %>% 
  mutate(ActivityDate = as.Date(ActivityDate, format = '%m/%d/%Y'))

print(class(daily_intensities$ActivityDate))

daily_steps <- daily_steps %>% 
  rename(ActivityDate = ActivityDay) %>% 
  mutate(ActivityDate = as.Date(ActivityDate, format = '%m/%d/%Y'))

print(class(daily_steps$ActivityDate))


# to merge the daily activity, daily_calories, daily_intensities and daily_steps into one table
merged_daily_records <- daily_activity %>% 
  merge(daily_calories, by = c('ActivityDate','Id' )) %>% 
  merge(daily_intensities, by = c('ActivityDate','Id' )) %>%
  merge(daily_steps, by = c('ActivityDate','Id' )) 


# to clean the hourly dataset
head(hourly_calories)
str(hourly_calories)

head(hourly_intensities)
str(hourly_intensities)

head(hourly_steps)
str(hourly_steps)

# to split date and time to separate columns by the first empty space  without separating other extra spaces
hourly_calories <- hourly_calories %>% 
  separate(ActivityHour, into = c('date','time'), sep = ' ', extra = 'merge')   

hourly_intensities <- hourly_intensities %>% 
  separate(ActivityHour, into = c('date','time'), sep = ' ', extra = 'merge')  

hourly_steps <- hourly_steps %>% 
  separate(ActivityHour, into = c('date','time'), sep = ' ', extra = 'merge')  


# Renaming the 'time' column to 'time_12hour'
hourly_calories <- hourly_calories %>% rename(time_12hour = time_24hour)
hourly_calories <- hourly_calories %>% rename(time_24hour = time_12hour)

# to convert time from 12hour format to 24 hour format
hourly_calories <- hourly_calories %>%
  mutate(time_24hour = format(parse_date_time(time_24hour, orders = "%I:%M:%S %p"), "%H:%M:%S"))

# to remove column time_r
hourly_calories <- hourly_calories[, -which(names(hourly_calories) == 'time_r')]

# Renaming the 'time' column to 'time_24hour'
hourly_steps <- hourly_steps %>% rename(time_24hour = time)

# to convert time from 12hour format to 24 hour format
hourly_steps <- hourly_steps %>%
  mutate(time_24hour = format(parse_date_time(time_24hour, orders = "%I:%M:%S %p"), "%H:%M:%S"))

# Renaming the 'time' column to 'time_24hour'
hourly_intensities <- hourly_intensities %>% rename(time_24hour = time)

# toconvert time format from %H:%M to %H:%M:%S
hourly_intensities$time_24hour <- format(strptime(hourly_intensities$time_24hour, format = "%H:%M"), format = "%H:%M:%S")


# to merge the hourly_calories, hourly_intensities, and hourly_steps into one table
merged_hourly_records <- hourly_calories %>% 
  merge(hourly_intensities, by = c('date','Id' )) %>% 
  merge(hourly_steps, by = c('date','Id' )) 


# to check for the summary statistics of the daily records 
summary(merged_daily_records)

# to check for the summary statistics of the hourly records 
summary(merged_hourly_records)

distinct_id <- unique(merged_daily_records$Id)
num_distinct_id <- length(distinct_id)
print(num_distinct_id)

print(max(merged_daily_records$TotalSteps))
print(min(merged_daily_records$TotalSteps))
print(mean(merged_daily_records$TotalSteps))

print(max(merged_daily_records$TotalDistance))
print(min(merged_daily_records$TotalDistance))
print(mean(merged_daily_records$TotalDistance))


# Convert 'ActivityDate' to week format using lubridate
merged_daily_records$week <- lubridate::week(merged_daily_records$ActivityDate)

# Group the data by week and calculate the mean Calories.x per week
grouped_data <- merged_daily_records %>%
  group_by(week) %>%
  summarise(avg_calories = mean(Calories.x))

# active minutes
max_sed <- max(merged_daily_records$SedentaryMinutes.y)
print(max_sed)
# max_sed = 1440

max_light <- max(merged_daily_records$LightlyActiveMinutes.y)
print(max_light)
# max_light = 518

max_fair <- max(merged_daily_records$FairlyActiveMinutes.y)
print(max_fair)
# max_fair = 143

max_very <- max(merged_daily_records$VeryActiveMinutes.y)
print(max_very)
# max_very = 210

# pie chart to show activity minutes
# Create sample data
values <- c(1440, 518, 143, 210)
labels <- c("Sedentary Minutes", "Lightly Active Minutes", "Fairly Active Minutes", "Very Active Minutes")

# Calculated percentage values
percent <- round(values / sum(values) * 100, 1)

# Created a data frame
data <- data.frame(labels, percent)

# Defined color codes
color_codes <- c("#5F9EA0", "#FFFACD", "#6495ED", "#F08080")

p <- ggplot(data, aes(x = "", y = percent, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_discrete(guide = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Users Daily Activity Minutes") +
  theme_void()

# To add labels and legend to the pie chart
p + geom_text(aes(label = paste(percent, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = color_codes, labels = labels, guide = guide_legend())

# Group the data by Activity date and calculate the mean Calories.x per day
group_data <- summarise(group_by(merged_daily_records, ActivityDate), avg_calories = mean(Calories.x))


# To create a bar plot for  daily average calories
ggplot(group_data, aes(x = ActivityDate, y = avg_calories)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("ActivityDate") +
  ylab("avg_calories") +
  ggtitle("Average Calories by Activity Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# To convert ActivityDate to Date type
group_data$ActivityDate <- as.Date(group_data$ActivityDate)

# To create a bar plot of average calories with grouped x-axis by week
ggplot(group_data, aes(x = ActivityDate, y = avg_calories)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", 
               limits = c(min(group_data$ActivityDate), max(group_data$ActivityDate))) +
  xlab("Week") +
  ylab("Average Calories") +
  ggtitle("Average Calories by Week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)


# to grouped by id and find average steps, average calories and average steps
daily_avg <- merged_daily_records %>% 
  group_by(Id) %>% 
  summarise (avg_daily_steps = mean(TotalSteps), avg_daily_calories = mean(Calories.y), avg_distance = mean(TotalDistance))

head(daily_avg)

ggplot(daily_avg, aes(x = Id, y = avg_daily_steps)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Id") +
  ylab("Average steps") +
  ggtitle("Average daily steps of each user") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# To Sort the data frame by the ID column in ascending order
daily_avg_ascending <- daily_avg[order(daily_avg$avg_daily_steps), ]

# Print the sorted data frame
print(daily_avg_ascending)

least_steps_users = head(daily_avg_ascending, n = 10)
top_steps_users = tail(daily_avg_ascending, n = 10)


# BAR CHARTS FOR USERS WITH THE LEAST DAILY AVERAGE STEPS 
# Reshape data to long formatv
data_long <- tidyr::pivot_longer(least_steps_users, 
                                 cols = c(avg_daily_steps, avg_daily_calories),
                                 names_to = "Category", values_to = "Values")

# Plot grouped bar chart of users with the least average steps 
ggplot(data_long, aes(x = Id, y = Values, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Id", y = "Values", title = "Least steps users daily average") +
  scale_fill_manual(values = c("blue", "red"))
  theme(plot.width = 10, plot.height = 6)
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  

# BAR CHART FOR USERS WITH THE HIGEHEST DAILY AVERAGE STEPS
# Reshape data to long formatv
data_long <- tidyr::pivot_longer(top_steps_users, 
                                 cols = c(avg_daily_steps, avg_daily_calories),
                                 names_to = "Category", values_to = "Values")

# Plot grouped bar chart of users with the highest average steps
ggplot(data_long, aes(x = Id, y = Values, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Id", y = "Values", title = "Top steps users daily average") +
  scale_fill_manual(values = c("blue", "red"))
  theme(plot.width = 10, plot.height = 6)
  theme(axis.text.x = element_text(angle = 45, hjust = 1, )) 
  

max_steps <- max(daily_avg$avg_daily_steps)
print(max_steps)
# max_steps = 16040.03

min_steps <- min(daily_avg$avg_daily_steps)
print(min_steps)
# min_steps = 916.129


# CORRELATION SCATTER PLOT 
# to plot the scatter plot  with mean of steps on x-axis and mean of calories on Y-axis
plot(merged_daily_records$TotalSteps,merged_daily_records$Calories.x, xlab = 'Steps', ylab = 'Calories', col = c('red','blue'))

# to plot the scatter plot  with mean of distance on x-axis and mean of calories on Y-axis
plot(merged_daily_records$TotalDistance,merged_daily_records$Calories.x, xlab = 'Distance', ylab = 'Calories', col = c('red','blue'))

# Group the data by Activity date and calculate the mean steps per day
group_data3 <- summarise(group_by(merged_daily_records, ActivityDate), avg_steps = mean(TotalSteps))


# MERGED HOURLY RECORDS VISUALIZTIONS

# Group the data by date and calculate the mean Calories.x per day
group_hr_data <- merged_hourly_records %>%
  group_by(time_24hour.x) %>%
  summarize(average_calories = mean(Calories)) 


# Group the data by time and calculate the mean intensities.x per day
group_hr_data2 <- merged_hourly_records %>%
  group_by(time_24hour.y) %>%
  summarize(average_intensity = mean(AverageIntensity)) 

print(class(group_hr_data$time_24hour.x))
print(class(group_hr_data$avg_calories))


#Line graph for average a calories per hour
ggplot(data = group_hr_data) +
  geom_line(aes(x = time_24hour.x, y = average_calories, group = 1), color = "blue", size = 1) +
  xlab("Time") +
  ylab("Calories") +
  ggtitle("Hourly Average Colories")

# Line Graph for avg intensity per hour

ggplot(data = group_hr_data2) +
  geom_line(aes(x = time_24hour.y, y = average_intensity, group = 1), color = "red", size = 1) +
  xlab("Time") +
  ylab("Average Intensty") +
  ggtitle("Hourly Average Intensity")
  
group_hr_data3 <- merged_hourly_records %>%
  group_by(time_24hour.y) %>%
  summarize(average_intensity = mean(AverageIntensity), average_calories = mean(Calories), average_steps = mean(StepTotal))

# Line Graph for avg steps per hour
ggplot(data = group_hr_data3) +
  geom_line(aes(x = time_24hour.y, y = average_steps, group = 1), color = "blue", size = 1) +
  xlab("Time") +
  ylab("Average Steps") +
  ggtitle("Hourly Steps")

# Bar chart for avg intensity per hour
ggplot(data = group_hr_data3) +
  geom_bar(aes(x = time_24hour.y, y = average_intensity, fill = average_intensity), stat = "identity") +
  xlab("Time") +
  ylab("intensity") +
  ggtitle("Hourly intensity") +
  scale_fill_gradient(low = "blue", high = "red")

max_steps <- max(group_hr_data3$average_steps)
print(max_steps)

min_steps <- min(group_hr_data3$average_steps)
print(min_steps)

# PIE CHART OF users BMI

#Categorize BMI values into groups using if-conditional statements
underweight_count <- sum(weight_log_info$BMI < 18.5)
healthy_weight_count <- sum(weight_log_info$BMI >= 18.5 & weight_log_info$BMI < 25)
overweight_count <- sum(weight_log_info$BMI >= 25 & weight_log_info$BMI < 30)
obesity_count <- sum(weight_log_info$BMI >= 30)
  
bmi_counts <- c(underweight_count, healthy_weight_count, overweight_count, obesity_count)
bmi_labels <- c("Underweight", "Healthy weight", "Overweight", "Obesity")
bmi_percentages <- round((bmi_counts / sum(bmi_counts)) * 100, 1)
  
# A data frame of summary of BMI 
data2 <- data.frame(bmi_labels, bmi_percentages)
  
# Tp Plot the pie chart of Users BMI state
ggplot(data = data2, aes(x = "", y = bmi_counts, fill = bmi_labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "BMI Range Distribution",
        fill = "BMI Range") +
  scale_fill_manual(values = c("Underweight" = "#FFDAB9",
                                 "Healthy weight" = "#6495ED",
                                 "Overweight" = "#FFE4C4",
                                 "Obesity" = "#DC143C")) +
  geom_text(aes(label = paste0(bmi_percentages, "%")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 4) +
  theme_void()



