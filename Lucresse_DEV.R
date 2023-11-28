#input data set  into data frame 
crimeDS <- read.csv(file = "crime.csv", stringsAsFactors = TRUE)

#we get the dimension of the dataset
dim(crimeDS)

#we check the structure of the data
str(crimeDS)

# show the first ten records 
crimeDS[1:10,]

# we find the name of all our column
column_names <- names(crimeDS)
print(column_names)

#========================================================================================================


#=========================================================================================================
#data cleaning

#count total missing values in each column
sapply(crimeDS, function(x) sum(is.na(x)))

# we will handle the empty string in the shooting attribute
# we will replace the empty space by N 
crimeDS$SHOOTING <- as.character(crimeDS$SHOOTING)
crimeDS$SHOOTING <- factor(
  ifelse(is.na(crimeDS$SHOOTING), "N", crimeDS$SHOOTING),
  levels = c("Y", "N")
)

#check the missing space
summar1 <- colSums(crimeDS ==" ")
print(summar1)
crimeDS2 <-crimeDS
print(crimeDS2)

# Remove specified columns
crimeDS_Clean <- crimeDS2[, !(names(crimeDS) %in% c("Lat", "Long", "Location","UCR_PART","STREET"))]

#check all the empty values for the variables
missing_val <- is.na(crimeDS_Clean)
summary(missing_val)

#Replacing the NAS in the reporting_area column by the mean
reporting_area_median <- median(crimeDS_Clean$REPORTING_AREA, na.rm = TRUE)
crimeDS_Clean$REPORTING_AREA[is.na(crimeDS_Clean$REPORTING_AREA)] <- reporting_area_median
colSums(is.na(crimeDS_Clean))
head(crimeDS_Clean)

# check the variables with the Na
colSums(is.na(crimeDS_Clean))

# function to remove outliers 
remove_outliers <- function(crimeDS_Clean, variable, threshold = 1.5) {
  q1 <- quantile(crimeDS_Clean[[variable]], 0.25)
  q3 <- quantile(crimeDS_Clean[[variable]], 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - threshold * iqr
  upper_limit <- q3 + threshold * iqr
  crimeDataSet_filtered <- crimeDS_Clean[crimeDS_Clean[[variable]] >= lower_limit & crimeDS_Clean[[variable]] <= upper_limit, ]
  return(crimeDataSet_filtered)
}
#Removing outliers
 variable_outliers <- "HOUR"
crimeDS_Clean <- remove_outliers( crimeDS_Clean, variable_outliers)

#printing theh clean dataset
 print(crimeDS_Clean)

#========================================================================================
#lets calculate the statistical parameter for each of the numerical variables
# statistical  of HOUR
hour_mean <- mean(crimeDS_Clean$HOUR)
print(hour_mean)
hour_median <- median(crimeDS_Clean$HOUR)
print(hour_median)
hour_min <- min(crimeDS_Clean$HOUR)
print(hour_min)
hour_max <- max(crimeDS_Clean$HOUR)
print(hour_max)
hour_sd <- sd(crimeDS_Clean$HOUR)
print(hour_sd)

# statistical  of  year
year_min <- min(crimeDS_Clean$YEAR)
print(year_min)
year_max <- max(crimeDS_Clean$YEAR)
print(year_max)

# statistical  of month
month_mean <- mean(crimeDS_Clean$MONTH)
print(month_mean)
month_median <- median(crimeDS_Clean$MONTH)
print(month_median)
month_min <- min(crimeDS_Clean$MONTH)
print(month_min)
month_max <- max(crimeDS_Clean$MONTH)
print(month_max)
month_sd <- sd(crimeDS_Clean$MONTH)
print(month_sd)

# statistical  of reporting area
reporting_area_mean <- mean(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_mean)
reporting_area_median <- median(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_median)
reporting_area_min <- min(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_min)
reporting_area_max <- max(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_max)
reporting_area_sd <- sd(crimeDS_Clean$REPORTING_AREA)
print(reporting_area_sd)







# ================================================================================================
#min and max normalization
# required libraries
library(ggplot2)
library(robustbase)
library(gridExtra)
# the function
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# applying Min-Max Normalization to the numerical columns
columnsNormalize <- c("YEAR", "MONTH", "HOUR","REPORTING_AREA")
crimeDS_minmax <- crimeDS_Clean
crimeDS_minmax[, columnsNormalize] <- apply(crimeDS_Clean[, columnsNormalize], 2, normalizeMinMax)
# print the results
print(crimeDS_minmax)

  
# Robust Standardization function
columnsNormalize <- c("YEAR", "MONTH", "HOUR", "REPORTING_AREA")
crimeDS_robust <- crimeDS_Clean
crimeDS_robust[, columnsNormalize] <- scale(crimeDS_Clean[, columnsNormalize], center = TRUE, scale = TRUE)
print(crimeDS_robust)
 
#z-score standardization
standardizeZScore <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
columnsNormalize <- c("YEAR", "MONTH", "HOUR", "REPORTING_AREA")
crimeDS_zscore <- crimeDS_Clean
crimeDS_zscore[, columnsNormalize] <- apply(crimeDS_Clean[, columnsNormalize],2, standardizeZScore)
print(crimeDS_zscore)



#================================================================================================================


#========================================================================
#Data visualization
library(ggplot2)
ggplot(crimeDS_Clean, aes(x = YEAR)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Kernel Density Plot of Crime Incidents by Year")

ggplot(crimeDS_Clean, aes(x = 1, y = MONTH)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Boxplot of Crime Incidents by Reporting Area")

ggplot(crimeDS_Clean, aes(x = REPORTING_AREA)) +
  geom_histogram(binwidth = 1, fill = "black", color = "purple") +
  labs(title = "Distribution of Crime Incidents by Reporting Area")


# Convert DAY_OF_WEEK to a factor with a specific order
crimeDS_Clean$DAY_OF_WEEK <- factor(crimeDS_Clean$DAY_OF_WEEK)
# Aggregate the data to get the total sum of hours for each day
agg_data <- crimeDS_Clean %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(Total_Hours = sum(HOUR, na.rm = TRUE))
# Create a bar plot
ggplot(agg_data, aes(x = DAY_OF_WEEK, y = Total_Hours)) +
  geom_bar(stat = "identity", fill= "blue") +
  labs(title = "Total Sum of Hours Per Day", x = "Day of Week", y = "Total Hours") +
  theme_minimal()
