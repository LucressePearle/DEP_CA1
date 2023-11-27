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

#check all the empty values for the variables
missing_val <- is.na(crimeDS)
summary(missing_val)

# check the variables with the Na
colSums(is.na(crimeDS))

#lets calculate the statistical parameter for each of the numerical variables
# statistical  of HOUR
hour_mean <- mean(crimeDS$HOUR)
print(hour_mean)
hour_median <- median(crimeDS$HOUR)
print(hour_median)
hour_min <- min(crimeDS$HOUR)
print(hour_min)
hour_max <- max(crimeDS$HOUR)
print(hour_max)
hour_sd <- sd(crimeDS$HOUR)
print(hour_sd)

# statistical  of  year
year_min <- min(crimeDS$YEAR)
print(year_min)
year_max <- max(crimeDS$YEAR)
print(year_max)

# statistical  of month
month_mean <- mean(crimeDS$MONTH)
print(month_mean)
month_median <- median(crimeDS$MONTH)
print(month_median)
month_min <- min(crimeDS$MONTH)
print(month_min)
month_max <- max(crimeDS$MONTH)
print(month_max)
month_sd <- sd(crimeDS$MONTH)
print(month_sd)






