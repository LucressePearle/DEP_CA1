#input data set  into data frame 
crimeDS <- read.csv(file = "crime.csv", stringsAsFactors = TRUE)

# show the first ten records 
crimeDS[1:10,]