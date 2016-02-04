
setwd("C:/Users/Jvandegevel/Documents/Coursera/Reproducible Research")

file <- unzip("activity.zip")
dataset <- read.csv(file,sep = ",", header = TRUE, stringsAsFactors = FALSE)
data <- dataset[which(!is.na(dataset$steps)),]


nrsteps <-tapply(data$steps, data$date, sum)

#Show histogram
hist(nrsteps, main ="Total # of steps per days", xlab = "", col="blue")

# What are the mean and median total number of steps taken per day?
mean(nrsteps)
median(nrsteps)

#What is the average daily activity pattern?
dailysteps <-tapply(data$steps, data$interval, mean)

plot(y = dailysteps,x = names(dailysteps), type = "l", xlab = "5-minute interval", 
     main = "Average Daily Activity Pattern", ylab = "Average # of steps")

max(dailysteps)
dailysteps[dailysteps == max(dailysteps)]


#Inputting missing values
data <- dataset
data[which(is.na(data$steps)),1] <- dailysteps[as.character(data[which(is.na(data$steps)),3])]
dailysteps_new<-tapply(data$steps, data$date, sum)

#Show new histogram
hist(dailysteps_new, main = "Total # of steps per day", xlab = "",col="red")

#Calculating new mean and median
mean(dailysteps_new)
median(dailysteps_new)


#Convert dates to date format
data$date <- as.Date(data$date)

#Use weekdays function
data$weekday <- weekdays(data$date)

#Add typeofday 
dayType <- function(dates) {
  f <- function(date) {
    if (weekdays(date) %in% c("zaterdag", "zondag")) {
      "weekend"
    }
    else {
      "weekday"
    }
  }
  sapply(dates, f)
}

data$typeofday <- as.factor(dayType(data$date))


#Creating subsets of data
data_weekend <- subset(data, typeofday == "weekend")
data_weekday <- subset(data, typeofday == "weekday")

#Calculate new daily steps per weekend and weekday
dailysteps_weekend <- tapply(data_weekend$steps, data_weekend$interval, mean)
dailysteps_weekday <- tapply(data_weekday$steps, data_weekday$interval, mean)

#Make plot for weekend
plot(y = dailysteps_weekend, x = names(dailysteps_weekend), type = "l", xlab = "5-minute interval", 
     main = "Average Daily Activity Pattern in weekend", ylab = "Average # of steps")

#Make plot for weekday
plot(y = dailysteps_weekday,x = names(dailysteps_weekday), type = "l", xlab = "5-minute interval", 
     main = "Average Daily Activity Pattern on weekdays", ylab = "Average # of steps")
