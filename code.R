#Q1 Load and read data
library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
activity <- read.csv("activity.csv",header=T)



#Q2 What is mean total number of steps taken per day?

totalstepsperday <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(totalstepsperday)
hist(totalstepsperday$steps, main="Total Steps per Day", xlab="Number of Steps per Day",  ylab = "Interval", col="blue")
dev.copy(png,file = "HistNumSteps.png")
dev.off()

#Calculate and report the mean and median of the total number of steps taken per day
msteps <- mean(totalstepsperday$steps)
msteps
medsteps <- median(totalstepsperday$steps)
medsteps





#Q3 What is the average daily activity pattern?

## five minute average using steps to interval - FUN = mean instead of sum
activity <- read.csv("activity.csv",header=T)
fivemin <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
## line chart
plot(x = fivemin$interval, 
     y = fivemin$steps, 
     type = "l", 
     col = "blue",
     xlab = "5-minute Intervals",
     ylab = "Average Steps Taken in Days",
     main = "Average Daily Activity Pattern")
dev.copy(png,file = "AvgActivityPattern.png")
dev.off()

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxsteps <- fivemin[which.max(fivemin$steps),]
maxsteps





#Q4 Imputing missing values

#Calculate and report the total number of missing values in the dataset
activity <- read.csv("activity.csv",header=T)
table(is.na(activity))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.#In the original data set aggregating (mean) steps over 5-minute interval
meaninterval<- aggregate(steps ~ interval, activity, FUN=mean)
#Merging the mean of total steps for activity date with the original data set
anew <- merge(x=activity, y=meaninterval, by="interval")
#Replacing the NA values with the mean for that 5-minute interval
anew$steps <- ifelse(is.na(anew$steps.x), anew$steps.y, anew$steps.x)
#Merged dataset which will be subsetted in the next step by removing not required columns
head(anew)

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
anew <- select(anew, steps, date, interval)
head(anew)


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
#Do these values differ from the estimates from the first part of the assignment?
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Aggregating(summation) of steps over date
totalstepsperday <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
totalsteps_new<- aggregate(steps ~ date, anew, FUN=sum)

#Plotting
#Setting up the pannel for one row and two columns
par(mfrow=c(1,2))

#Histogram after imputing NA values with mean of 5-min interval
hist(totalsteps_new$steps, 
     col="blue",
     xlab = "Total Number of Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(After imputing NA values)",
     cex.main = 0.7)

#Histogram with the orginal dataset
hist(totalstepsperday$steps, 
     col="purple", 
     xlab = "Total Number of Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(Orginal Dataset)",
     cex.main = 0.7)

dev.copy(png,file = "2hist.png")
dev.off()

msteps <- mean(totalstepsperday$steps)
msteps
medsteps <- median(totalstepsperday$steps)
medsteps

msteps_new <- mean(totalsteps_new$steps)
msteps_new
medsteps_new <- median(totalsteps_new$steps)
medsteps_new

#Comparing Means
paste("New Mean      :", round(msteps_new,2), "," ,  
      " Original Mean :", round(msteps,2),"," , 
      " Difference :",round(msteps_new,2) -  round(msteps,2))

#Comparing Medians
paste("New Median    :", medsteps_new, ",", 
      " Original Median :", medsteps,"," , 
      " Difference :",round(medsteps_new-medsteps,2))




#Q5 Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
library(chron)
table(is.weekend(anew$date))
anew$dayofweek <- ifelse(is.weekend(anew$date), "weekend", "weekday")
table(anew$dayofweek)
meaninterval_new<- aggregate(steps ~ interval + dayofweek, anew, FUN=mean)
head(meaninterval_new)

ggplot(meaninterval_new, aes(x=interval, y=steps)) + 
        geom_line(color="blue", size=1) + 
        facet_wrap(~dayofweek, nrow=2) +
        labs(x="\nInterval", y="\nNumber of steps", title = "Total Number Of Steps Taken in Weekday & Weekend")

dev.copy(png,file = "week.png")
dev.off()

