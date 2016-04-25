# FitBit Activity data Analysis
24th April  

### Description

This is the first project of Reproducible Research course in Coursera's Data Science Specialization. This project deals with answering a series of questions by analysing an anonymous Fitbit data.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](provided in the github repo) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Loading and preprocessing the data

The repo has been forked, downloaded to github desktop and used for analysis
unzip the file `activity.zip` and load it into a dataframe `data`


```r
data<-read.csv("activity/activity.csv")
```

## What is mean total number of steps taken per day?

Sum total number of steps by day (date), create a histogram, calculate mean and median


```r
steps_eachday<-aggregate(steps~date,data,sum)
hist(steps_eachday$steps,main = "Total steps per each day", col = "red", xlab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)



```r
steps_mean<-mean(steps_eachday$steps)
steps_median<-median(steps_eachday$steps)
```
The mean thus obtained is `steps_mean` = 10766.188, median is `steps_median` = 10765 


## What is the average daily activity pattern?

* Calculate Average number of steps for each interval for each day
* Plot the Average Number Steps per Day by Interval into a time series plot. 
* Find interval with most average steps. 


```r
steps_eachinterval <- aggregate(steps ~ interval, data, mean)
plot(steps_eachinterval$interval,steps_eachinterval$steps, type="l", xlab="Interval of the day", ylab="Number of Steps",main="Average Number of Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)



```r
max_steps<-steps_eachinterval[which.max(steps_eachinterval$steps),1]
```

The 5-minute interval containing the maximum number of steps in the data is 835.

## Impute missing values. Compare imputed to non-imputed data.
Missing data (NA) needed to be imputed. Missing values were imputed by inserting the average for each interval.  


```r
noofNAs<- sum(!complete.cases(data))
rearranged_data <- transform(data, steps = ifelse(is.na(data$steps), steps_eachinterval$steps[match(data$interval, steps_eachinterval$interval)], data$steps))
```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps which is very high. 


```r
rearranged_data[as.character(rearranged_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram. 


```r
steps_eachday_re <- aggregate(steps ~ date, rearranged_data, sum)
hist(steps_eachday_re$steps, main = paste("Total steps per each day"), col="red", xlab="Number of Steps")

#difference histogram 
hist(steps_eachday$steps, main = paste("Total steps per each day"), col="blue", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("red", "blue"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)


Calculate new mean and median for imputed data. 


```r
steps_mean_re <- mean(steps_eachday_re$steps)
steps_median_re <- median(steps_eachday_re$steps)
```

Calculate difference between imputed and non-imputed data.


```r
mean_difference <- steps_mean_re - steps_mean
median_difference <- steps_median_re - steps_median
```

Calculate total difference.


```r
total_difference_steps <- sum(steps_eachday_re$steps) - sum(steps_eachday$steps)
```
* The imputed data mean is 10589.69
* The imputed data median is 10766
* The difference between the non-imputed mean and imputed mean is -176.4948
* The difference between the non-imputed mean and imputed mean is 1.18867
* The difference between total number of steps between imputed and non-imputed data is 75363.32. Thus, the imputed data has  75363 more steps than original data.


## Are there differences in activity patterns between weekdays and weekends?
Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.  


```r
library(lattice)
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
rearranged_data$dayclassify = as.factor(ifelse(is.element(weekdays(as.Date(rearranged_data$date)),weekdays), "Weekday", "Weekend"))

steps_eachinterval_re <- aggregate(steps ~ interval + dayclassify, rearranged_data, mean)

xyplot(steps_eachinterval_re$steps ~ steps_eachinterval_re$interval|steps_eachinterval_re$dayclassify, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)

