library(lattice)
data<-read.csv("activity/activity.csv")
steps_eachday<-aggregate(steps~date,data,sum)
hist(steps_eachday$steps,main = "Total steps per each day", col = "red", xlab = "Number of steps")
steps_mean<-mean(steps_eachday$steps)
steps_median<-median(steps_eachday$steps)

steps_eachinterval <- aggregate(steps ~ interval, data, mean)
plot(steps_eachinterval$interval,steps_eachinterval$steps, type="l", xlab="Interval of the day", ylab="Number of Steps",main="Average Number of Steps per Interval")
max_steps<-steps_eachinterval[which.max(steps_eachinterval$steps),1]

noofNAs<- sum(!complete.cases(data))
rearranged_data <- transform(data, steps = ifelse(is.na(data$steps), steps_eachinterval$steps[match(data$interval, steps_eachinterval$interval)], data$steps))
rearranged_data[as.character(rearranged_data$date) == "2012-10-01", 1] <- 0
steps_eachday_re <- aggregate(steps ~ date, rearranged_data, sum)
hist(steps_eachday_re$steps, main = paste("Total steps per each day"), col="red", xlab="Number of Steps")

#difference histogram 
hist(steps_eachday$steps, main = paste("Total steps per each day"), col="blue", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("red", "blue"), lwd=10)

steps_mean_re <- mean(steps_eachday_re$steps)
steps_median_re <- median(steps_eachday_re$steps)

mean_difference <- steps_mean_re - steps_mean
median_difference <- steps_median_re - steps_median

total_difference_steps <- sum(steps_eachday_re$steps) - sum(steps_eachday$steps)


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
rearranged_data$dayclassify = as.factor(ifelse(is.element(weekdays(as.Date(rearranged_data$date)),weekdays), "Weekday", "Weekend"))

steps_eachinterval_re <- aggregate(steps ~ interval + dayclassify, rearranged_data, mean)

xyplot(steps_eachinterval_re$steps ~ steps_eachinterval_re$interval|steps_eachinterval_re$dayclassify, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")