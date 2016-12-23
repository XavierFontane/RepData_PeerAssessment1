#Activity Monitoring Data

##Loading and preprocessing the data
```{r reading,echo=TRUE}
     activity<-read.csv("activity.csv")
```


##What is mean total number of steps taken per day?

####Make a histogram of the total number of steps taken each day
```{r hist1,echo=TRUE}
       total_steps <- aggregate(steps ~ date, activity, sum)
       hist(total_steps$steps, breaks=10, main = "Total Steps Each Day", col="red", xlab="Number of Steps")
```


####Calculate and report the mean and median total number of steps taken per day
```{r res1,echo=TRUE}
       options(scipen=999,digits=2)
       step_mean <- mean(total_steps$steps)
       step_median <- median(total_steps$steps)
```

    Mean total number of steps taken per day = `r step_mean`
    Median total number of steps taken per day = `r step_median`


##What is the average daily activity pattern?

####Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
```{r interval1,echo=TRUE}
       interval_steps <- aggregate(steps ~ interval, activity, mean)
       plot(interval_steps$interval,interval_steps$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```


####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r intmax,echo=TRUE}
       interval_max <- interval_steps[which.max(interval_steps$steps),1]
```
    5-min interval with maximum number of steps = `r interval_max`


##Imputing missing values

####Calculate and report the total number of missing values in the dataset
```{r missing,echo=TRUE}
       missing_val <- sum(is.na(activity$steps))
```


####Devise a strategy for filling in all of the missing values in the dataset

The strategy for filling consist in replace NA by the average of this interval where NA is located
```{r imputed,echo=TRUE}
       activity2 <- transform(activity, steps = ifelse(is.na(activity$steps), interval_steps$steps[match(activity$interval, interval_steps$interval)], activity$steps))
```


####Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r imputed_data,echo=TRUE}
       total_steps2 <- aggregate(steps ~ date, activity2, sum)
       step_mean2 <- mean(total_steps2$steps)
       step_median2 <- median(total_steps2$steps)
       interval_steps2 <- aggregate(steps ~ interval, activity2, mean)
       interval_max2 <- interval_steps2[which.max(interval_steps2$steps),1]
```

Values for imputed data are:  
    ·New mean total steps = `r step_mean2`  
    ·New median total steps = `r step_median2`  
    ·New interval with maximum number of steps = `r interval_max2`   


####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
```{r new_hist,echo=TRUE}
       hist(total_steps2$steps, breaks=10, main = "Total Steps Each Day - Imputed Data", col="blue", xlab="Number of Steps")
```


####What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r diff,echo=TRUE}
       options(scipen=999,digits=2)
       diff_steps <- sum(total_steps2$steps) - sum(total_steps$steps)
```
Difference for total daily number of steps = `r diff_steps`


##Are there differences in activity patterns between weekdays and weekends?

####Create a new factor variable in the dataset with two levels -- "weekday" and "weekend"
```{r w_day,echo=TRUE}
       w_day <- weekdays(as.Date(activity2$date))
       select <- ifelse (w_day == "sábado" | w_day == "domingo", "weekend", "weekday")
       activity2$wk <- as.factor(select)
```


####Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days
```{r final_plot,echo=TRUE}
       library(lattice)
       dataplot <- aggregate(steps ~ wk+interval, data=activity2, FUN=mean)
       xyplot(steps ~ interval | factor(wk),
              layout = c(1, 2),
              xlab="Interval",
              ylab="Number of steps",
              type="l",
              lty=1,
              data=dataplot)
```
