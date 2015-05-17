---
title: "peerAssesment1"
author: "Micah Norman"
date: "May 17, 2015"
output: html_document
---

```r
#install.packages('dplyr')
#install.packages('gplot2')
#install.packages('reshape2')
library(dplyr)
library(ggplot2)
library(reshape2)
```

```r
getData <- function(){
    activity<-read.csv(file = 'activity.csv', stringsAsFactors=FALSE)
    activity$date<-as.Date(strptime(activity$date, format='%Y-%m-%d'))
    activity
}
activity <- getData()
```


## Summarazation  



```r
activitySum <- sum(na.omit(activity$steps))
totalSteps      <-activity %>% summarise('TotalSteps' = sum(na.omit(steps)))

summaryStepsPerDay<-activity %>% group_by(date) %>% summarise('medianStepsPerDay' = median(na.omit(steps)), 'meanStepsPerDay' = mean(na.omit(steps)), 'TotalStepsPerDay' = sum(na.omit(steps)), 'minStepsPerDay' = min(steps, na.rm=TRUE) , 'maxStepsPerDay' = max(steps, na.rm=TRUE) )
summaryStepsPerDay$dt <- ts(summaryStepsPerDay$date)


summaryStepsPerInterval<-activity %>% group_by(interval) %>% summarise('medianStepsPerInterval' = median(na.omit(steps)), 'meanStepsPerInterval' = mean(na.omit(steps)), 'TotalStepsPerInterval' = sum(na.omit(steps)), 'minStepsPerInterval' = min(steps, na.rm=TRUE) , 'maxStepsPerInterval' = max(steps, na.rm=TRUE) )
# summaryStepsPerInterval$dt <- ts(summaryStepsPerInterval$date)

startDate   <- min(activity$date)
endDate     <- max(activity$date)
meanDate    <- mean(activity$date)

smd<-melt(summaryStepsPerDay[complete.cases(summaryStepsPerDay),], id.vars = 'dt')
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
smi<-melt(summaryStepsPerInterval[complete.cases(summaryStepsPerInterval),], id.vars = 'interval')
```




### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day


```r
hist(summaryStepsPerDay$TotalStepsPerDay)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

The total steps for all measurements between 2012-10-01 and 2012-11-30 was 570608.  




2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
meanStepsPerDay<-round(mean(summaryStepsPerDay$TotalStepsPerDay))
medianStepsPerDay<-round(median(summaryStepsPerDay$TotalStepsPerDay))
```
The mean steps per day are: 9354 while the median steps per day are 1.0395 &times; 10<sup>4</sup>.

### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
title<-'Steps Per Interval'
smi %>% filter(variable=='meanStepsPerInterval' | variable == 'meanStepsPerInterval')%>%
    ggplot()+aes(x=interval, y=value, colour=variable)+geom_line() + ggtitle(paste('Summary', title))
```

![plot of chunk perIntSum](figure/perIntSum-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mostActiveInterval<-activity[complete.cases(activity), ] %>% group_by(interval) %>% summarise('m'=sum(steps))
mInterval<-mostActiveInterval[which.max(mostActiveInterval$m),][,1]
```

The interval **835** is the most active interval accross all data in the dataset.

### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
totalIncompleteRecords<-abs(sum(complete.cases(activity)) - nrow(activity))
naAccrossCat<-sapply(names(activity), function(x) sum(is.na(activity[,x])))
```

There are 2304 records with missing Data.  Of these, the following table lists the number of NA's per column. 

2304, 0, 0

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


##### Summarizations


Scaled by Log

```r
title<-'Steps Per Interval'

ggplot(data=smi)+aes(x=interval, y=log(value), colour=variable)+geom_line() + ggtitle(paste('Summary', title))
```

![plot of chunk catFigs](figure/catFigs-1.png) 



##### Per Day

```r
title<-'Steps Per Day'

ggplot(data=activity)+aes(x=date)+geom_histogram(aes(y=factor(steps)), stat='identity') + ggtitle(paste('Histogram', title))
```

![plot of chunk perDaySum](figure/perDaySum-1.png) 

```r
ggplot(data=smd)+aes(x=dt, y=log(value), colour=variable)+geom_line() + ggtitle(paste('Summary', title))
```

![plot of chunk perDaySum](figure/perDaySum-2.png) 









