---
title: "Reproducible Research: Peer Assessment 1"
output: html_document

---

## Loading and preprocessing the data

```r
# Set working directory and unzip file, overwrite old one
setwd("~/Documents/Coursera Data Science/Reproducible research/RepData_Peerassessment1")
unzip("activity.zip", overwrite=TRUE)

# read data in dataframe, remove NA values
stepData <- read.csv("activity.csv", stringsAsFactors=FALSE)
stepDataClean <- stepData[!is.na(stepData$steps), ]
naData = stepData[is.na(stepData$steps), ]
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
stepsPerDay = aggregate(steps ~ date, data=stepData, FUN=sum, na.rm=TRUE)
qplot(steps, data=stepsPerDay, na.rm=TRUE, main="Histogram of number of steps per day", binwidth=500)
```

<img src="figure/unnamed-chunk-2.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

```r
meanSteps = mean(stepsPerDay$steps, na.rm=TRUE)
medianSteps = median(stepsPerDay$steps, na.rm=TRUE)
```
The mean total number of steps per day is 1.0766 &times; 10<sup>4</sup>, while the median total number of steps per day is 10765.

## What is the average daily activity pattern?

```r
activity <- aggregate(steps~interval, data=stepData, FUN=mean, na.rm=TRUE)
ggplot(data=activity, aes(x=interval, y=steps)) + 
    geom_line() + 
    ggtitle("Average number of steps per time interval")
```

<img src="figure/unnamed-chunk-3.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

```r
maxInterval <- activity[which.max(activity$steps), 1]
```
The interval with the highest number of steps, averaged over all days in the dataset, is 835.

## Imputing missing values

```r
library(dplyr,warn.conflicts = FALSE)
sumNA <- dim(naData)[1]
replaceData <- select(merge(select(naData,date:interval), activity), steps, date, interval)
stepDataNew <- arrange(rbind(replaceData, stepDataClean), date, interval)
stepsPerDayNew = aggregate(steps ~ date, data=stepDataNew, FUN=sum, na.rm=TRUE)
```
The number of missing values is 2304. To replace these values, the average activity in each time interval will be substituted. To this end, the rows with NA values are separated from the rest of the dataset, the column with steps data is removed, and the remaining data is joined with the averaged activity by interval. The resulting set is combined with the original data set and reordered.

```r
qplot(steps, data = stepsPerDayNew, na.rm=TRUE, binwidth=500, main="Histogram of number of steps per day")
```

<img src="figure/unnamed-chunk-5.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

```r
meanStepsNew <- mean(stepsPerDayNew$steps, na.rm=TRUE)
medianStepsNew <- median(stepsPerDayNew$steps, na.rm=TRUE)
```
For the new data set, the mean total number of steps per day is 1.0766 &times; 10<sup>4</sup>, while the median total number of steps per day is now 1.0766 &times; 10<sup>4</sup>. The mean number of steps is not changed, but the frequency has increased, because we added new data with the mean value. The median has shifted to the mean value.

## Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_TIME", "en_US") #Set location to ensure use of English names for weekdays
```

```
## [1] "en_US"
```

```r
weekOrWeekend <- function(date) {
    if (weekdays(as.Date(date)) == "Saturday" || weekdays(as.Date(date)) == "Sunday") {
        daytype <- "weekend"
    } else {
        daytype <- "weekday"
    }
}
stepDataNew <- mutate(stepDataNew, daytype = sapply(date, weekOrWeekend))
activityNew <- aggregate(steps~interval+daytype, data=stepDataNew, FUN=mean, na.rm=TRUE)

ggplot(activityNew, aes(x=interval, y=steps)) + 
    geom_line() + 
    facet_wrap( ~ daytype, ncol=1) +
    ggtitle("Average number of steps per time interval")
```

<img src="figure/unnamed-chunk-6.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />
From the figure above, it is clear that there is a difference is activity patterns for weekdays and the weekend. The 'go to work/school' peak in the morning is much smaller during the weekend, and the activity is more evenly distributed over the day.
