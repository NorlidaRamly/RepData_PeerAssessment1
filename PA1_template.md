---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
##Package use

```r
library(knitr)
library(dplyr)
library(ggplot2)
```

##Loading and Preprocessing the Data

```r
data_full <- read.csv("C:/Users/BTM KPKT/Documents/GitHub/Reproducible Research/Assignment1 Reproducible/activity.csv", header=T,na.strings="NA", nrows=17568)
data_clean <- data_full[with(data_full, {!(is.na(steps))}),]
head(data_clean)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

##What is mean total number of steps taken per day?
###Calculate the total number of steps taken per day

```r
by_date <- group_by(data_clean, date)
steps_by_date <- summarise(by_date, total = sum(steps))
steps_by_date
```

```
## Source: local data frame [53 x 2]
## 
##          date total
##        (fctr) (int)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

###Make a histogram of the total number of steps taken each day

```r
hist(steps_by_date$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![plot of chunk Include histogram](figure/Include histogram-1.png) 

###Calculate and report the mean and median of the total number of steps taken per day

```r
summary(steps_by_date)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
###Mean = 10766; Median = 10765
```

##What is the average daily activity pattern?

```r
steps_by_interval <- aggregate(steps ~ interval, data_clean, mean)
head(steps_by_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```


```r
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![plot of chunk includes scatterplot](figure/includes scatterplot-1.png) 

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# find row with max of steps
max_steps_row <- which.max(steps_by_interval$steps)

## find interval with this max
steps_by_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
### Interval 835 contains the maximum number of steps
```

##Imputing missing values
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data_full))
```

```
## [1] 2304
```

```r
put_a_data <- data_full
for (i in 1:nrow(put_a_data)) {
        if (is.na(put_a_data$steps[i])) {
                interval_value <- put_a_data$interval[i]
                steps_value <- steps_by_interval[
                        steps_by_interval$interval == interval_value,]
                put_a_data$steps[i] <- steps_value$steps
        }
}
###NA is replaced by mean of 5 minute-interval
head(put_a_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
###New data set with no NA.
```


```r
by_date_new <- group_by(put_a_data, date)
steps_by_date2 <- summarise(by_date_new, total = sum(steps))
steps_by_date2
```

```
## Source: local data frame [61 x 2]
## 
##          date    total
##        (fctr)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

###Make a histogram of the total number of steps taken each day

```r
hist(steps_by_date2$total, main="Histogram of total number of steps per day (no missing value)", 
     xlab="Total number of steps in a day")
```

![plot of chunk Include histogram2](figure/Include histogram2-1.png) 
###Calculate and report the mean and median total number of steps taken per day
###Get mean and median of new data set (with no missing value)

```r
summary(steps_by_date)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
summary(steps_by_date2)
```

```
##          date        total      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
###Mean is the same.  Median is different.
###The impact of imputing missing data on the estimates of the total daily number of steps will provide different result.  
###Thus we must choose the correct method to impute the data.
```

##Are there differences in activity patterns between weekdays and weekends?
###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
put_a_data['type_of_day'] <- weekdays(as.Date(put_a_data$date))
head(put_a_data)
```

```
##       steps       date interval type_of_day
## 1 1.7169811 2012-10-01        0      Monday
## 2 0.3396226 2012-10-01        5      Monday
## 3 0.1320755 2012-10-01       10      Monday
## 4 0.1509434 2012-10-01       15      Monday
## 5 0.0754717 2012-10-01       20      Monday
## 6 2.0943396 2012-10-01       25      Monday
```

```r
put_a_data$type_of_day[put_a_data$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
put_a_data$type_of_day[put_a_data$type_of_day != "weekend"] <- "weekday"
head(put_a_data)
```

```
##       steps       date interval type_of_day
## 1 1.7169811 2012-10-01        0     weekday
## 2 0.3396226 2012-10-01        5     weekday
## 3 0.1320755 2012-10-01       10     weekday
## 4 0.1509434 2012-10-01       15     weekday
## 5 0.0754717 2012-10-01       20     weekday
## 6 2.0943396 2012-10-01       25     weekday
```
###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken.

```r
# convert type_of_day from character to factor
put_a_data$type_of_day <- as.factor(put_a_data$type_of_day)
str(put_a_data)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps      : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date       : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval   : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ type_of_day: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
# calculate average steps by interval across all days
put_a_data_steps_by_interval <- aggregate(steps ~ interval + type_of_day, put_a_data, mean)
```

##Create a plot

```r
qplot(interval, 
      steps, 
      data = put_a_data_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
        facet_wrap(~ type_of_day, ncol = 1)
```

![plot of chunk Include plot2](figure/Include plot2-1.png) 

```r
###Yes, there are differences in activity patterns between weekdays and weekends.
```
