---
title: "PA1_template"
author: "Hemant"
output: html_document
---




## Importing the file activity.csv


```r
activity<-read.csv("./repdata_data_activity/activity.csv", sep=",", header = TRUE)
activity$date<-as.Date(activity$date)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## Plotting histogram of total steps per day


```r
#library(ggplot2)
agg<-aggregate(steps~date, activity, sum)
g<-ggplot(agg, aes(steps))
g+geom_histogram(color="black", fill="#03FC49")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk histogram](figure/histogram-1.png)

## Mean and Median of the steps taken each day.


```r
mean(activity$steps, na.rm=TRUE)
```

```
## [1] 37.3826
```

```r
median(activity$steps, na.rm=TRUE)
```

```
## [1] 0
```

## Mean and Median of the total steps taken each day.


```r
mean(agg$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(agg$steps, na.rm=TRUE)
```

```
## [1] 10765
```

## Time series plot of Steps vs Intervals 


```r
agg<-aggregate(steps~interval, activity, mean, na.rm=TRUE)
g<-ggplot(agg, aes(interval, steps))
g+geom_point(color="#0398FC")+geom_line(color="#FC0328")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## Interval with the highest number of steps


```r
agg[agg$steps==max(agg$steps),]$interval
```

```
## [1] 835
```

## Imputing NA values in the dataset with the mean values of steps taken in a new dataset


```r
activity_na<-activity
activity_na[is.na(activity_na)]=mean(activity_na$steps, na.rm=TRUE)
head(activity_na)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

## Histogram of total steps taken each day


```r
agg<-aggregate(steps~date, activity_na, sum)
g<-ggplot(agg, aes(steps))
g+geom_histogram(color="black", fill="#0390FC")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## Mean and Median after imputing NA values


```r
mean(agg$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(agg$steps, na.rm=FALSE)
```

```
## [1] 10766.19
```

The aggregated mean is same as before but the median differs. This is due to the presence of 37.38 in place of NA values in the dataset.

## Using weekdays() analysis comparing average steps taken per 5-minute interval on weekends and weekdays 


```r
activity_na<-mutate(activity_na, day=weekdays(date))
head(activity_na)
```

```
##     steps       date interval    day
## 1 37.3826 2012-10-01        0 Monday
## 2 37.3826 2012-10-01        5 Monday
## 3 37.3826 2012-10-01       10 Monday
## 4 37.3826 2012-10-01       15 Monday
## 5 37.3826 2012-10-01       20 Monday
## 6 37.3826 2012-10-01       25 Monday
```

```r
sub_weekday<-filter(activity_na, day!=c("Saturday","Sunday"))
sub_weekend<-filter(activity_na, day==c("Saturday","Sunday"))
sub_weekday$day<-"Weekday"
sub_weekend$day<-"Weekend"
activity_day<-rbind(sub_weekday, sub_weekend)
agg<-aggregate(steps~interval+day, activity_day, mean)
g<-ggplot(agg, aes(interval, steps))
g+geom_point(color="#75192E")+geom_line(color="#1057C2")+facet_grid(day~.)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
