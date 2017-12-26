---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
#setwd("D:/git_repo/RepData_PeerAssessment1")
library(dplyr); library(lubridate)
```
## Loading and preprocessing the data

```r
act <- read.csv("activity.csv", stringsAsFactors = FALSE)
act <- mutate(act,date =ymd(act$date)) 
```
## What is mean total number of steps taken per day?
* Calculate the total number of steps taken per day
* Make a histogram of the total number of steps taken each day
* Calculate and report the mean and median of the total number of steps taken per day

```r
steps_by_day <- act %>% group_by(date) %>% 
    summarize(tot = sum(steps, na.rm=T))
# there was no instruction to remove the days with 0 steps, so I left these days
# steps_by_day <- filter(steps_by_day, tot != 0)
hist(steps_by_day$tot, breaks = 10, 
     xlab="Steps by Day", main ="Histogram of Steps taken by Day")
```

![](RLobel_RR_Assignment_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean_steps <- as.integer(mean(steps_by_day$tot))
median_steps <- as.integer(median(steps_by_day$tot))
```
The mean steps per day is **9354** and the
median steps per day is **10395**

## What is the average daily activity pattern?
* Make a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_by_int <- act %>% group_by(interval) %>% 
    summarize(int_mean = mean(steps, na.rm=T))
with(steps_by_int, plot(x=interval, y=int_mean, type="l", 
     xlab="5 min interval", ylab="mean steps", main="Steps Time Series Plot"))
```

![](RLobel_RR_Assignment_1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
max_int <- which.max(steps_by_int$int_mean)
int_with_max_mean = steps_by_int$interval[max_int]
max_mean = as.integer(steps_by_int$int_mean[max_int])
```
The interval with max mean steps is the **835**, 
with **206** average steps

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?