---
title: "Reproducible Research: Peer Assessment 1"
author: "Marco Lunardi"
date: "18 October 2014"
output: html_document
---

## Loading and preprocessing data

The following R code unzips and loads the required data; it calls also the libraries required to complete the assignment:


```r
unzip("activity.zip")
data <- read.csv("activity.csv", header = TRUE)
library(plyr)
library(ggplot2)
library(Hmisc)
library(knitr)
```

## What is mean total number of steps taken per day?

The following R code processes data to get an histogram of the total number of steps taken each day (ignoring the missing values), and then plots the histogram that you can see below.


```r
nona <- data[complete.cases(data),]
daystepsum <- ddply(nona, .(date), numcolwise(sum), drop=FALSE)
y <- daystepsum[,2]
hist(y, col="red", 
     main="Histogram of total number of Steps taken each Day",
     xlab="Total number of Steps each Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The following R code calculate the mean and median total number of steps taken per day.


```r
daymean <- mean(daystepsum[,2])
daymedian <- median(daystepsum[,2])
report <- paste("Mean=",daymean,", Median=",daymedian)
report
```

```
## [1] "Mean= 10766.1886792453 , Median= 10765"
```

