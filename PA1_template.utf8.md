---
title: "PA1_template"
author: "Marta Tabares"
date: "25 de marzo de 2018"
output: html_document
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


## 1. Code for reading in the dataset and/or processing the data


```r
#Loading libraries
#------------------------
library("data.table")
library(ggplot2)
#------------------------
  
#Loading and preprocessing the data
setwd("C:/Users/MartaT/Dropbox/EAFIT MS/Curso DATA SCIENCE-Jonhs Hopkins University/Course 5 - Reproducible Research")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'))
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
dataAct <- read.csv("./data/activity.csv")

# Removing NAs from dataAct
dataAct.RemovedNAs <- dataAct[!is.na(dataAct$steps), ]
# Removing any date factors for NA
dataAct.RemovedNAs$date <- factor(dataAct.RemovedNAs$date)
```

## What is mean total number of steps taken per day?

### 2. Histogram of the total number of steps taken each day

```r
TotalofSteps <- as.data.frame(tapply(dataAct.RemovedNAs$steps, as.factor(dataAct.RemovedNAs$date), sum))
names(TotalofSteps) <- c("Steps")
```
### a. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

```r
# FIG1
qplot(TotalofSteps$Steps, geom="histogram", ylab="Number of Days", xlab="Number of Steps", color = "red", binwidth = 1000, main = "Daily Steps")
```

<img src="PA1_template_files/figure-html/PA1_template_Step3-1.png" width="672" />
### 3. Calculate the Mean and median number of steps taken each day

```r
print(mean(TotalofSteps$Steps), row.names = FALSE)
```

```
## [1] 10766.19
```

```r
print(median(TotalofSteps$Steps), row.names = FALSE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### 4. Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanStepsPerInterval <- tapply(dataAct.RemovedNAs$steps, as.factor(dataAct.RemovedNAs$interval), mean)
meanStepsPerInterval <- as.data.frame(meanStepsPerInterval)
meanStepsPerInterval$interval <- rownames(meanStepsPerInterval)
# FIG2
plot(meanStepsPerInterval$interval, meanStepsPerInterval$mean, type = 'l', ylab = "Mean Steps", xlab = "Interval", main = "Average Number of Steps Taken")
```

<img src="PA1_template_files/figure-html/PA1_template_Step5-1.png" width="672" />
### 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
meanStepsPerInterval[meanStepsPerInterval$mean == max(meanStepsPerInterval$mean), ][1]
```

```
##     meanStepsPerInterval
## 835             206.1698
```
### 6. Code to describe and show a strategy for imputing missing data

```r
# total number of missing values in the dataset
nrow(dataAct[is.na(dataAct$steps), ])
```

```
## [1] 2304
```

```r
# For all data points with NA steps, impute the average number of steps for that interval.
meanStepsPerInterval$interval <- as.integer(meanStepsPerInterval$interval)
dataAct.filledIn <- merge(dataAct, meanStepsPerInterval, by = "interval")
dataAct.filledIn$steps[is.na(dataAct.filledIn$steps)] <- dataAct.filledIn$meanStepsPerInterval[is.na(dataAct.filledIn$steps)]
```

### 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
TotalofSteps.filledIn <- as.data.frame(tapply(dataAct.filledIn$steps, as.factor(dataAct.filledIn$date), sum))
names(TotalofSteps.filledIn) <- c("Steps")
# FIG3
qplot(TotalofSteps.filledIn$Steps, geom="histogram", ylab="Number of Days", xlab="Number of Steps", binwidth = 1000, main = "StepsbyDay")
```

<img src="PA1_template_files/figure-html/PA1_template_Step8-1.png" width="672" />

```r
## Calculate the mean and median total number of steps per day:
print(mean(TotalofSteps.filledIn$Steps), row.names = FALSE)
```

```
## [1] 10766.19
```

```r
print(median(TotalofSteps.filledIn$Steps), row.names = FALSE)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
TotalofSteps <- data.table::fread(input = "./data/activity.csv")
TotalofSteps[, date := as.POSIXct(date, format = "%Y-%m-%d")]
TotalofSteps[, `Day of Week`:= weekdays(x = date)]
TotalofSteps[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
TotalofSteps[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
TotalofSteps[, 'weekday or weekend' := as.factor('weekday or weekend')]
head(TotalofSteps, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:    NA 2012-10-01        0       lunes weekday or weekend
##  2:    NA 2012-10-01        5       lunes weekday or weekend
##  3:    NA 2012-10-01       10       lunes weekday or weekend
##  4:    NA 2012-10-01       15       lunes weekday or weekend
##  5:    NA 2012-10-01       20       lunes weekday or weekend
##  6:    NA 2012-10-01       25       lunes weekday or weekend
##  7:    NA 2012-10-01       30       lunes weekday or weekend
##  8:    NA 2012-10-01       35       lunes weekday or weekend
##  9:    NA 2012-10-01       40       lunes weekday or weekend
## 10:    NA 2012-10-01       45       lunes weekday or weekend
```
## Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
TotalofSteps[is.na(steps), "steps"] <- TotalofSteps[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalTotalSteps <- TotalofSteps[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)]
# FIG 4
ggplot(IntervalTotalSteps , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

<img src="PA1_template_files/figure-html/PA1_template_Step10-1.png" width="672" />
