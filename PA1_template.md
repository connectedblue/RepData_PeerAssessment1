# Reproducible Research: Peer Assessment 1

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

The raw data is contained in a zip file which is unpacked and read into a variable called **steps**. The date column is converted into a date object to allow for easier processing.


```r
unzip("./activity.zip")
steps<-read.csv("./activity.csv", header = TRUE)
steps$date <- as.Date(steps$date)
head(steps)
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

## Number of Daily steps 

The **steps** dataframe is summarised by grouping the time periods for each day and summing the total number of steps.  At this stage, the data contains missing values which are ignored for now.  This will be addressed later in the paper.  

The resulting **daily_steps** variable can them be plotted as a histogram which gives a sense of how the daily total varies over the two months.

```r
daily_steps <- steps %>% group_by(date) %>%
                         summarise(daily_steps=sum(steps, na.rm=TRUE))
ggplot(daily_steps,aes(x=date,y=daily_steps))+
                   geom_bar(stat="identity") +
                   ylab("Daily Steps") + xlab("Date")
```

![](PA1_template_files/figure-html/dailysteps-1.png)

## What is mean total number of steps taken per day?

The **daily_steps** data frame can be used to calculate the mean number of steps per day as follows: 

```r
daily_mean_steps <- mean(daily_steps$daily_steps)
daily_median_steps <- median(daily_steps$daily_steps)
```

This gives a daily mean of 9354.2295082 steps taken.  The daily median is 10395.


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
