---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Peer-graded Assignment: Course Project 1



## Code to set the global options

```r
opts_chunk$set(echo=TRUE, results = "asis")
```

## Code for reading in the dataset and/or processing the data

```r
if(!file.exists("activity.csv")) {unzip("activity.zip")}
df_activity <- read.csv("activity.csv")
df_activity <- transform(df_activity, date = as.Date(date))
```


## What is mean total number of steps taken per day?

```r
# png("Figure1_steps_per_day.png")
par(mar=c(4,4,2,1))
df_activity_daily_agg <- df_activity %>% group_by(date) %>% 
        summarise(total=sum(steps, na.rm=TRUE), mean=mean(steps, na.rm=TRUE), median = median(steps, na.rm = TRUE))
g <- ggplot(df_activity_daily_agg, aes(x=total))
g + geom_histogram(binwidth = 2000)
```

![](PA1_template_files/figure-html/stepsperday-1.png)<!-- -->

```r
# dev.off()
```

## Mean and median number of the steps taken each day

```r
# png("Figure2_mean_median_steps_per_day.png")
par(mfrow = c(1,2), mar=c(4,4,2,1))
with(df_activity_daily_agg, plot(date, mean, ylab = "Steps per day (Mean)"))
title(main="Average steps per day")
with(df_activity_daily_agg, plot(date, median, ylab = "Steps per day (Median)"))
title(main="Median steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# dev.off()
```

## Time series plot of the average number of steps taken

```r
# png("Figure3_timeseries_avgperday.png")
par(mfrow = c(1,1))
df_activity_5min_agg <- df_activity %>% group_by(interval) %>% 
        summarise(total=sum(steps, na.rm=TRUE), Average = mean(steps, na.rm = TRUE))
with(df_activity_5min_agg, plot(interval, Average, type='l'))
```

![](PA1_template_files/figure-html/timeseries1-1.png)<!-- -->

```r
# dev.off()
```

## The 5-minute interval that, on average, contains the maximum number of steps

```r
index <- which(df_activity_5min_agg$Average == max(df_activity_5min_agg$Average))
interval_max <- df_activity_5min_agg[index, ]$interval
```
The five-minute interval that, on average, contains the maximum number of steps is: 835

## Code to describe and show a strategy for imputing missing data

```r
df_impute <- df_activity %>% group_by(interval) %>% 
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>% ungroup()
```


## Histogram of the total number of steps taken each day after missing values are imputed

```r
# png("Figure4_after_impute.png")
par(mar=c(4,4,2,1))
df_impute_agg <- df_impute %>% group_by(date) %>% 
        summarise(total=sum(steps, na.rm=TRUE), mean=mean(steps, na.rm=TRUE), median = median(steps, na.rm = TRUE))

g <- ggplot(df_impute_agg, aes(x=total))
g + geom_histogram(binwidth = 2000)
```

![](PA1_template_files/figure-html/imputehist-1.png)<!-- -->

```r
# dev.off()
```

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
# png("Figure5_weekend_weekday.png")
par(mar=c(4,4,2,1))
df_impute_wd <- df_impute %>% mutate(day = weekdays(date)) %>%
        mutate(weekend = ifelse(day %in% c("Saturday", "Sunday"), "weekend", "weekday"))

g <- ggplot(df_impute_wd, aes(interval, steps))
g+geom_line(stat = "summary",fun.y="mean", lwd=1)+facet_wrap(~weekend)+
        labs(y="Average number of steps taken")
```

![](PA1_template_files/figure-html/weekactivity-1.png)<!-- -->

```r
# dev.off()
```
