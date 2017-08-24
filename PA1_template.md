# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv(unzip("activity.zip"))
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Let's obtain clean step data:


```r
cl_steps <- activity$steps[!is.na(activity$steps)]
```

## What is mean total number of steps taken per day?

1. A histogram of the total number of steps taken each day:


```r
library(ggplot2)
qplot(cl_steps, binwidth = diff(range(cl_steps))/100)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
qplot(cl_steps, binwidth = diff(range(cl_steps))/100)+coord_cartesian(ylim=c(0, 500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

And one without zeros:

```r
qplot(cl_steps, binwidth = diff(range(cl_steps))/100, xlim=c(1, 800))
```

```
## Warning: Removed 11016 rows containing non-finite values (stat_bin).
```

```
## Warning: Removed 1 rows containing missing values (geom_bar).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



```r
mean(activity$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(activity$steps, na.rm = TRUE)
```

```
## [1] 0
```


## What is the average daily activity pattern?


```r
avg <- function(intrv, df) {
  mean(df$steps[df$interval==intrv], na.rm = TRUE)
}

ints <- unique(activity$interval)
stats <- list(intervals=ints, avg_steps=sapply(ints, avg, activity))

plot(stats$intervals, stats$avg_steps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
max_steps <- max(stats$avg_steps)
max_int <- stats$intervals[stats$avg_steps == max_steps]
```
Max count of steps (206.1698113) is achieved on interval #835.


## Imputing missing values

Number of NAs:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Replacing NAs with intervals averages:

```r
restored <- activity
sum(is.na(restored$steps))
```

```
## [1] 2304
```

```r
nas <- is.na(activity$steps)

for(i in 1:length(nas)){
  if(nas[i]){
    restored$steps[i] <- stats$avg_steps[stats$interval == restored$interval[i]]
  }
}

# restored$steps[nas] <- stats$avg_steps[which(stats$interval %in% restored$interval[nas])]

sum(is.na(restored$steps))
```

```
## [1] 0
```

```r
qplot(steps, data = restored, binwidth = diff(range(cl_steps))/100)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
mean(restored$steps)
```

```
## [1] 37.3826
```

```r
median(restored$steps)
```

```
## [1] 0
```



## Are there differences in activity patterns between weekdays and weekends?



```r
is_weekend <- function(date){
  w <- weekdays(date)
  w =="Saturday" || w == "Sunday"
}

restored$wtype <- sapply(restored$date, function(d){
  factor(levels = c("weekday", "weekend"),
            x = if(is_weekend(as.Date(d))){"weekend"} else {'weekday'})
  })

sum(restored$wtype == "weekend")
```

```
## [1] 4608
```

```r
sum(restored$wtype == "weekday")
```

```
## [1] 12960
```

```r
weekends <- restored[restored$wtype == "weekend",]
attr(weekends, 'name') <- "Weekends"
weekdays <- restored[restored$wtype == "weekday",]
attr(weekdays, 'name') <- "Weekdays"

par(mfrow = c(2, 1))

for(df in list(weekdays, weekends)){
  str(df)
  ints <- unique(df$interval)
  stats <- list(intervals=ints, avg_steps=sapply(ints, avg, df))
  print(any(is.na(stats)))
  plot(stats$intervals, stats$avg_steps, type="l", main=attributes(df)$name)
}
```

```
## 'data.frame':	12960 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ wtype   : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  - attr(*, "name")= chr "Weekdays"
## [1] FALSE
```

```
## 'data.frame':	4608 obs. of  4 variables:
##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 6 6 6 6 6 6 6 6 6 6 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ wtype   : Factor w/ 2 levels "weekday","weekend": 2 2 2 2 2 2 2 2 2 2 ...
##  - attr(*, "name")= chr "Weekends"
## [1] FALSE
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
