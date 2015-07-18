# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

load data in unzipped activity.csv file

```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Calculate total steps per day ignoring intervals with missing data (na) and plot histogram

```r
s1 <- split(data, data$date)
Steps_Per_Day <- sapply(s1, function(x) sum(x[, "steps"], na.rm=T))
hist(Steps_Per_Day)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Calculate and display mean total steps per day

```r
print(mean(Steps_Per_Day))
```

```
## [1] 9354.23
```

Calculate and display median total steps per day

```r
print(median(Steps_Per_Day))
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Calculate and plot Mean_Steps_Per_Interval

```r
s2 <- split(data, data$interval)
Mean_Steps_Per_Interval <- sapply(s2, function(x) mean(x[, "steps"], na.rm=T))
plot(Mean_Steps_Per_Interval, xlab = "Interval # (Each interval is 5 minutes long)")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Determine the interval which, on average across all days in the dataset, contains the maximal number of steps. Display the interval and then the average number of steps in that interval.

```r
sorted <- sort(Mean_Steps_Per_Interval, decreasing=T)
print(sorted[1])
```

```
##      835 
## 206.1698
```
The maximum mean steps per interval occurred in interval 835 and was 206.1698 steps.

## Imputing missing values

Determine and display the total number of intervals where data on steps is missing

```r
y <- sum(is.na(data$steps))
print(y)
```

```
## [1] 2304
```

Fill in all missing values for steps with the average number of steps in that interval across all days

```r
#Make sure that the length of Mean_Steps_Per_Interval and data$steps are the same
y <- length(Mean_Steps_Per_Interval)
x <- length(data$steps)
print(y)
```

```
## [1] 288
```

```r
print(x)
```

```
## [1] 17568
```

```r
z<-(x/y)
ReplacementValues <- vector()
for(i in 1:z){
ReplacementValues <- c(ReplacementValues, Mean_Steps_Per_Interval)       
}
print(head(ReplacementValues))
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

```r
print(tail(ReplacementValues))
```

```
##      2330      2335      2340      2345      2350      2355 
## 2.6037736 4.6981132 3.3018868 0.6415094 0.2264151 1.0754717
```

```r
filleddata <- data
for(i in 1:x){
if(is.na(filleddata$steps[i]))
        filleddata$steps[i] <- ReplacementValues[i]
}
n <- sum(is.na(filleddata$steps))
print(n)
```

```
## [1] 0
```

```r
print(length(filleddata$steps))
```

```
## [1] 17568
```

Calculate total steps per day after filling in missing data (na) and plot histogram

```r
s1 <- split(filleddata, filleddata$date)
Filled_Steps_Per_Day <- sapply(s1, function(x) sum(x[, "steps"], na.rm=T))
hist(Filled_Steps_Per_Day)
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Calculate and display mean total steps per day

```r
print(mean(Filled_Steps_Per_Day))
```

```
## [1] 10766.19
```

Calculate and display median total steps per day

```r
print(median(Filled_Steps_Per_Day))
```

```
## [1] 10766.19
```




## Are there differences in activity patterns between weekdays and weekends?
