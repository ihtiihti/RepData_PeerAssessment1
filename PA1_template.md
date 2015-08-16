# Reproducible Research: Peer Assessment 1


```r
## Constants used in these calculations

# The name of the CSV file in the zipped file
CSV_DATA_FILE_NAME <- 'activity.csv'

# Standard Time Interval in Minutes
STANDARD_TIME_INTERVAL <- 5

# The method of the bicubic interpolations
BICUBIC_INTERPOLATION_METHOD <- "natural"

## Libraries used in these calculations
library(lubridate)
library(lattice)
```

## Loading and preprocessing the data

The code below loads data from a CSV file and transforms _date_ defined as string to _Date_ class.


```r
# Load CSV Data
data <- read.csv(CSV_DATA_FILE_NAME)

# Translate strings to Date type
data['date'] <- lapply(data['date'], ymd)
```

## What is mean total number of steps taken per day?

### The Total Number of Steps Taken per Day
The code below shows how the _total number of steps taken per date_ is calculated.


```r
steps.per.day <- with(data, tapply(steps, date, function(x) sum(x, na.rm = TRUE)))
```

### Histogram of the Total Number of Steps Taken Each Day
The code below shows how the histogram of the total number of steps taken for each date is built.

```r
hist(steps.per.day, main = 'Histogram of Steps per Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

### Mean and Median of the total number of steps taken per day

```r
mean.value <- mean(steps.per.day, na.rm = TRUE)
median.value <- median(steps.per.day, na.rm = TRUE)
```

__Mean:__ _9354.2295082_.

__Median:__ _10395_.

## What is the average daily activity pattern?

### Time Series Plot

```r
# Add to each date corresponding interval in minutes to get complete date and time
date.time <- do.call(function(date, interval) date + minutes(interval), data[,c('date', 'interval')])

# Draw a time series plot
plot(
    date.time, 
    data$steps, 
    type = 'l', 
    xlab = 'Date & Time', 
    ylab = 'Steps per 5 mins', 
    main = 'Steps of Dataset ~ Date & Time'
)

# Draw a Mean Value (as a horizontal line)
abline(h = mean(data$steps, na.rm = TRUE), col = 'red')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

### Maximum number of steps within 5-minute intervals
The code below calculates the maximum number of steps within 5-minute intervals, 
on average across all the days in the dataset.

```r
# Calculate a maximum number of steps among all 5-minute intervals (find the first one)
max.steps <- max(data$steps, na.rm = TRUE)

max.step.mask <- data$steps == max.steps & !is.na(data$steps)
max.date     <- data$date[max.step.mask]
max.interval <- data$interval[max.step.mask]
max.date.time.begin <- max.date + minutes(max.interval)
max.date.time.end   <- max.date + minutes(max.interval + STANDARD_TIME_INTERVAL)
```

__Maximum Number of Steps within 5-minute Intervals:__ _806_ 
@ interval [2012-11-27 10:15:00; 2012-11-27 10:20:00]..

## Imputing missing values

### Total Number of Missing Values in the Dataset
The code below calculates the total number of missing values in the dataset.

```r
total.missing.values <- sum(is.na(data$steps))
```

__Total Number of Missing Values in the Dataset:__ _2304_.

### Strategy for filling in all of the missing values in the dataset.
The code below shows the function that is applied into the dataset in order to fill the missed values.
The idea of the function is following:
* if for the whole day there all values are missed -- set values as the mean
* if for the whole day there are some values, then find an interpolation polynomial for the present values
    and then calculate the values for the missed dates.


```r
interpolation <- function(steps, intervals, mean.value) {
    ## The function that finds missed values for intervals of data
    ## Missed values mean the values those have N/A.
    ##
    ## Arguments:
    ##      steps -- data to interpolate (Y-Data)
    ##      intervals -- intervals (X-Data)
    ##      mean.value -- default value is all data have N/A i.e. if interpolation is impossible
    # if all steps filled for a certain day for all its available intervals -- nothing to do
    na.mask <- is.na(steps)
    total.na.amount <- sum(na.mask)
    
    # if there is no missed data for steps (N/A) thus nothing to do -- return steps as they are
    if (total.na.amount == 0) {
        steps
    } else {
        # if all steps have N/A that means that we haven't data to apply interpolation thus return for all steps mean value
        if(total.na.amount == length(intervals)) {
            rep(mean.value, length(intervals))
        } else {
            only.valid.mask <- !na.mask
            only.valid.steps <- steps[only.valid.mask]
            only.valid.intervals <- intervals[only.valid.mask]
            min.interval <- min(intervals)
            max.interval <- max(intervals)
            interpolation.steps <- (max.interval - min.interval) / STANDARD_TIME_INTERVAL
            interpolated.data <- spline(
                x = only.valid.intervals,
                y = only.valid.steps,
                method = BICUBIC_INTERPOLATION_METHOD,
                interpolation.steps,
                xmin = min.interval,
                xmax = max.interval)
                steps[!only.valid.mask] <- 
                    interpolated.data[
                        seq(min.interval, max.interval, STANDARD_TIME_INTERVAL) %in% intervals[!only.valid.mask]
                    ]
                steps
        }
    }
}
```

### Dataset with Interpolated Missed Steps

The code below shows how interpolation to missed steps is applied:
* split the _data_ by dates thus we obtains chunks of data per day;
* apply for each chunk of data interpolation thus fill missed values for steps
* aggregate obtained steps into one vector and replace by this vector all old values.


```r
new.data <- data
step.interval.mean.value <- mean(new.data$steps, na.rm = TRUE)
new.data['steps']  <- do.call(
    c,
    lapply(
        split(data, data[['date']]),
        function(data) interpolation(data[['steps']], data[['interval']], step.interval.mean.value)
    )
)
```

### Mean and Median of the Total Number of Steps Taken per Day for New Dataset
The code for calculation of _mean_ and _median_ values for the new dataset.
_Node:_ due to the fact that all data must be filled, there is no _na.rm = TRUE_ passed to all functions used to 
calculate values.


```r
new.steps.per.day <- with(new.data, tapply(steps, date, function(x) sum(x)))

new.mean.value <- mean(new.steps.per.day)
new.median.value <- median(new.steps.per.day)
```

__New Mean:__ _10766.19_.

__New Median:__ _10766.19_.


_Do these values differ from the estimates from the first part of the assignment?_

__Answer:_ yes, these values are _a bit different_ to the previous ones.

_What is the impact of imputing missing data on the estimates of the total daily number of steps?_

__Answer:_ the impact is increasing of __both__ _mean_ and _median_ values; in additinal, both _mean_ and _median_ are become the same valuse.


### Histogram of the Dataset with Interpolated Missed Steps


```r
# Draw a time series plot
plot(
    date.time, 
    new.data$steps, 
    type = 'l', 
    xlab = 'Date & Time', 
    ylab = 'Steps per 5 mins', 
    main = 'Steps of New Dataset ~ Date & Time'
)
abline(h = mean(new.data$steps, na.rm = TRUE), col = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

### The Dataset with Factor Variable: Weekday/Weekend

```r
weeekday.weekand.factor <- factor(c('Weekdays', 'Weekends'))

weekday.weekend.match <- function(date) {
    if (weekdays(date, FALSE) %in% c('Saturday', 'Sunday')) {
        'weekend'
    } else {
        'weekday'
    }
}

week.days <- c()
for(date in as.list(data$date)) {
    week.days <- c(week.days, weekday.weekend.match(date))
}

new.week.data <- cbind(
    date.time,
    new.data['steps'],
    week.days
)
```

### Panel Plot of the Dataset with Factor Weekday/Weekend Factor Variable

```r
# Draw Panel Plot by using xlattice

xyplot(
    steps ~ date.time | week.days,
    data = new.week.data,
    type = "l",
    lty = 1,
    lwd = 1,
    col.line = c(rep("black", 2)),
    layout = c(1, 2),
    xlab = 'Date & Time',
    ylab = 'Step per 5-minute Interval'
)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
