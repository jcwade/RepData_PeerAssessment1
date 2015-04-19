library(dplyr)
library(lubridate)

## Loading and preprocessing the data

load.data <- function() {
        con <- unz("activity.zip", "activity.csv")
        d <- tbl_df(read.csv(con, stringsAsFactors = FALSE))
        d$date <- ymd(d$date)
        d
}

data <- load.data()

## What is mean total number of steps taken per day?

# 1. Total steps per day.
total_steps_per_day <-
        data %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps))

# 2. Histogram of total steps per day.
hist_total_steps_per_day <- function() {
        hist(total_steps_per_day$total_steps,
             main = "Frequency of steps per day",
             xlab = "Steps")
}

# 3. Mean and median of total steps per day.
mean_total_steps_per_day <-
        mean(total_steps_per_day$total_steps, na.rm = TRUE)

median_total_steps_per_day <-
        median(total_steps_per_day$total_steps, na.rm = TRUE)

## What is the average daily activity pattern?

# 1. Time series plot of average steps per interval.
mean_steps_per_interval <-
        data %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(interval_mean = mean(steps, na.rm = TRUE))

plot_mean_steps_per_interval <- function() {
        plot(mean_steps_per_interval,
             type = "l",
             main = "Average steps per interval",
             xlab = "Interval",
             ylab = "Steps")
}

# 2. On average, which interval has the most number of steps?
most_mean_steps_per_interval <-
        which.max(mean_steps_per_interval$interval_mean) %>%
        mean_steps_per_interval$interval[.]

## Imputing missing values

# 1. Total NA.
total_na <- sum(!complete.cases(data))

# 2. Figure out a way to fill in missing values.
# 3. Fill in missing values.
imputed_steps <-
        data %>%
        inner_join(mean_steps_per_interval, by = "interval") %>%
        mutate(steps = ifelse(is.na(steps), interval_mean, steps))

# 4a. Make a histogram of the total number of steps taken each day.
total_imputed_steps_per_day <-
        imputed_steps %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps))

hist_total_imputed_steps_per_day <- function() {
        hist(total_imputed_steps_per_day$total_steps,
             main = "Frequency of steps per day (imputed data)",
             xlab = "Steps")
}

# 4b. Report mean and median values.
mean_total_imputed_steps_per_day <-
        mean(total_imputed_steps_per_day$total_steps)

median_total_imputed_steps_per_day <-
        median(total_imputed_steps_per_day$total_steps)

# 4c. Do these differ from before?

# The mean is necessarily the same, modulo floating point inaccuracies.

# The median is not the same.

## Are there differences in activity patterns between weekdays and weekends?

day_type <-function(date) {
        labs <- ifelse(weekdays(date) %in% c("Saturday", "Sunday"),
                       "weekend", "weekday")
        factor(labs)
}

steps_per_day_type <-
        imputed_steps %>%
        mutate(day_type = day_type(date))

total_steps_per_day_type <-
        steps_per_day_type %>%
        group_by(interval, day_type) %>%
        summarize(total_steps = sum(steps)) %>%
        arrange(interval, day_type)

plot_day_type_steps <- function() {
        xyplot(total_steps ~ interval | day_type,
               data = total_steps_per_day_type,
               type = "l",
               layout = c(1, 2),
               main = "Steps per interval: weekend vs weekdays",
               xlab = "Interval",
               ylab = "Number of steps")
}
