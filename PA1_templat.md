Reproducible Research Course Project 1
Heck
5/3/2019
R Markdown
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see http://rmarkdown.rstudio.com.

When you click the Knit button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

# libraries
library(dplyr)
library(lattice)
## import data
activity <- read.csv("activity.csv",header = TRUE)
** Number of Steps per day **

### calulate steps per day
steps_by_day <- aggregate(steps ~ date, activity, sum)
### create a histagram
hist(steps_by_day$steps, xlab= "Total Number of steps per day", ylab = "Number of Days", main = "Total Number of Steps taken each day")


** Calculate mean and median number of steps taken per day **

mean_raw <- mean(steps_by_day$steps)
median_raw <- median(steps_by_day$steps)
** Average of daily activity pattern **

averStepsbyInt <- aggregate(steps ~ interval, activity, mean)
with(averStepsbyInt, plot(interval, steps, type = "l"))


** 5 minute interval (on average across all the days) with the maximum number of steps **

averStepsbyInt[which.max(averStepsbyInt[,2]), 1]
## [1] 835
** Inputing missing values **

### Total number of missing values in the dataset ###
missingindex <- is.na(activity[,1])
m <- mean(averStepsbyInt$steps)
### creating new dataset with all missing values included in the original dataset ###
activity1 <- activity
activity1[missingindex, 1] <- m
head(activity1)
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
** Creating a histagram with the total number of steps taken each day with missing data filled in **

totalstepsDay1 <- aggregate(steps ~ date, activity1, sum)
hist(totalstepsDay1$steps, xlab = "Total Number of Steps per Day", ylab = "Number of Days", main = "Number of Steps per day after missing values are input")


** Calculating and reporting the mean and median total number of steps taken per day **

mean_afterInput <- mean(totalstepsDay1$steps)
median_afterInput <- median(totalstepsDay1$steps)
** Differences in activity patterns between weekdays and weekends **

activity1$date <- as.Date(activity1$date)
activity2 <- activity1%>%
  mutate(dayType = ifelse(weekdays(activity1$date)== "Saturday" | weekdays(activity1$date)== "Sunday", "Weekend", "Weekday"))
head(activity2)
##     steps       date interval dayType
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
averStepsByDTandInter <- activity2 %>%
  group_by(dayType, interval) %>%
  summarize(averStepsbyDay = sum(steps))

head(averStepsByDTandInter)
## # A tibble: 6 x 3
## # Groups:   dayType [1]
##   dayType interval averStepsbyDay
##   <chr>      <int>          <dbl>
## 1 Weekday        0           315.
## 2 Weekday        5           242.
## 3 Weekday       10           231.
## 4 Weekday       15           232.
## 5 Weekday       20           228.
## 6 Weekday       25           283.
** Create two time series plot of the 5 minute interval and the average number of steps taken across weekdays or weekend days **

with(averStepsByDTandInter,
     xyplot(averStepsbyDay ~ interval | dayType,
            type = "l",
            main = "Total Number of Steps within Intervals by dayType",
            xlab = "Daily Intervals",
            ylab = "Average Number of Steps"))
