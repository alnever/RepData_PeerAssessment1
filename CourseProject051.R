## Course Project 05-1
## by A. Neverov

library(dplyr)

## Read data
data <- read.csv("activity.csv")
data <- tbl_df(data) %>%
        mutate(date = as.Date(date)) 


## Total
by_date <- group_by(data, date)
totals <- summarize(by_date, steps = sum(steps))

hist(totals$steps, 
	     xlab = "Steps", 
	     main = "The total number of steps taken each day"
     )

dev.copy(png,"figure/totals-1.png")
dev.off()

## Mean and median
print("Mean and median of the total number of steps taken per day")
print("Mean   = ") 
print(mean(totals$step, na.rm = TRUE))
print("Median = ") 
print(median(totals$step, na.rm = TRUE))

## Time series plot
by_interval <- group_by(data, interval)
avgs <- summarize(by_interval, avg_steps = mean(steps, na.rm = TRUE))
plot(avgs$interval, avgs$avg_steps, type = "l", 
     xlab = "Interval",
     ylab = "Number of steps",
     main = "Agerage number of steps per 5-min interval"
)

dev.copy(png,"figure/time_series_avg.png")
dev.off()

##Max steps are fixed in this interval
print("Interval with max of steps")
print(as.integer(avgs[avgs$avg_steps == max(avgs$avg_steps),"interval"]))

## Missing values
print("Total number of missing values")
print(sum(is.na(data$steps)))


## Fill missing values
medians <- summarize(by_interval, med_steps = median(steps, na.rm = TRUE))

fdata <- full_join(data, medians) 
fdata <- mutate(fdata, steps = ifelse(!is.na(steps), steps, med_steps))
fdata <- fdata[,-4]

## Total of filled data
fby_date <- group_by(fdata, date)
ftotals <- summarize(fby_date, steps = sum(steps, na.rm = TRUE))

hist(ftotals$steps, 
	     xlab = "Steps", 
	     main = "The total number of steps taken each day (without NAs)"
     )

dev.copy(png,"figure/totals-2.png")
dev.off()

## Mean and median (filled data)
print("Mean and median of the total number of steps taken per day")
print("Mean   = ") 
print(mean(ftotals$step, na.rm = TRUE))
print("Median = ") 
print(median(ftotals$step, na.rm = TRUE))

## Mean values comparation
boxplot(log10(totals$steps+1), log10(ftotals$steps+1), xaxt = "n")
abline(h = mean(log10(totals$steps+1), na.rm = TRUE), col = "blue")
abline(h = mean(log10(ftotals$steps+1), na.rm = TRUE), col = "red")
axis(1, 1:2, labels = c("Totals with NAs","Totals without NAs"))

dev.copy(png,"figure/ds_comp-1.png")
dev.off()

## Total value compareation
hist(c(totals$steps, ftotals$step), col = c("blue","red"))

fdata <- mutate(fdata, daytype = as.factor(ifelse((as.POSIXlt(date))$wday %in% c(0,6),"weekend","weekday")))

fby_interval <- group_by(fdata, interval, daytype)
favg <- summarize(fby_interval, steps = mean(steps, na.rm = TRUE))

library(ggplot2)
g <- ggplot(favg, aes(interval, steps))
g <- g + geom_point()
g <- g + geom_path()
g <- g + facet_wrap( ~ daytype)
print(g)

dev.copy(png,"figure/weekdays_comp.png")
dev.off()

	








