library(ggplot2)

## Read and prepocess data
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
data$daytype  <- factor(ifelse(as.POSIXlt(data$date)$wday %in% c(0,6),"weekend","weekday"))

## Calculate total values per day
totals <- aggregate(data$steps, by = list(data$date), FUN = sum)
names(totals) <- c("date","steps")

## Calculate number of NAs
sum(is.na(totals$steps))
mean(is.na(totals$steps))

## Calculate mean and median values of totals
mean(totals$steps, na.rm =TRUE)
median(totals$steps, na.rm =TRUE)

## Plot a histogram
g <- ggplot(totals, aes(steps))
g <- g + geom_histogram(binwidth = 1000)
g <- g + labs(title = "Total number of steps taken each day")
print(g)

## Calculate average values per intervals
avgs <- aggregate(data$steps, by = list(data$interval), FUN = mean, na.rm = TRUE) 
names(avgs) <- c("interval", "steps")
avgs$interval <- as.numeric(paste(avgs$interval))

## Make time series plot
g <- ggplot(avgs, aes(interval, steps))
g <- g + geom_point(col = "blue", alpha = .5, size = 1)
g <- g + geom_path()
g <- g + scale_x_continuous(breaks = round(seq(min(avgs$interval), max(avgs$interval), by = 200),1))
g <- g + labs(title = "The average daily activity pattern")
g <- g + xlab("Intervals")
g <- g + ylab("Average number of steps")
print(g)

## Calculate the interval having the maximum average value
as.integer(avgs[avgs$steps == max(avgs$steps),"interval"])

## Fill NAs with mean valies per interval

fdata <- data
fdata$steps <- as.numeric(fdata$steps)
fdata <- merge(fdata, avgs, by = "interval")
fdata <- transform(fdata, steps.x = ifelse(!is.na(steps.x),steps.x,steps.y))
fdata <- fdata[,1:4]
names(fdata)[2] <- "steps"

## Calculate new totals
ftotals <- aggregate(fdata$steps, by = list(fdata$date), FUN = sum)
names(ftotals) <- c("date","steps")
mean(is.na(ftotals$steps))

## Plot histograms to compare the total values

### Merge data sets with and without NAs and mark the observations
tmp1 <- totals
tmp1$source <- "With NAs"
tmp2 <- ftotals
tmp2$source <- "Without NAs"
tmp <- rbind(tmp1, tmp2)
tmp$source <- factor(tmp$source)

### Plot
g <- ggplot(tmp, aes(steps))
g <- g + geom_histogram(binwidth = 1000, aes(color = source, fill = source))
g <- g + facet_grid(~source)
g <- g + labs(title = "Total number of steps taken each day")
print(g)


## Calculate new mean and median values
# Summary for old dataset (with NAs)
summary(totals$steps)
# Summary for new dataset (without NAs)
summary(ftotals$steps)

## Create dataset for activity pattern analisys for weekdays and weekends
favgs <- aggregate(fdata$steps, by = list(fdata$interval, fdata$daytype), FUN = mean, na.rm = TRUE)  
names(favgs) <- c("interval","daytype","steps")
favgs$interval <- as.integer(favgs$interval)

## Plot faceted time series for activity patterns
g <- ggplot(favgs, aes(interval, steps))
g <- g + geom_point()
g <- g + geom_path()
g <- g + geom_smooth()
g <- g + facet_wrap( ~ daytype)
g <- g + labs(title = "Activity patterns for weekdays and weekends")
print(g)



