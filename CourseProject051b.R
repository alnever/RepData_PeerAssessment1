data <- read.csv("activity.csv")
data$date = as.Date(data$date)
weekend = as.POSIXct(data$date).wday %in% c(0,6)


hist(totals$steps, xlab="Step number", main = "Histogram of Step number")
abline(v = median(totals$steps, na.rm =TRUE), col = "red")
