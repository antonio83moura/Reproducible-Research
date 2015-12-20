source("/home/henrique/Cousera/Reproducible Research/set.R")

file.src  <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file.name <- basename(file.src)
file.dst  <- paste(workspace,"Assessment","01-Assessment","01-Data","repdata%2Fdata%2Factivity.zip",sep = "/")

# # Download dataset.
# download.file(
#   url = file.src, 
#   destfile = file.dst,
#   method = "wget" 
# )

# # Unzip dataset.
# unzip(zipfile = file.dst, exdir = dirname(file.dst))

# Read dataset
#dataset.raw     <- read.csv(file = paste(dirname(file.dst),"activity.csv",sep="/"), na.strings=c("NA","NaN", " "))
dataset.raw     <- read.csv(file = paste(dirname(file.dst),"activity.csv",sep="/"))
dataset.raw$date<- as.Date(dataset.raw$date)
dataset.raw$weekday <- wday(dataset.raw$date, label = TRUE, abbr = FALSE)
#datatable.raw <- data.table(dataset.raw)
dataset.filter  <- na.omit(dataset.raw)


summary(dataset.filter)
summary(dataset.raw)

head(dataset.raw)

###

step.total <- aggregate(steps ~ date, data = dataset.raw, sum, na.rm = TRUE)
hist(step.total$steps, main = "Total steps by day", xlab = "day", col = "blue")

mean(step.total$steps)
median(step.total$steps)

step.interval <- aggregate(steps ~ interval, data = dataset.raw, FUN = mean)
plot(step.interval, type = "l")
step.interval$interval[which.max(step.interval$steps)]

###
# MISSING VALUES 

sum(is.na(dataset.raw))


fillNA <- numeric()
for (i in 1:nrow(dataset.raw)) {
  obs <- dataset.raw[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(step.interval, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

dataset.complete <- dataset.raw
dataset.complete$steps <- fillNA

step.total.complete <- aggregate(steps ~ date, data = dataset.complete, sum, na.rm = TRUE)
hist(step.total.complete$steps, main = "Total steps by day", xlab = "day", col = "red")


mean(step.total.complete$steps)
median(step.total.complete$steps)


daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
dataset.complete$daytype <- as.factor(sapply(dataset.complete$date, daytype))

par(mfrow = c(1, 2))
for (type in c("weekend", "weekday")) {
  step.type <- aggregate(steps ~ interval, data = dataset.complete, subset = dataset.complete$daytype == 
                            type, FUN = mean)
  plot(step.type, type = "l", main = type)
}

