#change to intervals
library(chron)
library(caTools)

traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

nBroad <- nrow(broad)
nTraffic <- nrow(traffic)

#add time_min to every broadcast
broad['time_min'] <- 0
for (index in 1:nBroad) { #nBroad
  time <- broad$time[[index]]
  timeMinute <- 60 * 24 * as.numeric(times(time))
  broad$time_min[[index]] <- timeMinute
}
#add time_min and date to every travel
traffic['time_min'] <- 0
traffic$date_time <- as.character(traffic$date_time)
trafficDateSplitWhole <- strsplit(traffic$date_time, "\\s+")
trafficDateSplitUnlist <- unlist(trafficDateSplitWhole)
traffictime <- trafficDateSplitUnlist[seq(2, length(trafficDateSplitUnlist), 2)]
traffic$date <- trafficDateSplitUnlist[seq(1, length(trafficDateSplitUnlist), 2)]
traffic$time_min <- 60 * 24 * as.numeric(times(traffictime))
trafficDateSplitWhole <-  NULL
trafficDateSplitUnlist <- NULL

#count visits pre-commercial
broad['preVisitors'] <- 0
intervalSize <- 5
start <- Sys.time()
for (index in 1:500) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  extraViews <- 0
  if(broadTime - intervalSize < 0){
    extraViews <- length(which(traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize))
  } else{
    broad$preVisitors[[index]] <- length(which(traffic$date == broadDate & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViews
  }
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}
#count visits post-commercial
broad['postVisitors'] <- 0
start <- Sys.time()
for (index in 1:500) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broad$postVisitors[[index]] <- length(which(traffic$date == broadDate & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize))
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}

mean(broad$postVisitors[1:500] - broad$preVisitors[1:500])
data = cbind(broad$postVisitors[1:500], broad$preVisitors[1:500])
#data = cbind(log(broad$postVisitors[1:500]), log(broad$preVisitors[1:500]))
data <- as.data.frame(data)
colnames(data) <- c("postVisitors", "preVisitors")

data_split = sample.split(data$postVisitors, SplitRatio = 0.8)
train <- subset(data, data_split == TRUE)
test <-subset(data, data_split == FALSE)
plot(data$preVisitors, data$postVisitors)
lines(cbind(0,10000), cbind(0,10000))

model <- lm(postVisitors ~ 0 + preVisitors, data = train) #DataFlair
summary(model)
coefficients(model)
hist(broad$postVisitors[1:500])
hist(broad$preVisitors[1:500])
