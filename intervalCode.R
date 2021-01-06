#change to intervals
library(chron)

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

#count visits pre-commercial
broad['preVisitors'] <- 0
intervalSize <- 10
start <- Sys.time()
for (index in 1:nBroad) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broad$preVisitors[[index]] <- length(which(traffic$date == broadDate & traffic$time_min <= broadTime & traffic$time_min >= broadTime - intervalSize))
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}
#count visits pre-commercial
broad['postVisitors'] <- 0
intervalSize <- 10
start <- Sys.time()
for (index in 1:nBroad) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broad$postVisitors[[index]] <- length(which(traffic$date == broadDate & traffic$time_min >= broadTime & traffic$time_min <= broadTime + intervalSize))
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}
