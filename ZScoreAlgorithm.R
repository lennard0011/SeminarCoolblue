# Z Score Algorithm -- Seminar Coolblue 2020-2021
# @authors: 

# Start by testing out the biggest commercial [2019-04-30 21:55:00]
broad = broad[order(broad$gross_rating_point, decreasing=T),]
dayCommercials = subset(broad, date == "2019-04-30")
dayCommercials = dayCommercials[order(dayCommercials$time_min),]
dayVisits = subset(visitorsSum, date == "2019-04-30")
dayVisits = dayVisits$visitsAppNet

# Plot the daily visits
plot(dayVisits, type = 'l', main = "Traffic WebNet 2019-04-30", 
     xlab = "Time (min)", ylab = "Visit densitity")
abline(v = broad[1,]$time_min, col = 'blue')

# lag = lag of moving window (5 = use 5 last obsv)
# threshold = the z-score at which the algorithm signals (e.g. 3.5 stdev away)
# influence = the influence (between 0 and 1) of new signals on the mean and standard deviation (?)
# stationairy

# Function who does signalling
ThresholdingAlgo <- function(y,lag,threshold,influence) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] <- sd(y[0:lag], na.rm=TRUE)
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] <- sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}

# Data
y <- c(1,1,1.1,1,0.9,1,1,1.1,1,0.9,1,1.1,1,1,0.9,1,1,1.1,1,1,1,1,1.1,0.9,1,1.1,1,1,0.9,
       1,1.1,1,1,1.1,1,0.8,0.9,1,1.2,0.9,1,1,1.1,1.2,1,1.5,1,3,2,5,3,2,1,1,1,0.9,1,1,3,
       2.6,4,3,3.2,2,1,1,0.8,4,4,2,2.5,1,1,1)

lag <- 30
threshold <- 5
influence <- 0

# Run algo with lag = 30, threshold = 5, influence = 0
y = dayVisits
result <- ThresholdingAlgo(y,lag,threshold,influence)

# Plot result
par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
plot(1:length(y),y,type="l",ylab="",xlab="")
#for (i in 1:nrow(dayCommercials)) {
#  abline(v = dayCommercials[i,]$time_min, col = 'blue')
#}
lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)

positiveSignals = matrix(NA, sum(result$signals))
ind = 1
for (i in 1:length(result$signals)) {
  if (result$signals[i] == 1) {
    positiveSignals[ind] = i
    ind = ind + 1
  }
}

for (i in 1:length(positiveSignals)) {
  # was er 5 minuten pre een commercial? double loop
}