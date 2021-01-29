# Z Score Algorithm -- Seminar Coolblue 2020-2021
# @authors: 
# Reference: https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data/54507329#54507329

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

# Tuning variables
lag <- 30
threshold <- 5
influence <- 0

# Run algo with lag = 30, threshold = 5, influence = 0
#y = dayVisits
y = as.numeric(visitorsSum$visitsWebNet)
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

#### Investigate for which commercials there was a positive signal

# calculate the minute in the year of the commercial
broadNet = broadNet[order(broadNet$date, broadNet$time),]
broadNet['minute_in_year'] = 0
print(paste0("Num of. Dutch commercials we consider: ", nrow(broadNet)))
for (i in 1:nrow(broadNet)) {
  dayCom = yday(broadNet$date[i])
  dayTime = broadNet$time_min[i]
  broadNet$minute_in_year[i] = ((dayCom-1)*1440)+dayTime+1
}

# make indicator for commercials on the large line
commercialIndicator = matrix(0, nrow = 1440*181)
for (i in 1:nrow(broadNet)) {
  minuteInYear = broadNet$minute_in_year[i]
  if ( commercialIndicator[minuteInYear] > 0) {
    commercialIndicator[minuteInYear] = commercialIndicator[minuteInYear] + 1
  } else {
    commercialIndicator[minuteInYear] = 1 
  }
}
sum(commercialIndicator)

# Find for which positive spikes there was a commercial before
posSpikes = result$signals
sum(posSpikes==1)
sum(posSpikes==1) / (1440*181)
broadNet['useful'] = 0
for (i in 1:length(posSpikes)) {
  if (posSpikes[i] == 1) {
    if (commercialIndicator[i] > 0) {
      ind = match(i, broadNet$minute_in_year)
      broadNet$useful[ind] = 1
    } 
    if (commercialIndicator[i-1] > 0) {
      ind = match(i-1, broadNet$minute_in_year)
      broadNet$useful[ind] = 1
    }
    if (commercialIndicator[i-2] > 0) {
      ind = match(i-2, broadNet$minute_in_year)
      broadNet$useful[ind] = 1
    }
    if (commercialIndicator[i-3] > 0) {
      ind = match(i-3, broadNet$minute_in_year)
      broadNet$useful[ind] = 1
    }
    if (commercialIndicator[i-4] > 0) {
      ind = match(i-4, broadNet$minute_in_year)
      broadNet$useful[ind] = 1
    }
  }
}
print(paste0("Num. of useful commercials: ", sum(broadNet$useful), " (out of ", nrow(broadNet), " commercials.)"))
