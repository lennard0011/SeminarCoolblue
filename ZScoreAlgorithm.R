# Z Score Algorithm -- Seminar Coolblue 2020-2021
# @authors: 
# Reference: https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data/54507329#54507329

### =====================================================
###                   PRE-WORK
### =====================================================

# NL: Subsetting day of the biggest commercial [2019-04-30 21:55:00]
broad = broad[order(broad$gross_rating_point, decreasing=T),]
dayCommercials = subset(broad, date == "2019-04-30")
dayCommercials = dayCommercials[order(dayCommercials$time_min),]
dayVisits = subset(visitorsSum, date == "2019-04-30")
dayVisitsWeb = dayVisits$visitsWebNet
dayVisitsApp = dayVisits$visitsAppNet

# NL: calculate the minute in the year of the commercial
broadNet = broadNet[order(broadNet$date, broadNet$time),]
broadNet['minute_in_year'] = 0
print(paste0("Num of. Dutch commercials we consider: ", nrow(broadNet)))
for (i in 1:nrow(broadNet)) {
  dayCom = yday(broadNet$date[i])
  dayTime = broadNet$time_min[i]
  broadNet$minute_in_year[i] = ((dayCom-1)*1440)+dayTime+1
}

# BE: TODO

# Basic Z Score Algorithm (function which does signalling)
  # lag = lag of moving window (5 = use 5 last obsv)
  # threshold = the z-score at which the algorithm signals (e.g. 3.5 stdev away)
  # influence = the influence (between 0 and 1) of new signals on the mean and standard deviation (?)
ThresholdingAlgo = function(y,lag,threshold,influence) {
  signals = rep(0,length(y))
  filteredY = y[0:lag]
  avgFilter = NULL
  stdFilter = NULL
  avgFilter[lag] = mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] = sd(y[0:lag], na.rm=TRUE)
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] = 1;
      } else {
        signals[i] = -1;
      }
      filteredY[i] = influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] = 0
      filteredY[i] = y[i]
    }
    avgFilter[i] = mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] = sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}

### ===============================================================
###                  TUNING, RUNNING, PLOTTING
### ===============================================================

# Tuning variables
lag = 30
threshold = 5
influence = 0

# Choose data
#y = as.numeric(dayVisits$visitsWebNet)    # only pick one day 
# !make sure that visitorsSum has length 260640!
y = as.numeric(visitorsSum$visitsWebNet) # apply on concatenated data

# Run algorithm 
result = ThresholdingAlgo(y,lag,threshold,influence)

# Plot result
par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
plot(1:length(y),y,type="l",ylab="",xlab="")
#abline(v = broad[1,]$time_min, col = 'blue') #only for single day
lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)

### ===============================================================
###             ANALYISE THE RESULTS NL (181 days)
### ===============================================================

# Signal analysis
print(paste0("Nr. of positive spikes: ", sum(result$signals==1)))
print(paste0("Percentage of positive spikes: ", sum(result$signals==1)/(1440*181)))
print(paste0("Nr. of negative spikes: ", sum(result$signals==-1)))
print(paste0("Percentage of negative spikes: ", sum(result$signals==-1)/(1440*181)))

# Create indicator for commercials on the large line
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
posSpikes = (result$signals==1)
broadNet['useful'] = 0
for (i in 5:length(posSpikes)) {
  # look 5 minutes prior
  if (posSpikes[i] == TRUE) {
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
# Correct for double commercials on the same minute
for (i in 1:(nrow(broadNet)-1)) {
  # if commercial is useful and the next commercial has the same minute_in_year
  if (broadNet$useful[i] == 1 && broadNet$minute_in_year[i] == broadNet$minute_in_year[i+1]) {
    broadNet$useful[i+1] = 1
  }
}
print(paste0("Num. of useful commercials: ", sum(broadNet$useful), " (out of ", nrow(broadNet), " commercials.)"))

# TODO print number of spikes which did not have a commercial in 5 minutes prior

# Subset on useful commercials
usefulCommercials <- subset(broadNet, useful==1)