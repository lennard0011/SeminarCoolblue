# ZScore algorithm for Belgium.

# Z Score Algorithm -- Seminar Coolblue 2020-2021
# @authors: 
# Reference: https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data/54507329#54507329

### =====================================================
###                   PRE-WORK
### =====================================================

options(max.print=1500)

# NL: calculate the minute in the year of the commercial
broadBel = subset(broad, country == 'Belgium')
broadBel = broadBel[order(broadBel$date, broadBel$time),]
broadBel['minute_in_year'] = 0
print(paste0("Num of. Belgian commercials we consider: ", nrow(broadBel)))
for (i in 1:nrow(broadBel)) {
  dayCom = yday(broadBel$date[i])
  dayTime = broadBel$time_min[i]
  broadBel$minute_in_year[i] = floor(((dayCom-1)*1440)+dayTime+1) # round down
}

# Basic Z Score Algorithm (function which does signalling)
# lag = lag of moving window (5 = use 5 last obsv)
# threshold = the z-score at which the algorithm signals (e.g. 3.5 stdev away)
# influence = the influence (between 0 and 1) of new signals on the mean and standard deviation (?)
ThresholdingAlgo = function(y, lag, threshold, influence) {
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
lag = 20
threshold = 6
influence = 0.75

# Settings from progress report: lag=30, threshold=6, influence=0.8

# Choose data
# !make sure that visitorsSum has length 260640!
y = as.numeric(visitorsSum$visitsWebBel) # apply on concatenated data

# Run algorithm 
resultBel = ThresholdingAlgo(y,lag,threshold,influence)
#resultReversed = ThresholdingAlgoReversed(y,lag,threshold,influence)

# Plot result
par(mfrow = c(2,1),oma = c(2,2,0,0)+0.1, mar = c(0,0,2,1)+0.2)
#plot(1:length(y),y,type="l",xaxt='n', yaxt = 'n', ann=FALSE)
#abline(v=broad[1,]$time_min, col = 'grey') #only for single day
#par(new=TRUE)
plot(1:length(y),y,type="l",ylab="Visit density",xlab="Time (minutes)")
lines(1:length(y),resultBel$avgFilter,type="l",col="cyan",lwd=1.5)
lines(1:length(y),resultBel$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=1.8)
lines(1:length(y),resultBel$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=1.8)
plot(resultBel$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)
sum(resultBel$signals==1)

### ===============================================================
###             ANALYISE THE RESULTS BEL (181 days)
### ===============================================================

# Create indicator for commercials on the large line
commercialIndicatorBel = matrix(0, nrow = 1440*181)
for (i in 1:nrow(broadBel)) {
  minuteInYear = broadBel$minute_in_year[i]
  if ( commercialIndicatorBel[minuteInYear] > 0) {
    commercialIndicatorBel[minuteInYear] = commercialIndicatorBel[minuteInYear] + 1
  } else {
    commercialIndicatorBel[minuteInYear] = 1 
  }
}
sum(commercialIndicatorBel)

# Find for which positive spikes there was a commercial before
posSpikesBel = which(resultBel$signals==1) # gives TRUE indices
length(posSpikesBel)
for (i in 1:length(posSpikesBel)) {
  if ((posSpikesBel[i] %% 1440) == 1) {
    posSpikesBel[i] = NA
  }
}
posSpikesBel = subset(posSpikesBel, !is.na(posSpikesBel))
length(posSpikesBel)
broadBel['usefulWeb'] = 0
for (i in posSpikesBel) { # TODO make more efficient
  checker = 0
  if (commercialIndicatorBel[i-1] > 0) {
    ind = match(i-1, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  } 
  if (commercialIndicatorBel[i-2] > 0) {
    ind = match(i-2, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
  if (commercialIndicatorBel[i-3] > 0) {
    ind = match(i-3, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
  if (commercialIndicatorBel[i-4] > 0) {
    ind = match(i-4, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
  if (commercialIndicatorBel[i-5] > 0) {
    ind = match(i-5, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
}

# Calculate number of false spikes (before double commercial correction)
numFalseSpikesBel = length(posSpikesBel) - sum(broadBel$usefulWeb)

# Correct for double commercials on the same minute
for (i in 1:(nrow(broadBel)-1)) {
  # if commercial is useful and the next commercial has the same minute_in_year
  if (broadBel$usefulWeb[i] == 1 && broadBel$minute_in_year[i] == broadBel$minute_in_year[i+1]) {
    broadBel$usefulWeb[i+1] = 1
  }
}

# Print results
print(paste0("Nr. of positive spikes web: ", length(posSpikesBel), " (", 
             format((100*length(posSpikesBel)/(1440*181)), digits=2), "%)"))
print(paste0("Nr. of negative spikes web: ", sum(resultBel$signals==-1), " (", 
             format((100*sum(resultBel$signals==-1)/(1440*181)), digits=2), "%)"))
print(paste0("Num. of useful commercials web: ", sum(broadBel$usefulWeb), " / ", 
             nrow(broadBel), " (", format((sum(broadBel$usefulWeb)/nrow(broadBel))*100, digits=2), "%)"))
print(paste0("Num of false spikes web: ", numFalseSpikesBel, " / ", length(posSpikesBel), 
             " (", format((numFalseSpikesBel/length(posSpikesBel))*100, digits=3), "%)"))

# TODO: what is the percentage within the add period?

### ===============================================================
###       RUN IF YOU WANT THE SAME ANALYSIS VOOR APP DATA
### ===============================================================

# Choose data
yAppBel = as.numeric(visitorsSum$visitsAppBel) # apply on concatenated data

# Run algorithm 
resultAppBel = ThresholdingAlgo(yAppBel,lag,threshold,influence)

# Create indicator for commercials on the large line
commercialIndicatorAppBel = matrix(0, nrow = 1440*181)
for (i in 1:nrow(broadBel)) {
  minuteInYear = broadBel$minute_in_year[i]
  if (commercialIndicatorAppBel[minuteInYear] > 0) {
    commercialIndicatorAppBel[minuteInYear] = commercialIndicatorAppBel[minuteInYear] + 1
  } else {
    commercialIndicatorAppBel[minuteInYear] = 1 
  }
}
sum(commercialIndicatorAppBel)

# Find for which positive spikes there was a commercial before
posSpikesAppBel = which(resultAppBel$signals==1) # gives TRUE indices
length(posSpikesAppBel)
for (i in 1:length(posSpikesAppBel)) {
  if ((posSpikesAppBel[i] %% 1440) == 1) {
    posSpikesAppBel[i] = NA
  }
}
posSpikesAppBel = subset(posSpikesAppBel, !is.na(posSpikesAppBel))
length(posSpikesAppBel)
broadBel['usefulApp'] = 0
numFalseSpikesAppBel = 0
for (i in posSpikesAppBel) { # TODO make more efficient
  checker = 0
  if (commercialIndicatorAppBel[i-1] > 0) {
    ind = match(i-1, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  } 
  if (commercialIndicatorAppBel[i-2] > 0) {
    ind = match(i-2, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
  if (commercialIndicatorAppBel[i-3] > 0) {
    ind = match(i-3, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
  if (commercialIndicatorAppBel[i-4] > 0) {
    ind = match(i-4, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
  if (commercialIndicatorAppBel[i-5] > 0) {
    ind = match(i, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
}


# Calculate number of false spikes (before double commercial correction)
numFalseSpikesAppBel = length(posSpikesAppBel) - sum(broadBel$usefulApp)

# Correct for double commercials on the same minute
for (i in 1:(nrow(broadBel)-1)) {
  # if commercial is useful and the next commercial has the same minute_in_year
  if (broadBel$usefulApp[i] == 1 && broadBel$minute_in_year[i] == broadBel$minute_in_year[i+1]) {
    broadBel$usefulApp[i+1] = 1
  }
}

# Calculate how many useful Web commercials also show a spike in App data
numUsefulWebAndAppBel = 0
for (i in 1:nrow(broadBel)) {
  if (broadBel$usefulWeb[i] == 1 && broadBel$usefulApp[i] == 1) {
    numUsefulWebAndAppBel = numUsefulWebAndAppBel + 1
  }
}

# Print results
print(paste0("Nr. of positive spikes app Bel: ", length(posSpikesAppBel), " (", 
             format((100*length(posSpikesAppBel)/(1440*181)), digits=2), "%)"))
print(paste0("Nr. of negative spikes app Bel: ", sum(resultAppBel$signals==-1), " (", 
             format((100*sum(resultAppBel$signals==-1)/(1440*181)), digits=2), "%)"))
print(paste0("Num. of useful commercials app Bel: ", sum(broadBel$usefulApp), " / ", 
             nrow(broadBel), " (", format((sum(broadBel$usefulApp)/nrow(broadBel))*100, digits=2), "%)"))
print(paste0("Num of false spikes app Bel: ", numFalseSpikesAppBel, " / ", length(posSpikesAppBel), 
             " (", format((numFalseSpikesAppBel/length(posSpikesAppBel))*100, digits=3), "%)"))
usefulCommercialsBel = subset(broadBel, usefulWeb==1)
print(paste0("Num commercials useful web AND app Bel: ", numUsefulWebAndAppBel, " / ", nrow(usefulCommercialsBel), 
             " (", format((numUsefulWebAndAppBel/nrow(usefulCommercialsBel))*100, digits=3), "%)"))

## ================================================
##      Analyse best performing commercials
## ================================================

# Subset on useful WEB commercials
usefulCommercialsBel = subset(broadBel, usefulWeb==1)
#summary(usefulCommercialsBel)
sort(summary(as.factor(usefulCommercialsBel$channel)))
sort(summary(as.factor(usefulCommercialsBel$position_in_break_3option)))
sort(summary(usefulCommercialsBel$gross_rating_point))
summary(broadBel$gross_rating_point)
sort(summary(as.factor(usefulCommercialsBel$weekdays)))

# Calculate the biggest relative increase after broadcast time
# max i+1, i+2, i+3, i+4, i+5 (can also be i-1 vs. i, i+1, ...)
usefulCommercialsBel['relativePeak'] = 0
usefulCommercialsBel['timeTilPeak'] = 0
for (i in 1:nrow(usefulCommercialsBel)) {
  comTime = usefulCommercialsBel$minute_in_year[i]
  comTraf = visitorsSum$visitsWebBel[comTime]
  afterTraf = matrix(0.0, ncol=5)
  for (j in 1:5) {
    afterTraf[j] = visitorsSum$visitsWebBel[comTime+j]
  }
  usefulCommercialsBel$relativePeak[i] = max(afterTraf) - comTraf
  usefulCommercialsBel$timeTilPeak[i] = match(max(afterTraf), afterTraf)
}
usefulCommercialsBel = usefulCommercialsBel[order(usefulCommercialsBel$relativePeak, decreasing=T),]

# TODO calculate the number of overlapping commercials in a 5 minute interval

# TODO what time sloths are commercials broadcast?

# TODO what is the commercial-spike-density if we only look at commercial campaign periods
# (especially important for BE)