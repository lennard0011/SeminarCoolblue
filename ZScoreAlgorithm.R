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
broadNet = subset(broad, country == 'Netherlands')
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
threshold = 6
influence = 0.8

# Settings from progress report: lag=30, threshold=6, influence=0.8

# Choose data
# !make sure that visitorsSum has length 260640!
y = as.numeric(visitorsSum$visitsWebNet) # apply on concatenated data
### y = as.numeric(dayVisits$visitsWebNet)    # only pick one day 

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
posSpikes = which(result$signals==1) # gives TRUE indices
broadNet['usefulWeb'] = 0
for (i in posSpikes) { # TODO make more efficient
  checker = 0
  if (commercialIndicator[i-1] > 0) {
    ind = match(i-1, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  } 
  if (commercialIndicator[i-2] > 0) {
    ind = match(i-2, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
  if (commercialIndicator[i-3] > 0) {
    ind = match(i-3, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
  if (commercialIndicator[i-4] > 0) {
    ind = match(i-4, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
  if (commercialIndicator[i-5] > 0) {
    ind = match(i-5, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
}

# Calculate number of false spikes (before double commercial correction)
numFalseSpikes = length(posSpikes) - sum(broadNet$usefulWeb)

# Correct for double commercials on the same minute
for (i in 1:(nrow(broadNet)-1)) {
  # if commercial is useful and the next commercial has the same minute_in_year
  if (broadNet$usefulWeb[i] == 1 && broadNet$minute_in_year[i] == broadNet$minute_in_year[i+1]) {
    broadNet$usefulWeb[i+1] = 1
  }
}

# Print results
print(paste0("Nr. of positive spikes web: ", sum(result$signals==1), " (", 
             format((100*sum(result$signals==1)/(1440*181)), digits=2), "%)"))
print(paste0("Nr. of negative spikes web: ", sum(result$signals==-1), " (", 
             format((100*sum(result$signals==-1)/(1440*181)), digits=2), "%)"))
print(paste0("Num. of useful commercials web: ", sum(broadNet$usefulWeb), " / ", 
             nrow(broadNet), " (", format((sum(broadNet$usefulWeb)/nrow(broadNet))*100, digits=2), "%)"))
print(paste0("Num of false spikes web: ", numFalseSpikes, " / ", length(posSpikes), 
             " (", format((numFalseSpikes/length(posSpikes))*100, digits=3), "%)"))

### ===============================================================
###       RUN IF YOU WANT THE SAME ANALYSIS VOOR APP DATA
### ===============================================================

# Choose data
yApp = as.numeric(visitorsSum$visitsAppNet) # apply on concatenated data

# Run algorithm 
resultApp = ThresholdingAlgo(yApp,lag,threshold,influence)

# Create indicator for commercials on the large line
commercialIndicatorApp = matrix(0, nrow = 1440*181)
for (i in 1:nrow(broadNet)) {
  minuteInYear = broadNet$minute_in_year[i]
  if (commercialIndicatorApp[minuteInYear] > 0) {
    commercialIndicatorApp[minuteInYear] = commercialIndicatorApp[minuteInYear] + 1
  } else {
    commercialIndicatorApp[minuteInYear] = 1 
  }
}
sum(commercialIndicatorApp)

# Find for which positive spikes there was a commercial before
posSpikesApp = which(resultApp$signals==1) # gives TRUE indices
broadNet['usefulApp'] = 0
numFalseSpikesApp = 0
for (i in posSpikesApp) { # TODO make more efficient
  checker = 0
  if (commercialIndicatorApp[i-1] > 0) {
    ind = match(i-1, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  } 
  if (commercialIndicatorApp[i-2] > 0) {
    ind = match(i-2, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
  if (commercialIndicatorApp[i-3] > 0) {
    ind = match(i-3, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
  if (commercialIndicatorApp[i-4] > 0) {
    ind = match(i-4, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
  if (commercialIndicatorApp[i-5] > 0) {
    ind = match(i-5, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
}

# Calculate how many useful Web commercials also show a spike in App data
numUsefulWebAndApp = 0
for (i in 1:nrow(broadNet)) {
  if (broadNet$usefulWeb[i] == 1 && broadNet$usefulApp[i] == 1) {
    numUsefulWebAndApp = numUsefulWebAndApp + 1
  }
}

# Calculate number of false spikes (before double commercial correction)
numFalseSpikesApp = length(posSpikesApp) - sum(broadNet$usefulApp)

# Correct for double commercials on the same minute
for (i in 1:(nrow(broadNet)-1)) {
  # if commercial is useful and the next commercial has the same minute_in_year
  if (broadNet$usefulApp[i] == 1 && broadNet$minute_in_year[i] == broadNet$minute_in_year[i+1]) {
    broadNet$usefulApp[i+1] = 1
  }
}

# Calculate how many useful Web commercials also show a spike in App data
numUsefulWebAndApp = 0
for (i in 1:nrow(broadNet)) {
  if (broadNet$usefulWeb[i] == 1 && broadNet$usefulApp[i] == 1) {
    numUsefulWebAndApp = numUsefulWebAndApp + 1
  }
}

# Print results
print(paste0("Nr. of positive spikes app: ", sum(resultApp$signals==1), " (", 
             format((100*sum(resultApp$signals==1)/(1440*181)), digits=2), "%)"))
print(paste0("Nr. of negative spikes app: ", sum(resultApp$signals==-1), " (", 
             format((100*sum(resultApp$signals==-1)/(1440*181)), digits=2), "%)"))
print(paste0("Num. of useful commercials app: ", sum(broadNet$usefulApp), " / ", 
             nrow(broadNet), " (", format((sum(broadNet$usefulApp)/nrow(broadNet))*100, digits=2), "%)"))
print(paste0("Num of false spikes app: ", numFalseSpikesApp, " / ", length(posSpikesApp), 
             " (", format((numFalseSpikesApp/length(posSpikesApp))*100, digits=3), "%)"))
usefulCommercials = subset(broadNet, usefulWeb==1)
print(paste0("Num commercials useful web AND app: ", numUsefulWebAndApp, " / ", nrow(usefulCommercials), 
             " (", format((numUsefulWebAndApp/nrow(usefulCommercials))*100, digits=3), "%)"))

## ================================================
##      Analyse best performing commercials
## ================================================

# Subset on useful WEB commercials
usefulCommercials = subset(broadNet, usefulWeb==1)
#summary(usefulCommercials)
sort(summary(as.factor(usefulCommercials$channel)))
sort(summary(as.factor(usefulCommercials$position_in_break_3option)))
sort(summary(as.factor(usefulCommercials$length_of_spot)))
sort(summary(usefulCommercials$gross_rating_point))
summary(broadNet$gross_rating_point)
#quantile(broadNet$gross_rating_point, probs=seq(0,1,0.01))
sort(summary(as.factor(usefulCommercials$weekdays)))

# Calculate the biggest relative increase after broadcast time
# max i+1, i+2, i+3, i+4, i+5 (can also be i-1 vs. i, i+1, ...)
usefulCommercials['relativePeak'] = 0
for (i in 1:nrow(usefulCommercials)) {
  comTime = usefulCommercials$minute_in_year[i]
  comTraf = visitorsSum$visitsWebNet[comTime]
  afterTraf = matrix(0.0, ncol=5)
  for (j in 1:5) {
    afterTraf[j] = visitorsSum$visitsWebNet[comTime+j]
  }
  usefulCommercials$relativePeak[i] = max(afterTraf) - comTraf
}
usefulCommercials = usefulCommercials[order(usefulCommercials$relativePeak, decreasing=T),]

# TODO: er zijn nog 2 commercials die negatief zijn