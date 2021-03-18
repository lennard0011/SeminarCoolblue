# Seminar Coolblue 2021 -- Z Score Algorithm
# @author: Lennard van der Plas, Erik van der Heide, Marjolein de With, Daniel Buijs
# Reference: https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data/54507329#54507329

### =====================================================
###                   PRE-WORK
### =====================================================

options(max.print = 1500)

# Netherlands: Subsetting day of the biggest commercial [2019-04-30 and 21:55:00]
broad = broad[order(broad$gross_rating_point, decreasing=T), ]
dayCommercials = subset(broad, date == "2019-04-30")
dayCommercials = dayCommercials[order(dayCommercials$time_min), ]
dayVisits = subset(visitorsSum, date == "2019-04-30")
dayVisitsWeb = dayVisits$visitsWebNet
dayVisitsApp = dayVisits$visitsAppNet

# Netherlands: calculate the minute in the year of the commercial
broadNet = subset(broad, country == 'Netherlands')
broadNet = broadNet[order(broadNet$date, broadNet$time), ]
broadNet['minute_in_year'] = 0
print(paste0("Num of. Dutch commercials we consider: ", nrow(broadNet)))
for (i in 1:nrow(broadNet)) {
  dayCom = yday(broadNet$date[i])
  dayTime = broadNet$time_min[i]
  broadNet$minute_in_year[i] = ((dayCom - 1) * 1440) + dayTime + 1
}

# Basic Z Score Algorithm (function which does signalling)
  # Lag: lag of moving window (5 = use 5 last obsv)
  # Threshold = the z-score at which the algorithm signals (e.g. 3.5 stdev away)
  # Influence = the influence (between 0 and 1) of new signals on the mean and standard deviation (?)
ThresholdingAlgo = function(y, lag, threshold, influence) {
  signals = rep(0,length(y))
  filteredY = y[0:lag]
  avgFilter = NULL
  stdFilter = NULL
  avgFilter[lag] = mean(y[0:lag], na.rm = T)
  stdFilter[lag] = sd(y[0:lag], na.rm = T)
  for (i in (lag + 1):length(y)){
    if (abs(y[i] - avgFilter[i - 1]) > threshold*stdFilter[i - 1]) {
      if (y[i] > avgFilter[i - 1]) {
        signals[i] = 1;
      } else {
        signals[i] = -1;
      }
      filteredY[i] = influence * y[i] + (1 - influence) * filteredY[i - 1]
    } else {
      signals[i] = 0
      filteredY[i] = y[i]
    }
    avgFilter[i] = mean(filteredY[(i - lag):i], na.rm = T)
    stdFilter[i] = sd(filteredY[(i - lag):i], na.rm = T)
  }
  return(list("signals" = signals,"avgFilter" = avgFilter,"stdFilter" = stdFilter))
}

### ===============================================================
###                  TUNING, RUNNING, PLOTTING
### ===============================================================

# Tuning variables
lag = 20
threshold = 6
influence = 0.75

# Choose data
# Make sure that visitorsSum has length 260640!
y = as.numeric(visitorsSum$visitsWebNet) # Apply on concatenated data
#y = as.numeric(dayVisits$visitsWebNet)    # Only pick one day 

# Run algorithm 
result = ThresholdingAlgo(y,lag,threshold,influence)

# Plot result
par(mfrow = c(2, 1), oma = c(2, 2, 0, 0) + 0.1, mar = c(0, 0, 2, 1) + 0.2)
plot(1:length(y), y, type = "l", xaxt = 'n', ylab = "Visit density", xlab = "Time (minutes)")
#plot(1:length(y), y, type = "l", xaxt = 'n', yaxt = 'n', ann = F)
#abline(v = broad[1, ]$time_min, col = 'grey') # Only for single day
#par(new = T)
axis(side = 1, at = c(1, 121, 241, 361, 481, 601, 721, 841, 961, 1081, 1201, 1321, 1441), 
    labels = c(0, 2, 4, 6, 8, 10 , 12, 14, 16, 18, 20, 22, 24))
lines(1:length(y), result$avgFilter, type = "l", col = "cyan", lwd = 1.8)
lines(1:length(y), result$avgFilter + threshold * result$stdFilter, type = "l", col = "green", lwd = 1.7)
lines(1:length(y), result$avgFilter - threshold * result$stdFilter, type = "l", col = "green", lwd = 1.7)

plot(result$signals, type = "S", col = "red", ylab = "", xlab = "", ylim = c(-1.5, 1.5),lwd = 2, xaxt = 'n', yaxt = 'n')
axis(side = 1, at = c(1, 121, 241, 361, 481, 601, 721, 841, 961, 1081, 1201, 1321, 1441), 
     labels= c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
axis(side = 2, at = c(-1, 0, 1), labels = c(-1, 0, 1))

sum(result$signals == 1)

### ===============================================================
###             ANALYSE THE RESULTS NL (181 days)
### ===============================================================

# Create indicator for commercials on the large line
commercialIndicator = matrix(0, nrow = 1440 * 181)
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
posSpikes = which(result$signals == 1) # Gives TRUE indices
length(posSpikes)
for (i in 1:length(posSpikes)) {
  if ((posSpikes[i] %% 1440) == 1) {
    posSpikes[i] = NA
  }
}
posSpikes = subset(posSpikes, !is.na(posSpikes))
length(posSpikes)
broadNet['usefulWeb'] = 0
for (i in posSpikes) {
  checker = 0
  if (commercialIndicator[i - 1] > 0) {
    ind = match(i - 1, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  } 
  if (commercialIndicator[i - 2] > 0) {
    ind = match(i - 2, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
  if (commercialIndicator[i - 3] > 0) {
    ind = match(i - 3, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
  if (commercialIndicator[i - 4] > 0) {
    ind = match(i - 4, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
  if (commercialIndicator[i - 5] > 0) {
    ind = match(i - 5, broadNet$minute_in_year)
    broadNet$usefulWeb[ind] = 1
  }
}

# Calculate number of false spikes (before double commercial correction)
numFalseSpikes = length(posSpikes) - sum(broadNet$usefulWeb)

print(paste0("Number of explained peaks website: ", sum(broadNet$usefulWeb)))
# Correct for double commercials on the same minute
for (i in 1:(nrow(broadNet)-1)) {
  # If commercial is useful and the next commercial has the same minute_in_year
  if (broadNet$usefulWeb[i] == 1 && broadNet$minute_in_year[i] == broadNet$minute_in_year[i + 1]) {
    broadNet$usefulWeb[i + 1] = 1
  }
}

# Print results
print(paste0("Nr. of positive spikes website: ", length(posSpikes), " (", 
             format((100 * length(posSpikes)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Nr. of negative spikes website: ", sum(result$signals == -1), " (", 
             format((100 * sum(result$signals == - 1)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Nr. of useful commercials website: ", sum(broadNet$usefulWeb), " / ", 
             nrow(broadNet), " (", format((sum(broadNet$usefulWeb)/nrow(broadNet)) * 100, digits = 2 ), "%)"))
print(paste0("Nr. of false spikes website: ", numFalseSpikes, " / ", length(posSpikes), 
             " (", format((numFalseSpikes/length(posSpikes)) * 100, digits = 3), "%)"))

# Only look at commercials in commercial period
posSpikesInCampaignPeriod = as.double(ceiling(posSpikes/1440))
uniqueDatesNetDay = as.double(sort(yday(uniqueDatesNet)))
setDifference = posSpikesInCampaignPeriod[!posSpikesInCampaignPeriod %in% uniqueDatesNetDay]
print(paste0("Nr. of peaks in commercial period: ", (length(posSpikes)-length(setDifference))))
rm(posSpikesInCampaignPeriod); rm(uniqueDatesNetDay)

### ===============================================================
###       RUN IF YOU WANT THE SAME ANALYSIS VOOR APP DATA
### ===============================================================

# Choose data
yApp = as.numeric(visitorsSum$visitsAppNet) # Apply on concatenated data

# Run algorithm 
resultApp = ThresholdingAlgo(yApp, lag, threshold, influence)

# Create indicator for commercials on the large line
commercialIndicatorApp = matrix(0, nrow = 1440 * 181)
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
posSpikesApp = which(resultApp$signals == 1) # gives TRUE indices
length(posSpikesApp)
for (i in 1:length(posSpikesApp)) {
  if ((posSpikesApp[i] %% 1440) == 1) {
    posSpikesApp[i] = NA
  }
}
posSpikesApp = subset(posSpikesApp, !is.na(posSpikesApp))
length(posSpikesApp)
broadNet['usefulApp'] = 0
numFalseSpikesApp = 0
for (i in posSpikesApp) { 
  checker = 0
  if (commercialIndicatorApp[i - 1] > 0) {
    ind = match(i - 1, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  } 
  if (commercialIndicatorApp[i - 2] > 0) {
    ind = match(i - 2, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
  if (commercialIndicatorApp[i - 3] > 0) {
    ind = match(i - 3, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
  if (commercialIndicatorApp[i - 4] > 0) {
    ind = match(i - 4, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
  if (commercialIndicatorApp[i - 5] > 0) {
    ind = match(i, broadNet$minute_in_year)
    broadNet$usefulApp[ind] = 1
  }
}


# Calculate number of false spikes (before double commercial correction)
numFalseSpikesApp = length(posSpikesApp) - sum(broadNet$usefulApp)

print(paste0("Number of explained peaks app: ", sum(broadNet$usefulApp)))
# Correct for double commercials on the same minute
for (i in 1:(nrow(broadNet) - 1)) {
  # If commercial is useful and the next commercial has the same minute_in_year
  if (broadNet$usefulApp[i] == 1 && broadNet$minute_in_year[i] == broadNet$minute_in_year[i + 1]) {
    broadNet$usefulApp[i + 1] = 1
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
print(paste0("Nr. of positive spikes app: ", length(posSpikesApp), " (", 
             format((100*length(posSpikesApp)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Nr. of negative spikes app: ", sum(resultApp$signals == -1), " (", 
             format((100*sum(resultApp$signals == -1)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Num. of useful commercials app: ", sum(broadNet$usefulApp), " / ", 
             nrow(broadNet), " (", format((sum(broadNet$usefulApp)/nrow(broadNet)) * 100, digits = 2), "%)"))
print(paste0("Num of false spikes app: ", numFalseSpikesApp, " / ", length(posSpikesApp), 
             " (", format((numFalseSpikesApp/length(posSpikesApp)) * 100, digits = 3), "%)"))
usefulCommercials = subset(broadNet, usefulWeb == 1)
print(paste0("Num commercials useful web AND app: ", numUsefulWebAndApp, " / ", nrow(usefulCommercials), 
             " (", format((numUsefulWebAndApp/nrow(usefulCommercials)) * 100, digits = 3), "%)"))

## ================================================
##      Analyse best performing commercials
## ================================================

# Subset on useful WEB commercials
usefulCommercials = subset(broadNet, usefulWeb == 1)
#summary(usefulCommercials)
sort(summary(as.factor(usefulCommercials$channel)))
sort(summary(as.factor(usefulCommercials$position_in_break_3option)))
sort(summary(as.factor(usefulCommercials$length_of_spot)))
sort(summary(usefulCommercials$gross_rating_point))
summary(broadNet$gross_rating_point)
sd(broadNet$gross_rating_point)
quantile(broadNet$gross_rating_point, probs = seq(0, 1, 0.01))
sort(summary(as.factor(usefulCommercials$weekdays)))
summary(usefulCommercials$hours)

# Calculate the biggest relative increase after broadcast time
# Max i + 1, i+2, i+3, i+4, i+5
usefulCommercials['relativePeak'] = 0
usefulCommercials['timeTilPeak'] = 0
for (i in 1:nrow(usefulCommercials)) {
  comTime = usefulCommercials$minute_in_year[i]
  comTraf = visitorsSum$visitsWebNet[comTime]
  afterTraf = matrix(0.0, ncol = 5)
  for (j in 1:5) {
    afterTraf[j] = visitorsSum$visitsWebNet[comTime + j]
  }
  usefulCommercials$relativePeak[i] = max(afterTraf) - comTraf
  usefulCommercials$timeTilPeak[i] = match(max(afterTraf), afterTraf)
}
usefulCommercials = usefulCommercials[order(usefulCommercials$relativePeak, decreasing=T), ]
row.names(usefulCommercials) = NULL # Resets row numbers
sum(usefulCommercials$timeTilPeak == 1)

usefulCommercials = cbind(usefulCommercials, 1:nrow(usefulCommercials))

# Commercials with no peak
nonUsefulCommercials = setdiff(broadNet, usefulCommercials[-c( (ncol(broadNet)+1):ncol(usefulCommercials) )])
nonUsefulCommercials = nonUsefulCommercials[order(nonUsefulCommercials$gross_rating_point, decreasing=T), ]
row.names(nonUsefulCommercials) = NULL # Resets row numbers

# Again, calculate the biggest relative increase after broadcast time
# now for the non-peaking commercials
nonUsefulCommercials['relativePeak'] = 0
nonUsefulCommercials['timeTilPeak'] = 0
for (i in 1:nrow(nonUsefulCommercials)) {
  comTime = nonUsefulCommercials$minute_in_year[i]
  comTraf = visitorsSum$visitsWebNet[comTime]
  afterTraf = matrix(0.0, ncol = 5)
  for (j in 1:5) {
    afterTraf[j] = visitorsSum$visitsWebNet[comTime + j]
  }
  nonUsefulCommercials$relativePeak[i] = max(afterTraf) - comTraf
  nonUsefulCommercials$timeTilPeak[i] = match(max(afterTraf), afterTraf)
}
#nonUsefulCommercials = nonUsefulCommercials[order(nonUsefulCommercials$relativePeak, decreasing=T), ]
#row.names(nonUsefulCommercials) = NULL # resets rownrs
#sum(nonUsefulCommercials$timeTilPeak == 1)
#nonUsefulCommercials = cbind(nonUsefulCommercials, 1:nrow(nonUsefulCommercials))

# 50 commercials with higest GRP, but no peak
nonUsefulCommercials50 = subset(nonUsefulCommercials, nonUsefulCommercials$time_min < 1435)
nonUsefulCommercials50 = subset(nonUsefulCommercials50, nonUsefulCommercials50$time_min > 60)
nonUsefulCommercials50 = nonUsefulCommercials50[1:50, ]
row.names(nonUsefulCommercials50) = NULL # Resets row numbers
sort(summary(as.factor(nonUsefulCommercials50$channel)))
sort(summary(as.factor(nonUsefulCommercials50$position_in_break_3option)))
sort(summary(as.factor(nonUsefulCommercials50$length_of_spot)))
sort(summary(nonUsefulCommercials50$gross_rating_point))
summary(broadNet$gross_rating_point)
sd(broadNet$gross_rating_point)
sort(summary(as.factor(nonUsefulCommercials50$weekdays)))
summary(nonUsefulCommercials50$hours)


## ======================================================
## ======================================================
##            The same analysis for Belgium
## ======================================================
## ======================================================

# calculate the minute in the year of the commercial
broadBel = subset(broad, country == 'Belgium')
broadBel = broadBel[order(broadBel$date, broadBel$time), ]
broadBel['minute_in_year'] = 0
print(paste0("Num of. Belgian commercials we consider: ", nrow(broadBel)))
for (i in 1:nrow(broadBel)) {
  dayCom = yday(broadBel$date[i])
  dayTime = broadBel$time_min[i]
  broadBel$minute_in_year[i] = floor(((dayCom - 1) * 1440) + dayTime + 1) # Round down
}

# Basic Z Score Algorithm (function which does signalling)
# Lag: lag of moving window (5 = use 5 last obsv)
# Threshold = the z-score at which the algorithm signals (e.g. 3.5 stdev away)
# Influence = the influence (between 0 and 1) of new signals on the mean and standard deviation (?)
ThresholdingAlgo = function(y, lag, threshold, influence) {
  signals = rep(0,length(y))
  filteredY = y[0:lag]
  avgFilter = NULL
  stdFilter = NULL
  avgFilter[lag] = mean(y[0:lag], na.rm = T)
  stdFilter[lag] = sd(y[0:lag], na.rm = T)
  for (i in (lag+1):length(y)){
    if (abs(y[i] - avgFilter[i - 1]) > threshold*stdFilter[i - 1]) {
      if (y[i] > avgFilter[i - 1]) {
        signals[i] = 1;
      } else {
        signals[i] = -1;
      }
      filteredY[i] = influence*y[i]+(1-influence)*filteredY[i - 1]
    } else {
      signals[i] = 0
      filteredY[i] = y[i]
    }
    avgFilter[i] = mean(filteredY[(i - lag):i], na.rm=T)
    stdFilter[i] = sd(filteredY[(i - lag):i], na.rm=T)
  }
  return(list("signals" = signals, "avgFilter" = avgFilter, "stdFilter" = stdFilter))
}

### ===============================================================
###                  TUNING, RUNNING, PLOTTING
### ===============================================================

# Tuning variables
lag = 20
threshold = 4
influence = 0.75

# Choose data
# Make sure that visitorsSum has length 260640!
y = as.numeric(visitorsSum$visitsWebBel) # apply on concatenated data

# Run algorithm 
resultBel = ThresholdingAlgo(y, lag,threshold, influence)
#resultReversed = ThresholdingAlgoReversed(y,lag,threshold,influence)

# Plot result
par(mfrow = c(2, 1), oma = c(2, 2, 0, 0) + 0.1, mar = c(0, 0, 2, 1) + 0.2)
#plot(1:length(y), y, type = "l", xaxt = 'n', yaxt = 'n', ann = F)
#abline(v = broad[1, ]$time_min, col = 'grey') # Only for single day
#par(new = T)
plot(1:length(y), y, type = "l", ylab = "Visit density", xlab = "Time (minutes)")
lines(1:length(y), resultBel$avgFilter,type = "l",col = "cyan", lwd = 1.5)
lines(1:length(y), resultBel$avgFilter + threshold * result$stdFilter, type = "l", col = "green", lwd = 1.8)
lines(1:length(y), resultBel$avgFilter - threshold * result$stdFilter, type = "l", col = "green", lwd = 1.8)
plot(resultBel$signals, type = "S", col = "red", ylab = "", xlab = "", ylim = c(-1.5, 1.5), lwd = 2)
sum(resultBel$signals == 1)

### ===============================================================
###             ANALYSE THE RESULTS BEL (181 days)
### ===============================================================

# Create indicator for commercials on the large line
commercialIndicatorBel = matrix(0, nrow = 1440 * 181)
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
posSpikesBel = which(resultBel$signals==1) # Gives T indices
length(posSpikesBel)
for (i in 1:length(posSpikesBel)) {
  if ((posSpikesBel[i] %% 1440) == 1) {
    posSpikesBel[i] = NA
  }
}
posSpikesBel = subset(posSpikesBel, !is.na(posSpikesBel))
length(posSpikesBel)
broadBel['usefulWeb'] = 0
for (i in posSpikesBel) {
  checker = 0
  if (commercialIndicatorBel[i - 1] > 0) {
    ind = match(i - 1, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  } 
  if (commercialIndicatorBel[i - 2] > 0) {
    ind = match(i - 2, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
  if (commercialIndicatorBel[i - 3] > 0) {
    ind = match(i - 3, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
  if (commercialIndicatorBel[i - 4] > 0) {
    ind = match(i - 4, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
  if (commercialIndicatorBel[i - 5] > 0) {
    ind = match(i - 5, broadBel$minute_in_year)
    broadBel$usefulWeb[ind] = 1
  }
}

# Calculate number of false spikes (before double commercial correction)
numFalseSpikesBel = length(posSpikesBel) - sum(broadBel$usefulWeb)

print(paste0("Number of explained peaks web: ", sum(broadBel$usefulWeb)))
# Correct for double commercials on the same minute
for (i in 1:(nrow(broadBel)-1)) {
  # If commercial is useful and the next commercial has the same minute_in_year
  if (broadBel$usefulWeb[i] == 1 && broadBel$minute_in_year[i] == broadBel$minute_in_year[i + 1]) {
    broadBel$usefulWeb[i + 1] = 1
  }
}

# Print results
print(paste0("Nr. of positive spikes web: ", length(posSpikesBel), " (", 
             format((100*length(posSpikesBel)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Nr. of negative spikes web: ", sum(resultBel$signals == -1), " (", 
             format((100*sum(resultBel$signals == -1)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Num. of useful commercials web: ", sum(broadBel$usefulWeb), " / ", 
             nrow(broadBel), " (", format((sum(broadBel$usefulWeb)/nrow(broadBel)) * 100, digits = 2), "%)"))
print(paste0("Num of false spikes web: ", numFalseSpikesBel, " / ", length(posSpikesBel), 
             " (", format((numFalseSpikesBel/length(posSpikesBel)) * 100, digits = 3), "%)"))

# Only look at commercials in commercial period
posSpikesInCampaignPeriod = as.double(ceiling(posSpikesBel/1440))
uniqueDatesBelDay = as.double(sort(yday(uniqueDatesBel)))
setDifference = posSpikesInCampaignPeriod[!posSpikesInCampaignPeriod %in% uniqueDatesBelDay]
print(paste0("Num. of peaks in commercial period: ", (length(posSpikesBel)-length(setDifference))))
rm(posSpikesInCampaignPeriod); rm(uniqueDatesBelDay)


### ===============================================================
###       RUN IF YOU WANT THE SAME ANALYSIS VOOR APP DATA
### ===============================================================

# Choose data
yAppBel = as.numeric(visitorsSum$visitsAppBel) # Apply on concatenated data

# Run algorithm 
resultAppBel = ThresholdingAlgo(yAppBel, lag, threshold, influence)

# Create indicator for commercials on the large line
commercialIndicatorAppBel = matrix(0, nrow = 1440 * 181)
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
posSpikesAppBel = which(resultAppBel$signals == 1) # Gives T indices
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
for (i in posSpikesAppBel) {
  checker = 0
  if (commercialIndicatorAppBel[i - 1] > 0) {
    ind = match(i - 1, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  } 
  if (commercialIndicatorAppBel[i - 2] > 0) {
    ind = match(i - 2, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
  if (commercialIndicatorAppBel[i - 3] > 0) {
    ind = match(i - 3, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
  if (commercialIndicatorAppBel[i - 4] > 0) {
    ind = match(i - 4, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
  if (commercialIndicatorAppBel[i - 5] > 0) {
    ind = match(i, broadBel$minute_in_year)
    broadBel$usefulApp[ind] = 1
  }
}

# Calculate number of false spikes (before double commercial correction)
numFalseSpikesAppBel = length(posSpikesAppBel) - sum(broadBel$usefulApp)

# Correct for double commercials on the same minute
for (i in 1:(nrow(broadBel) - 1)) {
  # If commercial is useful and the next commercial has the same minute_in_year
  if (broadBel$usefulApp[i] == 1 && broadBel$minute_in_year[i] == broadBel$minute_in_year[i + 1]) {
    broadBel$usefulApp[i + 1] = 1
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
             format((100*length(posSpikesAppBel)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Nr. of negative spikes app Bel: ", sum(resultAppBel$signals == -1), " (", 
             format((100*sum(resultAppBel$signals == -1)/(1440 * 181)), digits = 2), "%)"))
print(paste0("Num. of useful commercials app Bel: ", sum(broadBel$usefulApp), " / ", 
             nrow(broadBel), " (", format((sum(broadBel$usefulApp)/nrow(broadBel)) * 100, digits = 2), "%)"))
print(paste0("Num of false spikes app Bel: ", numFalseSpikesAppBel, " / ", length(posSpikesAppBel), 
             " (", format((numFalseSpikesAppBel/length(posSpikesAppBel)) * 100, digits = 3), "%)"))
usefulCommercialsBel = subset(broadBel, usefulWeb==1)
print(paste0("Num commercials useful web AND app Bel: ", numUsefulWebAndAppBel, " / ", nrow(usefulCommercialsBel), 
             " (", format((numUsefulWebAndAppBel/nrow(usefulCommercialsBel)) * 100, digits = 3), "%)"))

## ================================================
##      Analyse best performing commercials
## ================================================

# Subset on useful website commercials
usefulCommercialsBel = subset(broadBel, usefulWeb==1)
#summary(usefulCommercialsBel)
sort(summary(as.factor(usefulCommercialsBel$channel)))
sort(summary(as.factor(usefulCommercialsBel$position_in_break_3option)))
sort(summary(usefulCommercialsBel$gross_rating_point))
summary(broadBel$gross_rating_point)
sort(summary(as.factor(usefulCommercialsBel$weekdays)))

# Calculate the biggest relative increase after broadcast time
# Max i + 1, i + 2, i + 3, i + 4, i + 5 (can also be i - 1 vs. i, i + 1, ...)
usefulCommercialsBel['relativePeak'] = 0
usefulCommercialsBel['timeTilPeak'] = 0
for (i in 1:nrow(usefulCommercialsBel)) {
  comTime = usefulCommercialsBel$minute_in_year[i]
  comTraf = visitorsSum$visitsWebBel[comTime]
  afterTraf = matrix(0.0, ncol = 5)
  for (j in 1:5) {
    afterTraf[j] = visitorsSum$visitsWebBel[comTime + j]
  }
  usefulCommercialsBel$relativePeak[i] = max(afterTraf) - comTraf
  usefulCommercialsBel$timeTilPeak[i] = match(max(afterTraf), afterTraf)
}
usefulCommercialsBel = usefulCommercialsBel[order(usefulCommercialsBel$relativePeak, decreasing=T), ]