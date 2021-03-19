# Seminar Coolblue 2021 -- Direct Effects 20 minutes
# @author: Lennard van der Plas, Erik van der Heide, Marjolein de With, Daniel Buijs

# Same set-up as intervalCode, but now with a 20-min window and GRP > 0.5
# Note that we also calculate the results for App Data and Belgium Data, which we
# decided not to include in the report after all.

## ========================================================
##                  Collect interval data
## ========================================================

# Get commercials only in Netherlands, with Belgium as control
broadNet1 = subset(broad, country == "Netherlands")
broadNet1 = broadNet1[order(broadNet1$date), ]

broadNet1['preVisitorsWebNet'] = 0
broadNet1['postVisitorsWebNet'] = 0
broadNet1['preVisitorsAppNet'] = 0
broadNet1['postVisitorsAppNet'] = 0

broadNet1['preVisitorsWebBel'] = 0
broadNet1['postVisitorsWebBel'] = 0
broadNet1['preVisitorsAppBel'] = 0
broadNet1['postVisitorsAppBel'] = 0

# Remove commercials in Belgium sales period (assumed whole January)
broadNet1 = subset(broadNet1, yday(date) > 31)

# Remove commercials where Belgium has a commercial as well
belAdDates = sort(unique(subset(broad, country == "Belgium")$date))
for (i in 1:nrow(broadNet1)) {
  for (j in 1:length(belAdDates)) {
    if (broadNet1[i, ]$date == belAdDates[j]) {
      broadNet1[i, ]$channel = NA
    }
  }
}
broadNet1 = subset(broadNet1, !(is.na(channel)))
print(paste0("Number of commercials: ", nrow(broadNet1)))

# Delete lowest GRP ratios
broadNet1 = broadNet1[order(broad$gross_rating_point, decreasing = T), ]
broadNet1 = subset(broadNet1, gross_rating_point > 0.5)
print(paste0("Number of commercials: ", nrow(broadNet1)))

# Delete observations around midnight
broadNet1NoMidnight = subset(broadNet1, broadNet1$time_min < 1420)
broadNet1NoMidnight = subset(broadNet1NoMidnight, broadNet1NoMidnight$time_min > 20)
broadNet1 = broadNet1NoMidnight
print(paste0("Number of commercials: ", nrow(broadNet1)))

# Calculate pre- and post visitors, Netherlands and Belgium, Website and App (20 min)
intervalSize = 20
start = Sys.time()
for (i in 1:nrow(broadNet1)) {
  broadDate = broadNet1$date[[i]]
  broadTime = broadNet1$time_min[[i]]
  
  # Count extra visitors from day before or day after at midnight
  if (broadTime - intervalSize < 0) {
    # Extra Visitors of day before
    preVisitorsWebNetExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsWebNet)
    preVisitorsAppNetExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsAppNet)
    preVisitorsWebBelExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsWebBel)
    preVisitorsAppBelExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsAppBel)
  } else if (broadTime + intervalSize >= 60 * 24) {
    # Extra Visitors of day after
    postVisitorsWebNetExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsWebNet)
    postVisitorsAppNetExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsAppNet)
    postVisitorsWebBelExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsWebBel)
    postVisitorsAppBelExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsAppBel)
  } else {
    preVisitorsAppExtra = 0; preVisitorsWebExtra = 0; postVisitorsAppExtra = 0; postVisitorsWebExtra = 0; 
  }
  
  broadNet1$preVisitorsWebNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebNet)
  broadNet1$preVisitorsAppNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppNet)
  
  broadNet1$preVisitorsWebBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebBel)
  broadNet1$preVisitorsAppBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppBel)
    
  broadNet1$postVisitorsWebNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebNet)
  broadNet1$postVisitorsAppNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppNet)
  
  broadNet1$postVisitorsWebBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebBel)
  broadNet1$postVisitorsAppBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppBel)
    
  if(i %% 100 == 0) {print(paste(i,Sys.time() - start))}
}

# Overlap dummy
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

broadNetNoMidnight = subset(broadNet, broadNet$time_min < 1420)
broadNetNoMidnight = subset(broadNetNoMidnight, broadNetNoMidnight$time_min > 20)
broadNet = broadNetNoMidnight

broadBelNoMidnight = subset(broadBel, broadBel$time_min < 1420)
broadBelNoMidnight = subset(broadBelNoMidnight, broadBelNoMidnight$time_min > 20)
broadBel = broadBelNoMidnight

intervalSize = 20
iNet = 0
iBel = 0
broad = broad[order(broad$date_time), ]
broadNet = broadNet[order(broadNet$date_time), ]
broadBel = broadBel[order(broadBel$date_time), ]
broad$overlapBefore = 0
broad$overlapAfter = 0
for (i in 1:nrow(broad)){
  if (broad$country[i] == 'Netherlands'){
    iNet = iNet + 1
    datetime = broad$date_time[i]
    datetime = as.POSIXct(datetime)
    timeEarlier = datetime - intervalSize * 60
    timeLater = datetime + intervalSize * 60
    # Interval before
    if (iNet > 1){ # Exclude first Dutch commercial
      if (timeEarlier <= broadNet$date_time[iNet - 1] && broadNet$date_time[iNet - 1] <= datetime){
        broad$overlapBefore[i] = 1
      }
    }
    # Interval after
    if (iNet < nrow(broadNet)){ # Exclude last Dutch commercial
      if (datetime <= broadNet$date_time[iNet + 1] && broadNet$date_time[iNet + 1] <= timeLater){
        broad$overlapAfter[i] = 1
      }
    }
  }
  if (broad$country[i] == 'Belgium'){
    iBel = iBel + 1
    datetime = broad$date_time[i]
    datetime = as.POSIXct(datetime)
    timeEarlier = datetime - intervalSize * 60
    timeLater = datetime + intervalSize * 60
    # Interval before
    if (iBel > 1){
      if (timeEarlier <= broadBel$date_time[iBel - 1] && broadBel$date_time[iBel - 1] <= datetime){
        broad$overlapBefore[i] = 1
      }
    }
    # Interval after
    if (iBel < nrow(broadBel)){
      if (datetime <= broadBel$date_time[iBel + 1] && broadBel$date_time[iBel + 1] <= timeLater){
        broad$overlapAfter[i] = 1
      }
    }
  }
}
broad = broad[order(as.numeric(row.names(broad))), ]
broadNet = broadNet[order(as.numeric(row.names(broadNet))), ]
broadBel = broadBel[order(as.numeric(row.names(broadBel))), ]
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

## ========================================================
##                    First analysis
## ========================================================

print(paste0("Num. WebNet post > pre: ", sum(broadNet1$postVisitorsWebNet > broadNet1$preVisitorsWebNet)))
print(paste0("Num. WebBel post > pre: ", sum(broadNet1$postVisitorsWebBel > broadNet1$preVisitorsWebBel)))

biggestAdsNet1 = subset(broadNet1, postVisitorsWebNet - preVisitorsWebNet > 10)

## ========================================================
##            Regression models 20-minute model
## ========================================================

# dummiesDirectModel contains the treatment variables
variablesDirectModelNet1 = c("product_category", "channel", "length_of_spot", "position_in_break_3option", "overlapBefore", "overlapAfter")
dummiesDirectModelPreNet1 = dummy_cols(.data = broadNet1, select_columns = variablesDirectModelNet1)
dummiesDirectModelNet1 = dummiesDirectModelPreNet1[,((ncol(broadNet1) + 1):ncol(dummiesDirectModelPreNet1))]
dummiesDirectModelNet1 = as.data.frame(dummiesDirectModelNet1)
dummiesDirectModelNet1 = subset(dummiesDirectModelNet1, select = -c(`length_of_spot_30 + 10 + 5`, product_category_laptops, `channel_Discovery Channel`, position_in_break_3option_middle, overlapBefore_0, overlapAfter_0) )
rm(dummiesDirectModelPreNet1); rm(variablesDirectModelNet1)

# Function for model summary
getModelSumm <- function(model, coef) {
  if(coef) {
    print(coeftest(model, vcov = vcovHC(model, type="HC1"))) # Robust SE
  }
  print(paste("R^2: ", summary(model)$r.squared))
  print(paste("AIC: ",AIC(model)))
  print(paste("BIC: ", BIC(model)))
}

# Baseline model
# Website
broadNet1['minusPreVisitorsWebBel'] = -1 * broadNet1$preVisitorsWebBel
baselineModelWebNet1 = lm( (postVisitorsWebNet-postVisitorsWebBel) ~ preVisitorsWebNet +
                          minusPreVisitorsWebBel + factor(hours) + factor(weekdays), data = broadNet1)
getModelSumm(baselineModelWebNet1, T)

# Full model
# Website
broadNet1['minusPreVisitorsWebBel'] = -1 * broadNet1$preVisitorsWebBel
fullModelWebNet1 = lm( (broadNet1$postVisitorsWebNet-broadNet1$postVisitorsWebBel) ~ 
                         broadNet1$preVisitorsWebNet + broadNet1$minusPreVisitorsWebBel + 
                         factor(broadNet1$hours) + factor(broadNet1$weekdays) + broadNet1$gross_rating_point +., data = dummiesDirectModelNet1)
getModelSumm(fullModelWebNet1, T)

# Baseline model
# App
broadNet1['minusPreVisitorsAppBel'] = -1 * broadNet1$preVisitorsAppBel
baselineModelAppNet1 = lm( (postVisitorsAppNet-postVisitorsAppBel) ~ preVisitorsAppNet +
                          minusPreVisitorsAppBel + factor(hours) + factor(weekdays), data = broadNet1)
getModelSumm(baselineModelAppNet1, T)

# Full model
# App
broadNet1['minusPreVisitorsAppBel'] = -1 * broadNet1$preVisitorsAppBel
fullModelAppNet1 = lm( (broadNet1$postVisitorsAppNet-broadNet1$postVisitorsAppBel) ~ 
                      broadNet1$preVisitorsAppNet + broadNet1$minusPreVisitorsAppBel + 
                      factor(broadNet1$hours) + factor(broadNet1$weekdays) + broadNet1$gross_rating_point + ., data = dummiesDirectModelNet1)
getModelSumm(fullModelAppNet1, T)

# =============================================================
#                  Test for overfitting
# =============================================================

# Calculate Mean Squared Prediction Error 
preVisitorsWebNet = broadNet1$preVisitorsWebNet
postVisitorsWebNet = broadNet1$postVisitorsWebNet
hours = broadNet1$hours
weekdays = broadNet1$weekdays
postVisitorsWebBel = broadNet1$postVisitorsWebBel
grossRating = broadNet1$gross_rating_point
minusPreVisitorsWebBel = -1 * broadNet1$preVisitorsWebBel
broadDumm = cbind(postVisitorsWebNet, preVisitorsWebNet, postVisitorsWebBel, hours, minusPreVisitorsWebBel, weekdays, grossRating, dummiesDirectModelNet1)

set.seed(21)
folds = 100
avBaseTrainError = vector(length = folds)
avBaseTestError = vector(length = folds)
avFullTrainError = vector(length = folds)
avFullTestError = vector(length = folds)
for (i in 1:folds){
  broadTotal = broadDumm
  
  sampleSplit = sample.split(broadNet1$postVisitorsWebNet, SplitRatio = 0.8)
  broadTrain = broadDumm[sampleSplit == T, ]
  broadTest = broadDumm[sampleSplit == F, ]

  # Baseline model
  baselineModelWebNet1 = lm((postVisitorsWebNet - postVisitorsWebBel) ~ preVisitorsWebNet +
                              minusPreVisitorsWebBel + factor(weekdays) + factor(hours) , data = broadTotal)
 
  avBaseTrainError[i] = rmse((broadTrain$postVisitorsWebNet - broadTrain$postVisitorsWebBel), predict(baselineModelWebNet1, broadTrain))
  avBaseTestError[i] = rmse((broadTest$postVisitorsWebNet - broadTest$postVisitorsWebBel), predict(baselineModelWebNet1, broadTest))
  
 
  # Full model
  fullModel = lm((postVisitorsWebNet - postVisitorsWebBel) ~  ., data = broadTotal)

  avFullTrainError[i] = rmse((broadTrain$postVisitorsWebNet - broadTrain$postVisitorsWebBel), predict(fullModel, broadTrain))
  avFullTestError[i] = rmse((broadTest$postVisitorsWebNet - broadTest$postVisitorsWebBel), predict(fullModel, broadTest))
  
}
mean(avBaseTrainError)
mean(avBaseTestError)
mean(avFullTrainError)
mean(avFullTestError)


## ========================================================
##              Parallel trends assumption
## ========================================================

# Test parallel trends - website
set.seed(21)
minutes = 20
trendsMatrix = matrix(NA, nrow(broadNet1), minutes)
for (i in 1:nrow(broadNet1)){
  print(i)
  date = broadNet1$date[i]
  daysNet = which(as.character(visWebNet$date) == date)
  daysBel = which(as.character(visWebBel$date) == date)
  datetime = broadNet1$date_time[i]
  datetime = as.POSIXct(datetime)
  twentyEarlier = datetime - 20 * 60
  for (j in 1:minutes){
    minute = twentyEarlier + (j - 1) * 60
    minuteSub = substr(minute, 12, 19)
    if (minuteSub == ""){
      minuteSub = "00:00:00"
    }
    timeMin = 60 * 24 * as.numeric(times(minuteSub))
    visitIndexNet = sum(visWebNet$visits_index[daysNet[visWebNet$time_min[daysNet] == timeMin]])
    visitIndexBel = sum(visWebBel$visits_index[daysBel[visWebBel$time_min[daysBel] == timeMin]])
    trendsMatrix[i, j] = visitIndexNet - visitIndexBel
  }
}

peakMatrix = matrix(0, nrow(broadNet1), minutes)
for (i in 1:nrow(broadNet1)){
  sdPeak = sd(trendsMatrix[i, ])
  meanPeak = mean(trendsMatrix[i, ])
  for (j in 1:minutes){
    if (meanPeak - 2 * sdPeak <= trendsMatrix[i, j] & trendsMatrix[i, j] <= meanPeak + 2 * sdPeak){
      peakMatrix[i, j] = 1
    }
  }
}

nTotal = nrow(peakMatrix) * ncol(peakMatrix)
withinTwo = sum(peakMatrix)/nTotal

max = which(as.character(broadNet1$gross_rating_point) == max(broadNet1$gross_rating_point))
sdPeak = sd(trendsMatrix[max, ])
meanPeak = mean(trendsMatrix[max, ])
df = data.frame(x = 1:20, F = trendsMatrix[max, ], L = meanPeak - 2 * sdPeak, U = meanPeak + 2 * sdPeak)
plot(df$x, df$F, ylim = c(0, 0.4), type = "l", main = "", xlab = "Minute", ylab = "Difference")
lines(df$x, df$F, lwd = 2)
# Add red lines to plot
lines(df$x, df$U, col = "red", lty = 2)
lines(df$x, df$L, col = "red", lty = 2)