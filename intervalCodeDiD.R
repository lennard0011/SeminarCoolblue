# Seminar Coolblue 2021 - Direct effects model (1-hour diff-in-diff interval)
# For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
# Results are stored in the column preVisitors and postVisitors in the dataframe broadMostViewed

## ========================================================
##                  Collect interval data
## ========================================================

#get commercials only in NL, with BE control
broadNet1 = subset(broad, country == "Netherlands")
broadNet1 = broadNet1[order(broadNet1$date),]

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

# Remove commercials where Bel has a commercial as well
belAdDates = sort(unique(subset(broad, country == "Belgium")$date))
for (i in 1:nrow(broadNet1)) {
  for (j in 1:length(belAdDates)) {
    if (broadNet1[i,]$date == belAdDates[j]) {
      broadNet1[i,]$channel = NA
    }
  }
}
broadNet1 = subset(broadNet1, !(is.na(channel)))
print(paste0("Number of commercials: ", nrow(broadNet1)))

# Delete lowest GRP ratios
broadNet1 = broadNet1[order(broad$gross_rating_point, decreasing = T),]
broadNet1 = subset(broadNet1, gross_rating_point > 0.5)
print(paste0("Number of commercials: ", nrow(broadNet1)))

# Calculate pre- and post visitors, Net and Bel, Web and App (1 hour)
intervalSize = 20
start = Sys.time()
for (i in 1:nrow(broadNet1)) {
  broadDate = broadNet1$date[[i]]
  broadTime = broadNet1$time_min[[i]]
  
  # count extraViewers from day before or day after at midnight!
  if (broadTime - intervalSize < 0) {
    #extraVis of day before
    preVisitorsWebNetExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsWebNet)
    preVisitorsAppNetExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsAppNet)
    preVisitorsWebBelExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsWebBel)
    preVisitorsAppBelExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsAppBel)
  } else if (broadTime + intervalSize >= 60*24) {
    #extraVis on day after
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

## ========================================================
##            Parallel trends assumption
## ========================================================

# test parallel trends -- website
set.seed(21)
minutes = 20
broadNetRelevant = subset(broadNet1, broadNet1$gross_rating_point > 0.5)
trendsMatrix = matrix(NA, nrow(broadNetRelevant), minutes)
for (i in 1:nrow(broadNetRelevant)){
  print(i)
  date = broadNet$date[i]
  daysNet = which(as.character(visWebNet$date) == date)
  daysBel = which(as.character(visWebBel$date) == date)
  datetime = broadNet$date_time[i]
  datetime = as.POSIXct(datetime)
  oneEarlier = datetime - 60 * 60
  for (j in 0:(minutes - 1)){
    minute = oneEarlier + j * 60
    minuteSub = substr(minute, 12, 19)
    if (minuteSub == ""){
      minuteSub = "00:00:00"
    }
    timeMin = 60 * 24 * as.numeric(times(minuteSub))
    visitIndexNet = sum(visWebNet$visits_index[daysNet[visWebNet$time_min[daysNet] == timeMin]])
    visitIndexBel = sum(visWebBel$visits_index[daysBel[visWebBel$time_min[daysBel] == timeMin]])
    trendsMatrix[i, j + 1] = visitIndexNet - visitIndexBel
  }
}

peakMatrix = matrix(0, nrow(broadNetRelevant), minutes)
for (i in 1:nrow(broadNetRelevant)){
  sdPeak = sd(trendsMatrix[i, ])
  meanPeak = mean(trendsMatrix[i, ])
  for (j in 1:minutes){
    if (meanPeak - 2 * sdPeak <= trendsMatrix[i, j] & trendsMatrix[i, j] <= meanPeak + 2 * sdPeak){
      peakMatrix[i, j] = 1
    }
  }
}
sum(peakMatrix)/(nrow(broadNetRelevant) * minutes) * 100

## ========================================================
##                    First analysis
## ========================================================

#par(mfrow=c(1,2))
#plot(broadNet1$preVisitorsWebNet, broadNet1$postVisitorsWebNet, col = "blue")
#lines(cbind(0,10000), cbind(0,10000))
#plot(broadNet1$preVisitorsWebBel, broadNet1$postVisitorsWebBel, xlim = c(0,50), ylim = c(0,50), col = "red")
#lines(cbind(0,10000), cbind(0,10000))
print(paste0("Num. WebNet post > pre: ", sum(broadNet1$postVisitorsWebNet>broadNet1$preVisitorsWebNet)))
print(paste0("Num. WebBel post > pre: ", sum(broadNet1$postVisitorsWebBel>broadNet1$preVisitorsWebBel)))

biggestAdsNet1 = subset(broadNet1, postVisitorsWebNet-preVisitorsWebNet > 10)

## ========================================================
##            REGRESSION MODELS 20-minute model
## ========================================================

# dummiesDirectModel contains the treatment variables
variablesDirectModelNet1 = c("product_category", "channel", "length_of_spot", "position_in_break_3option", "overlapBefore", "overlapAfter")
dummiesDirectModelPreNet1 = dummy_cols(.data = broadNet1, select_columns = variablesDirectModelNet1)
dummiesDirectModelNet1 = dummiesDirectModelPreNet1[,((ncol(broadNet1)+1):ncol(dummiesDirectModelPreNet1))]
dummiesDirectModelNet1 = as.data.frame(dummiesDirectModelNet1)
dummiesDirectModelNet1 = subset(dummiesDirectModelNet1, select = -c(`length_of_spot_30 + 10 + 5`, product_category_laptops, `channel_Discovery Channel`, position_in_break_3option_middle, overlapBefore_0, overlapAfter_0) )
rm(dummiesDirectModelPreNet1); rm(variablesDirectModelNet1)

# function for model summary
getModelSumm <- function(model, coef) {
  if(coef) {
    #print(model)
    print(coeftest(model, vcov = vcovHC(model, type="HC1"))) # robust se
  }
  print(paste("R^2: ", summary(model)$r.squared))
  print(paste("AIC: ",AIC(model)))
  print(paste("BIC: ", BIC(model)))
}

# Baseline model
#web
broadNet1['minusPreVisitorsWebBel'] = -1*broadNet1$preVisitorsWebBel
baselineModelWebNet1 = lm( (postVisitorsWebNet-postVisitorsWebBel) ~ preVisitorsWebNet +
                          minusPreVisitorsWebBel + factor(hours) + factor(weekdays), data = broadNet1)
getModelSumm(baselineModelWebNet1, T)

# Full model
#web
broadNet1['minusPreVisitorsWebBel'] = -1*broadNet1$preVisitorsWebBel
fullModelWebNet1 = lm( (broadNet1$postVisitorsWebNet-broadNet1$postVisitorsWebBel) ~ 
                         broadNet1$preVisitorsWebNet + broadNet1$minusPreVisitorsWebBel + 
                         factor(broadNet1$hours) + factor(broadNet1$weekdays) + broadNet1$gross_rating_point +., data = dummiesDirectModelNet1)
getModelSumm(fullModelWebNet1, T)

#Baseline model
#app
broadNet1['minusPreVisitorsAppBel'] = -1*broadNet1$preVisitorsAppBel
baselineModelAppNet1 = lm( (postVisitorsAppNet-postVisitorsAppBel) ~ preVisitorsAppNet +
                          minusPreVisitorsAppBel + factor(hours) + factor(weekdays), data = broadNet1)
getModelSumm(baselineModelAppNet1, T)

# Full model
#app
broadNet1['minusPreVisitorsAppBel'] = -1*broadNet1$preVisitorsAppBel
fullModelAppNet1 = lm( (broadNet1$postVisitorsAppNet-broadNet1$postVisitorsAppBel) ~ 
                      broadNet1$preVisitorsAppNet + broadNet1$minusPreVisitorsAppBel + 
                      factor(broadNet1$hours) + + factor(broadNet1$weekdays) + broadNet1$gross_rating_point + ., data = dummiesDirectModelNet1)
getModelSumm(fullModelAppNet1, T)

