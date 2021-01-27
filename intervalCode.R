# Seminar Coolblue 2021 - Direct effects model (2 minute interval)
# For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
# Results are stored in the column preVisitors and postVisitors in the dataframe broad

## ========================================================
##                  Collect interval data
## ========================================================

# count visits pre-commercial
broad['preVisitorsWeb'] = 0
broad['postVisitorsWeb'] = 0
broad['preVisitorsApp'] = 0
broad['postVisitorsApp'] = 0
intervalSize

start = Sys.time()
for (i in 1:nBroad) { # nBroad
  broadDate = broad$date[[i]]
  broadTime = broad$time_min[[i]]
  broadCountry = broad$country[[i]]
  
  if(broadCountry == "Belgium") {
    if(broadTime - intervalSize < 0) {
      #extraVis of day before
      preVisitorsAppExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsAppBel)
      preVisitorsWebExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsWebBel)
    } else if(broadTime + intervalSize >= 60*24) {
      #extraVis on day after
      postVisitorsAppExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsAppBel)
      postVisitorsWebExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsWebBel)
    } else {
      preVisitorsAppExtra = 0; preVisitorsWebExtra = 0; postVisitorsAppExtra = 0; postVisitorsWebExtra = 0; 
    }
    
    broad$preVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppBel) + preVisitorsAppExtra
    broad$preVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebBel) + preVisitorsWebExtra
    
    broad$postVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppBel) + postVisitorsAppExtra
    broad$postVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebBel) + postVisitorsWebExtra
    
  } else if(broadCountry == "Netherlands") {
    if(broadTime - intervalSize < 0) {
      #extraVis of day before
      preVisitorsAppExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsAppNet)
      preVisitorsWebExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min >= 60*24 + broadTime - intervalSize)$visitsWebNet)
    } else if(broadTime + intervalSize >= 60*24) {
      #extraVis on day after
      postVisitorsAppExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsAppNet)
      postVisitorsWebExtra = sum(subset(visitorsSum, date == as.Date(broadDate) - 1 & time_min <= intervalSize - 60*24 + broadTime)$visitsWebNet)
    } else {
      preVisitorsAppExtra = 0; preVisitorsWebExtra = 0; postVisitorsAppExtra = 0; postVisitorsWebExtra = 0; 
    }
    broad$preVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppNet) + preVisitorsAppExtra
    broad$preVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebNet) + preVisitorsWebExtra
    
    broad$postVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppNet) + postVisitorsAppExtra
    broad$postVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebNet) + postVisitorsWebExtra
  }

  if(i %% 100 == 0) {print(paste(i,Sys.time() - start))}
}

# broad for Netherlands and Belgium
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')




## ========================================================
##                    First analysis
## ========================================================

# website analysis
mean(broad$postVisitorsWeb - broad$preVisitorsWeb)
min(broad$postVisitorsWeb - broad$preVisitorsWeb)
max(broad$postVisitorsWeb - broad$preVisitorsWeb)
sum(broad$postVisitorsWeb > broad$preVisitorsWeb)
sum(broad$postVisitorsWeb < broad$preVisitorsWeb)
# data plotting website
plot(broad$preVisitorsWeb, broad$postVisitorsWeb)
lines(cbind(0,10000), cbind(0,10000))
par(mfrow=c(2,1))
hist(broad$postVisitorsWeb, xlim = c(0,3))
hist(broad$preVisitorsWeb, xlim = c(0,3))
par(mfrow=c(1,1))
simpleModelWeb = lm(broad$postVisitorsWeb ~ broad$preVisitorsWeb + 0)
summary(simpleModelWeb)

# Website NL
mean(broadNet$postVisitorsWeb - broadNet$preVisitorsWeb)
min(broadNet$postVisitorsWeb - broadNet$preVisitorsWeb)
max(broadNet$postVisitorsWeb - broadNet$preVisitorsWeb)
sum(broadNet$postVisitorsWeb > broadNet$preVisitorsWeb)
sum(broadNet$postVisitorsWeb < broadNet$preVisitorsWeb)
# data plotting website
plot(broadNet$preVisitorsWeb, broadNet$postVisitorsWeb)
lines(cbind(0,10000), cbind(0,10000))
par(mfrow=c(2,1))
hist(broadNet$postVisitorsWeb, xlim = c(0,3))
hist(broadNet$preVisitorsWeb, xlim = c(0,3))
par(mfrow=c(1,1))
simpleModelWebNet = lm(broadNet$postVisitorsWeb ~ broadNet$preVisitorsWeb + 0)
summary(simpleModelWebNet)

# Website BE
mean(broadBel$postVisitorsWeb - broadBel$preVisitorsWeb)
min(broadBel$postVisitorsWeb - broadBel$preVisitorsWeb)
max(broadBel$postVisitorsWeb - broadBel$preVisitorsWeb)
sum(broadBel$postVisitorsWeb > broadBel$preVisitorsWeb)
sum(broadBel$postVisitorsWeb < broadBel$preVisitorsWeb)
# data plotting website
plot(broadBel$preVisitorsWeb, broadBel$postVisitorsWeb)
lines(cbind(0,10000), cbind(0,10000))
par(mfrow=c(2,1))
hist(broadBel$postVisitorsWeb, xlim = c(0,3))
hist(broadBel$preVisitorsWeb, xlim = c(0,3))
par(mfrow=c(1,1))
simpleModelWebBel = lm(broadBel$postVisitorsWeb ~ broadBel$preVisitorsWeb + 0)
summary(simpleModelWebBel)

# App analysis
mean(broad$postVisitorsApp - broad$preVisitorsApp)
min(broad$postVisitorsApp - broad$preVisitorsApp)
max(broad$postVisitorsApp - broad$preVisitorsApp)
sum(broad$postVisitorsApp > broad$preVisitorsApp)
sum(broad$postVisitorsApp < broad$preVisitorsApp)
# data plotting (app)
plot(broad$preVisitorsApp, broad$postVisitorsApp, xlim = c(0,0.2)) # deze plot....
lines(cbind(0,10000), cbind(0,10000))
par(mfrow=c(2,1))
hist(broad$postVisitorsApp)
hist(broad$preVisitorsApp)
par(mfrow=c(1,1))
simpleModelApp = lm(broad$postVisitorsApp ~ broad$preVisitorsApp + 0)
summary(simpleModelApp)

biggestAds = subset(broad, postVisitorsWeb-preVisitorsWeb > 0.6)

## ========================================================
##            Parallel trends assumption
## ========================================================

# test parallel trends -- website
set.seed(21)
minutes = 20
trendsMatrix = matrix(NA, nrow(broadNet), minutes)
for (i in 1:nrow(broadNet)){
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

peakMatrix = matrix(0, nrow(broadNet), minutes)
for (i in 1:nrow(broadNet)){
  sdPeak = sd(trendsMatrix[i, ])
  meanPeak = mean(trendsMatrix[i, ])
  for (j in 1:minutes){
    if (meanPeak - 2 * sdPeak <= trendsMatrix[i, j] & trendsMatrix[i, j] <= meanPeak + 2 * sdPeak){
      peakMatrix[i, j] = 1
    }
  }
}


 ## ========================================================
##            REGRESSION MODELS 2-minute model
## ========================================================

# function for model summary
getModelSumm <- function(model, coef) {
  if(coef) {
    print(model)
    print(coeftest(model, vcov = vcovHC(model, type="HC1"))) # robust se
  }
  print(paste("R^2: ", summary(model)$r.squared))
  #hist(model$residuals, breaks = 50)
  print(paste("AIC: ",AIC(model)))
  print(paste("BIC: ", BIC(model)))
}

# Baseline models
baselineModel = lm(postVisitorsWeb ~ preVisitorsWeb + factor(hours), data = broadNet)
getModelSumm(baselineModel, TRUE)

# Treatment effect only models
treatmentOnlyModel = lm(broadNet$postVisitorsWeb ~ ., data = dummiesDirectModel)
getModelSumm(treatmentOnlyModel, TRUE)

# Full model 
fullModel = lm(broadNet$postVisitorsWeb ~ broadNet$preVisitorsWeb + factor(broadNet$hours) + ., data = dummiesDirectModel)
getModelSumm(fullModel, T)

## ========================================================
##                    Overfitting Test
## ========================================================

#Calculate Mean Squared Prediction Error 
preVisitorsWeb = broadNet$preVisitorsWeb
postVisitorsWeb = broadNet$postVisitorsWeb
hours = broadNet$hours
grossRating = broadNet$gross_rating_point
broadDumm = cbind(postVisitorsWeb, preVisitorsWeb, hours, grossRating, dummiesDirectModel)

set.seed(21)
folds = 100
avBaseTrainError = vector(length = folds)
avBaseTestError = vector(length = folds)
avFullTrainError = vector(length = folds)
avFullTestError = vector(length = folds)
for (i in 1:folds){
  sampleSplit = sample.split(broadNet$postVisitorsWeb, SplitRatio = 0.8)
  broadTrain = broadDumm[sampleSplit == TRUE,]
  broadTest = broadDumm[sampleSplit == FALSE,]
  
  # Baseline model
  baselineModel = lm(postVisitorsWeb ~ preVisitorsWeb + hours, data = broadTrain)
  getModelSumm(baselineModel, FALSE)
  avBaseTrainError[i] = rmse(broadTrain$postVisitorsWeb, predict(baselineModel, broadTrain))
  avBaseTestError[i] = rmse(broadTest$postVisitorsWeb, predict(baselineModel, broadTest))
  
  # Treatment effects only models
  #treatmentOnlyModel = lm(postVisitors ~ .-preVisitors, data = broadTrain)
  #rmse(broadTest$postVisitors, predict(treatmentOnlyModel, broadTest))
  
  # Full treatment model
  fullModel = lm(postVisitorsWeb ~ preVisitorsWeb + ., data = broadTrain)
  getModelSumm(fullModel, FALSE)
  avFullTrainError[i] = rmse(broadTrain$postVisitorsWeb, predict(fullModel, broadTrain))
  avFullTestError[i] = rmse(broadTest$postVisitorsWeb, predict(fullModel, broadTest))
}
mean(avBaseTrainError)
mean(avBaseTestError)
mean(avFullTrainError)
mean(avFullTestError)
