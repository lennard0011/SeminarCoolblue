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
  
  # TO-DO count extraViewers from day before or day after at midnight!
  
  if(broadCountry == "Belgium") {
    broad$preVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppBel)
    broad$preVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebBel)
    
    broad$postVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppBel)
    broad$postVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebBel)
    
  } else if(broadCountry == "Netherlands") {
    broad$preVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppNet)
    broad$preVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebNet)
    
    broad$postVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppNet)
    broad$postVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebNet)
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




## ========================================================
##            REGRESSION MODELS 2-minute model
## ========================================================

<<<<<<< HEAD
# split data in training and test
set.seed(21)
data_split = sample.split(broad$postVisitorsWeb, SplitRatio = 0.8)
train = subset(broad$postVisitorsWeb, data_split == TRUE)
test = subset(broad$postVisitorsWeb, data_split == FALSE)

=======
>>>>>>> aec0f8bccc3541f5001d14b8aa5498b081a5cb3f
# function for model summary
getModelSumm <- function(model, coef) {
  if(coef) {
    print(model)
    print(coeftest(model, vcov = vcovHC(model, type="HC1"))) # robust se
  }
  print(paste("R^2: ", summary(model)$r.squared))
  hist(model$residuals, breaks = 50)
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
fullModel = lm(broadNet$postVisitorsWeb ~ broadNet$preVisitorsWeb + factor(hours) + ., data = dummiesDirectModel)
getModelSumm(fullModel, TRUE)




## ========================================================
##                    Overfitting Test
## ========================================================

<<<<<<< HEAD
# Calculate Mean Squared Prediction Error 
postVisitors = broadNet$postVisitorsWeb
preVisitors = broadNet$preVisitorsWeb
=======
#Calculate Mean Squared Prediction Error 
set.seed(21)
postVisitorsWeb = broadNet$postVisitorsWeb
preVisitorsWeb = broadNet$preVisitorsWeb
>>>>>>> aec0f8bccc3541f5001d14b8aa5498b081a5cb3f
hours = broadNet$hours
broadDumm = cbind(postVisitorsWeb, preVisitorsWeb, hours, dummiesDirectModel)

sampleSplit = sample.split(broadNet$postVisitorsWeb, SplitRatio = 0.8)
broadTrain = broadDumm[sampleSplit == TRUE,]
broadTest = broadDumm[sampleSplit == FALSE,]

# Baseline model
baselineModel = lm(postVisitorsWeb ~ preVisitorsWeb + factor(hours), data = broadTrain)
getModelSumm(baselineModel, FALSE)
rmse(broadTrain$postVisitorsWeb, predict(baselineModel, broadTrain))
rmse(broadTest$postVisitorsWeb, predict(baselineModel, broadTest))

# Treatment effects only models
#treatmentOnlyModel = lm(postVisitors ~ .-preVisitors, data = broadTrain)
#rmse(broadTest$postVisitors, predict(treatmentOnlyModel, broadTest))

# Full treatment model
<<<<<<< HEAD
FullModelTotal = lm(postVisitors ~ preVisitors + ., data = broadTrain)
summary(FullModelTotal)
rmse(broadTest$postVisitors, predict(FullModelTotal, broadTest))
=======
fullModel = lm(postVisitorsWeb ~ preVisitorsWeb + ., data = broadTrain)
getModelSumm(fullModel, FALSE)
rmse(broadTrain$postVisitorsWeb, predict(fullModel, broadTrain))
rmse(broadTest$postVisitorsWeb, predict(fullModel, broadTest))
>>>>>>> aec0f8bccc3541f5001d14b8aa5498b081a5cb3f
