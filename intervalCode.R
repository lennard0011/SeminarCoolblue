# Seminar Coolblue 2021 - Direct effects model (2 minute interval)


# For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
# Results are stored in the column preVisitors and postVisitors in the dataframe broad
  
# count visits pre-commercial
broad['preVisitorsApp'] = 0
broad['preVisitorsWeb'] = 0
broad['postVisitorsApp'] = 0
broad['postVisitorsWeb'] = 0
intervalSize

# count preVisitors and postvisitors for every broadcast
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

# First analysis (onderscheid NL en BE? heel lastig...)
# aggregate pre- and post-visitors (d, r, total)

#website
broad$postVisitorsWeb = as.numeric(broad$postVisitorsWeb)
broad$preVisitorsWeb = as.numeric(broad$preVisitorsWeb)
mean(broad$postVisitorsWeb - broad$preVisitorsWeb)
min(broad$postVisitorsWeb - broad$preVisitorsWeb)
max(broad$postVisitorsWeb - broad$preVisitorsWeb)
sum(broad$postVisitorsWeb > broad$preVisitorsWeb)
sum(broad$postVisitorsWeb < broad$preVisitorsWeb)
#data plotting website
plot(broad$preVisitorsWeb, broad$postVisitorsWeb)
lines(cbind(0,10000), cbind(0,10000))
par(mfrow=c(2,1))
hist(broad$postVisitorsWeb, xlim = c(0,3))
hist(broad$preVisitorsWeb, xlim = c(0,3))
par(mfrow=c(1,1))
simpleModelWeb = lm(broad$postVisitorsWeb ~ broad$preVisitorsWeb + 0)
summary(simpleModelWeb)
simpleModelApp = lm(broad$postVisitorsApp ~ broad$previsitorsApp + 0)
summary(simpleModelApp)

#weg?
#dataInterval = cbind(broad$preVisitorsWeb, broad$postVisitorsWeb)
#dataInterval = as.data.frame(dataInterval)
#colnames(dataInterval) = c("preVisitors", "postVisitors")

#app
broad$postVisitorsApp = as.numeric(broad$postVisitorsApp)
broad$preVisitorsApp = as.numeric(broad$preVisitorsWeb)
mean(broad$postVisitorsApp - broad$preVisitorsApp)
min(broad$postVisitorsApp - broad$preVisitorsApp)
max(broad$postVisitorsApp - broad$preVisitorsApp) # bizar laag!!
sum(broad$postVisitorsApp > broad$preVisitorsApp)
sum(broad$postVisitorsApp < broad$preVisitorsApp)
#data plotting (app)
plot(broad$preVisitorsApp, broad$postVisitorsApp) # deze plot....
lines(cbind(0,10000), cbind(0,10000))
par(mfrow=c(2,1))
hist(broad$postVisitorsApp, xlim = c(0,2))
hist(broad$preVisitorsApp, xlim = c(0,2))
par(mfrow=c(1,1))
simpleModelApp = lm(broad$postVisitorsApp ~ broad$previsitorsApp + 0)
summary(simpleModelApp)

# which ads show the biggest direct increase?
biggestAds = subset(broad, postVisitorsWeb - preVisitorsWeb > 0.7, 
                    select = -c(program_category_after, program_after))

# split data in training and test
data_split = sample.split(broad$postVisitorsWeb, SplitRatio = 0.8)
train = subset(broad$postVisitorsWeb, data_split == TRUE)
test = subset(broad$postVisitorsWeb, data_split == FALSE)

## REGRESSION MODELS 2-minute model

# TODO include option to regress on positive GRP obsv. only
# TODO delete NA dummies `channel_MTV (NL)` `channel_RTL 5` channel_SPIKE  channel_Viceland  channel_VIER channel_ZES  

# Baseline models
#all visitors
baselineModelTotal = lm(postVisitorsWeb ~ preVisitorsWeb + factor(hours), data = broad)
coeftest(baselineModelTotal, vcov = vcovHC(baselineModelTotal, type="HC1")) # robust se
summary(baselineModelTotal) # to get R^2
hist(baselineModelTotal$residuals, breaks = 50)
AIC(baselineModelTotal)
BIC(baselineModelTotal)

# Treatment effect only models
# all visitors
treatmentOnlyModelTotal = lm(broad$postVisitorsWeb ~ ., data = dummiesDirectModelNeeded)
#coeftest(treatmentOnlyModelTotal, vcov = vcovHC(treatmentOnlyModelTotal, type="HC1")) # robust se
summary(treatmentOnlyModelTotal)
hist(baselineModelTotal$residuals, breaks = 100)
AIC(treatmentOnlyModelTotal)
BIC(treatmentOnlyModelTotal)
# no channel
treatmentOnlyModelTotal = lm(broad$postVisitorsWeb ~ ., data = dummiesDirectModelNoChannel)
#coeftest(treatmentOnlyModelTotal, vcov = vcovHC(treatmentOnlyModelTotal, type="HC1")) # robust se
summary(treatmentOnlyModelTotal)$r.squared
AIC(treatmentOnlyModelTotal)
BIC(treatmentOnlyModelTotal)

# Calculate Mean Squared Prediction Error
#Full models
fullModelTotalNoChannel = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNoChannel)
coeftest(fullModelTotalNoChannel, vcov = vcovHC(fullModelTotalNoChannel, type = 'HC1')) #robust se
summary(fullModelTotalNoChannel)$r.squared
#Calculate Mean Squared Prediction Error OUTDATED
postVisitors = broad$postVisitors
preVisitors = broad$preVisitors
hours = broad$hours
broadDumm = cbind(postVisitors, preVisitors, hours, dummiesDirectModelNoChannelNoProduct)

sampleSplit = sample.split(broadDumm$postVisitors, SplitRatio = 0.8)
broadTrain = broadDumm[sampleSplit == TRUE,]
broadTest = broadDumm[sampleSplit == FALSE,]

# Baseline model
baselineModelTotal = lm(postVisitors ~ preVisitors + hours, data=broadTrain)
summary(baselineModelTotal)
rmse(broadTest$postVisitors, predict(baselineModelTotal, broadTest))

# Full treatment model
treatmentOnlyModelTotal = lm(postVisitors ~ ., data = broadTrain)
summary(treatmentOnlyModelTotal)$r.squared
rmse(broadTest$postVisitors, predict(treatmentOnlyModelTotal, broadTest))



#stop



# Full models
# all visitors
# all visitors -- no channel dummies
fullModelTotalNoChannel = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNoChannel)
coeftest(fullModelTotalNoChannel, vcov = vcovHC(fullModelTotalNoChannel, type="HC1")) # robust se
summary(fullModelTotalNoChannel)

# all visitors -- no channel dummies, no prod. category
fullModelTotalNoChannelNoProduct = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNoChannelNoProduct)
coeftest(fullModelTotalNoChannelNoProduct, vcov = vcovHC(fullModelTotalNoChannelNoProduct, type="HC1")) # robust se
summary(fullModelTotalNoChannelNoProduct) # makes clusters somewhat more sign. but not too many

# Evaluation (for now on FULL models)
R2models = cbind(summary(baselineModelTotal)$r.squared, summary(treatmentOnlyModelTotal)$r.squared, summary(fullModelTotal)$r.squared)
AICmodels = cbind(AIC(baselineModelTotal), AIC(treatmentOnlyModelTotal), AIC(fullModelTotal))
BICmodels = cbind(BIC(baselineModelTotal), BIC(treatmentOnlyModelTotal), BIC(fullModelTotal))
R2_AIC_BICmodels = rbind(R2models, AICmodels, BICmodels)
colnames(R2_AIC_BICmodels) = c("Baseline only", "Treatment only", "Full model") 
rownames(R2_AIC_BICmodels) = c("R2", "AIC", "BIC")

format(R2_AIC_BICmodels, scientific = FALSE, digits = 2)

fullModelTime = lm(broad$postVisitors ~broad$preVisitors +., data = dummiesDirectModelTime)

summary(fullModelTime)
