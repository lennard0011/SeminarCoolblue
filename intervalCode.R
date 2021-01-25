# Seminar Coolblue 2021 - Direct effects model (2 minute interval)

#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN


#count visits pre-commercial
broad['preVisitorsApp'] = 0
broad['preVisitorsWeb'] = 0
broad['postVisitorsApp'] = 0
broad['postVisitorsWeb'] = 0

intervalSize = 2

#count preVisitors and postvisitors for every broadcast
start = Sys.time()
for (i in 1:nBroad) { #nBroad
  broadDate = broad$date[[i]]
  broadTime = broad$time_min[[i]]
  broadCountry = broad$country[[i]]
  
  #TO-DO count extraViewers from day before or day after at midnight!
  
  if(broadCountry == "Belgium") {
    broad$preVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppBel)
    broad$preVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebBel)
    
    broad$postVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppBel)
    broad$postVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebBel)
    
  } else if(broadCountry == "Netherlands") {
    broad$preVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppNed)
    broad$preVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebNed)
    
    broad$postVisitorsApp[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppNed)
    broad$postVisitorsWeb[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebNed)
  }

  if(i %% 100 == 0) {print(paste(i,Sys.time() - start))}
}

#aggregate pre- and post-visitors (d, r, total)

# first analysis
mean(broad$postVisitorsWeb - broad$preVisitorsWeb)
lm(broad$postVisitorsWeb ~ broad$preVisitorsWeb + 0)

#min(broad$postVisitors - broad$preVisitors)
#max(broad$postVisitors - broad$preVisitors)
#dataInterval = cbind(broad$preVisitors, broad$postVisitors)
#data = cbind(log(broad$postVisitors[1:nBroad]), log(broad$preVisitors[1:nBroad]))
dataInterval = as.data.frame(dataInterval)
colnames(dataInterval) = c("preVisitors", "postVisitors")

# data plotting
plot(dataInterval$preVisitors, dataInterval$postVisitors)
lines(cbind(0,10000), cbind(0,10000))
hist(broad$postVisitors)
hist(broad$preVisitors)

# which ads show the biggest direct increase?
biggestAds = subset(broad, preVisitors - postVisitors > 30, 
                    select = -c(program_category_after, program_after))

# split data in training and test
data_split = sample.split(dataInterval$postVisitors, SplitRatio = 0.8)
train = subset(dataInterval, data_split == TRUE)
test = subset(dataInterval, data_split == FALSE)

#more advanced regression
indexHemelvaart = yday("2019-05-30")
broad$hemelvaart = 0
broad$monday = 0
for (i in 1:nBroad){
  if (yday(broad[i, ]$date) == indexHemelvaart){
    broad$hemelvaart[[i]] = 1
  }
  if (weekdays(as.Date(broad[i, ]$date)) == "maandag"){
    broad$monday[[i]] = 1
  }
}
regData = cbind(broad$hemelvaart, broad$monday)
modelVisitorsAdv = lm(postVisitors ~ ., data = broad)
summary(modelVisitorsAdv)
coefficients(modelVisitorsAdv)


##REGRESSION MODELS 2-minute model

# TODO include option to regress on positive GRP obsv. only
# TODO delete NA dummies `channel_MTV (NL)` `channel_RTL 5` channel_SPIKE  channel_Viceland  channel_VIER channel_ZES  

# Baseline models
#all visitors
baselineModelTotal = lm(postVisitors ~ preVisitors + factor(hours), data = broadNonZeroGross)
coeftest(baselineModelTotal, vcov = vcovHC(baselineModelTotal, type="HC1")) # robust se
summary(baselineModelTotal) # to get R^2
hist(baselineModelTotal$residuals)
AIC(baselineModelTotal)
BIC(baselineModelTotal)

# Treatment effect only models
# all visitors
treatmentOnlyModelTotal = lm(broadNonZeroGross$postVisitors ~ broadNonZeroGross$preVisitors + ., data = dummiesDirectModelNeeded)
coeftest(treatmentOnlyModelTotal, vcov = vcovHC(treatmentOnlyModelTotal, type="HC1")) # robust se
summary(treatmentOnlyModelTotal)
hist(baselineModelTotal$residuals, breaks=100)
AIC(treatmentOnlyModelTotal)
BIC(treatmentOnlyModelTotal)
# no channel
treatmentOnlyModelTotal = lm(broadNonZeroGross$postVisitors ~ broadNonZeroGross$preVisitors + ., data = dummiesDirectModelNoChannel)
coeftest(treatmentOnlyModelTotal, vcov = vcovHC(treatmentOnlyModelTotal, type="HC1")) # robust se
summary(treatmentOnlyModelTotal)
AIC(treatmentOnlyModelTotal)
BIC(treatmentOnlyModelTotal)


#Calculate Mean Squared Prediction Error
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
summary(treatmentOnlyModelTotal)
rmse(broadTest$postVisitors, predict(treatmentOnlyModelTotal, broadTest))




#stop





# Full models
#all visitors

#all visitors -- no channel dummies
fullModelTotalNoChannel = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNoChannel)
coeftest(fullModelTotalNoChannel, vcov = vcovHC(fullModelTotalNoChannel, type="HC1")) # robust se
summary(fullModelTotalNoChannel)


#all visitors -- no channel dummies, no prod. category
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

#polynomial test
polynomial = 6
baselineModelTotal = lm(postVisitors ~ preVisitors + poly(time_min, polynomial), data = broad)
summary(baselineModelTotal)

xseq = seq(0,60*24)
x = poly(xseq, polynomial)
#y = x %*% coef(baselineModelTotal)[3:(polynomial+2)]
y = x %*% coef(baselineModelTotal)[2:(polynomial+1)]
plot(xseq, y)
