#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN

#count visits pre-commercial
intervalSize = 2
start = Sys.time()
broadCountAmount = nBroad

#count visits pre-commercial
broad['preVisitorsDirect'] = 0
broad['preVisitorsOther'] = 0
broad['preVisitorsPaidSearch'] = 0
broad['preVisitorsFreeSearch'] = 0
intervalSize = 2
start = Sys.time()

for (i in 1:nBroad) { #nBroad
  broadDate = broad$date[[i]]
  broadTime = broad$time_min[[i]]
  broadCountry = broad$country[[i]]
  extraViews = 0 
  extraViewsDirect = 0
  extraViewsOther = 0
  extraViewsPaidSearch = 0
  extraViewsFreeSearch = 0
  
  if(intervalSize > broadTime){ # include views from prev. day if close to midnight
    extraViews = subset(traffic, traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry)
    extraViewsDirect = length(which(extraViews$visit_source == "direct"))
    extraViewsOther = length(which(extraViews$visit_source == "other"))
    extraViewsPaidSearch = length(which(extraViews$visit_source == "paid search"))
    extraViewsFreeSearch = length(which(extraViews$visit_source == "search"))
  }
  
  preVisitors = subset(traffic, traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)
  
  broad$preVisitorsDirect[[index]] = length(which(preVisitors$visit_source == "direct")) + extraViewsDirect
  broad$preVisitorsOther[[index]] = length(which(preVisitors$visit_source == "other")) + extraViewsOther
  broad$preVisitorsPaidSearch[[index]] = length(which(preVisitors$visit_source == "paid search")) + extraViewsPaidSearch
  broad$preVisitorsFreeSearch[[index]] = length(which(preVisitors$visit_source == "search")) + extraViewsFreeSearch
  
  if(index %% 100 == 0) {print(Sys.time() - start)}
}
  
#count visits post-commercial
broad['postVisitorsDirect'] = 0
broad['postVisitorsOther'] = 0
broad['postVisitorsPaidSearch'] = 0
broad['postVisitorsFreeSearch'] = 0
start = Sys.time()
for (index in 1:nBroad) { #nBroad
  broadDate = broad$date[[index]]
  broadTime = broad$time_min[[index]]
  broadCountry = broad$country[[index]]
  extraViews = 0 
  extraViewsDirect = 0
  extraViewsOther = 0
  extraViewsPaidSearch = 0
  extraViewsFreeSearch = 0
  
  if(broadTime > 60*24 - intervalSize){ # include views from next day if close to midnight
    extraViews = subset(traffic, traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime)
    extraViewsDirect = length(which(extraViews$visit_source == "direct"))
    extraViewsOther = length(which(extraViews$visit_source == "other"))
    extraViewsPaidSearch = length(which(extraViews$visit_source == "paid search"))
    extraViewsFreeSearch = length(which(extraViews$visit_source == "search"))  
  }
  
  postVisitors = subset(traffic, traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize)
  
  broad$postVisitorsDirect[[index]] = length(which(postVisitors$visit_source == "direct")) + extraViewsDirect
  broad$postVisitorsOther[[index]] = length(which(postVisitors$visit_source == "other")) + extraViewsOther
  broad$postVisitorsPaidSearch[[index]] = length(which(postVisitors$visit_source == "paid search")) + extraViewsPaidSearch
  broad$postVisitorsFreeSearch[[index]] = length(which(postVisitors$visit_source == "search")) + extraViewsFreeSearch
  
  if(index %% 100 == 0) {print(Sys.time() - start)}
}

#aggregate pre- and post-visitors (d, r, total)
broad['preVisitorsDirectOther'] = broad$preVisitorsDirect + broad$preVisitorsOther
broad['preVisitorsReferrals'] = broad$preVisitorsPaidSearch + broad$preVisitorsFreeSearch
broad['preVisitors'] = broad$preVisitorsDirectOther + broad$preVisitorsReferrals
broad['postVisitorsDirectOther'] = broad$postVisitorsDirect + broad$postVisitorsOther
broad['postVisitorsReferrals'] = broad$postVisitorsPaidSearch + broad$postVisitorsFreeSearch
broad['postVisitors'] = broad$postVisitorsDirectOther + broad$postVisitorsReferrals

# first analysis
mean(broad$postVisitors - broad$preVisitors)
min(broad$postVisitors - broad$preVisitors)
max(broad$postVisitors - broad$preVisitors)
dataInterval = cbind(broad$preVisitors, broad$postVisitors)
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
baselineModelTotal = lm(postVisitors ~ preVisitors, data = broad)
coeftest(baselineModelTotal, vcov = vcovHC(baselineModelTotal, type="HC1")) # robust se
summary(baselineModelTotal) # to get R^2
hist(baselineModelTotal$residuals)

# Treatment effect only models
#all visitors
treatmentOnlyModelTotal = lm(broad$postVisitors ~ ., data = dummiesDirectModelNeeded)
summary(treatmentOnlyModelTotal)
hist(baselineModelTotal$residuals)

# Full models
#all visitors

<<<<<<< HEAD
#REGRESSION MODELS 2-minute model

=======
baselineModelTotal = lm(postVisitors ~ preVisitors, data = broad)
summary(baselineModelTotal)
>>>>>>> 2d9e60f840de443652507b53edff0adcf5cf4cac


# full models

fullModelTotal = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNeeded)
coeftest(fullModelTotal, vcov = vcovHC(fullModelTotal, type="HC1")) # robust se

summary(fullModelTotal)
hist(fullModelTotal$residuals)
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
<<<<<<< HEAD
summary(fullModelTime)
=======
summary(fullModelTime)


#DUMMIES
#1. Product: Wasmachines, television, laptop
#2. Broadcast category: 7 
#3. TV channel: 51?
#4. Commercial length: 30, 30+10, 30+10+5
#5. Position in break: beginning (1-3), middle (4-15), last (15-25??)


#polynomial test
polynomial = 6
baselineModelTotal = lm(postVisitors ~ preVisitors + poly(time_min, polynomial), data = broad)
summary(baselineModelTotal)

xseq = seq(0,60*24)
x = poly(xseq, polynomial)
#y = x %*% coef(baselineModelTotal)[3:(polynomial+2)]
y = x %*% coef(baselineModelTotal)[2:(polynomial+1)]
plot(xseq, y)

>>>>>>> 2d9e60f840de443652507b53edff0adcf5cf4cac
