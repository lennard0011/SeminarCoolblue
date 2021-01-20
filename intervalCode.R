#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN

#count visits pre-commercial
intervalSize = 2
start = Sys.time()
BroadCountAmount = nBroad

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
dataInterval = cbind(broad$preVisitors, broad$postVisitors)
#data = cbind(log(broad$postVisitors[1:nBroad]), log(broad$preVisitors[1:nBroad]))
dataInterval = as.data.frame(dataInterval)
colnames(dataInterval) = c("preVisitors", "postVisitors")

# data plotting
plot(dataInterval$preVisitors, dataInterval$postVisitors)
lines(cbind(0,10000), cbind(0,10000))

# simple regression model
modelVisitors = lm(postVisitors[1:broadCountAmount] ~ 0 + preVisitors[1:broadCountAmount], data = broad) #DataFlair
summary(modelVisitors)
coefficients(modelVisitors)
hist(broad$postVisitors[1:broadCountAmount])
hist(broad$preVisitors[1:broadCountAmount])

modelVisitors = lm(postVisitors ~ 0 + preVisitors, data = broad) #DataFlair
summary(modelVisitors)
coefficients(modelVisitors)
hist(broad$postVisitors)
hist(broad$preVisitors)

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
modelVisitorsAdv = lm(broad$postVisitors[1:broadCountAmount] ~ broad$preVisitors[1:broadCountAmount] + regData)
  summary(modelVisitorsAdv)
coefficients(modelVisitorsAdv)

<<<<<<< HEAD
##REGRESSION MODELS 2-minute model

# baseline models
=======
#REGRESSION MODELS 2-minute model
>>>>>>> ccc0a5363eb45a5488180c8417269ddff405e8fa
baselineModelTotal = lm(postVisitors ~ preVisitors, data = broad)
summary(baselineModelTotal)
baselineModelSearchOther = lm(postVisitorsDirectOther ~ preVisitorsDirectOther, data = broad)
summary(baselineModelSearchOther)
baselineModelReferrals = lm(postVisitorsReferrals ~ broad$preVisitorsReferrals, data = broad)
summary(baselineModelReferrals)

<<<<<<< HEAD
# full models
fullModelTotal = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNeeded)
summary(fullModelTotal)
fullModelTotalNoChannel = lm(broad$postVisitors ~ broad$preVisitors + ., data = dummiesDirectModelNoChannel)
summary(fullModelTotalNoChannel)
=======
#DUMMIES
#1. Product: Wasmachines, television, laptop
#2. Broadcast category: 7 
#3. TV channel: 51?
#4. Commercial length: 30, 30+10, 30+10+5
#5. Position in break: beginning (1-3), middle (4-15), last (15-25??)
>>>>>>> ccc0a5363eb45a5488180c8417269ddff405e8fa
