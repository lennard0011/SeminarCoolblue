#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
<<<<<<< HEAD
#BEWARE IT TAKES A LONG TIME TO RUN
BroadCountAmount <- nBroad #nBroad

#count visits pre-commercial
broad['preVisitorsDirect'] <- 0
broad['preVisitorsSearch'] <- 0
intervalSize <- 2
start <- Sys.time()
for (index in 1:BroadCountAmount) {
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broadCountry <- broad$country[[index]]
  extraViewsDirect <- 0
  extraViewsSearch <- 0
=======
BroadCountAmount = nBroad

#count visits pre-commercial
broad['preVisitorsDirect'] = 0
broad['preVisitorsSearch'] = 0
intervalSize = 2
start = Sys.time()
for (index in 1:BroadCountAmount) { #nBroad
  broadDate = broad$date[[index]]
  broadTime = broad$time_min[[index]]
  broadCountry = broad$country[[index]]
  extraViewsDirect = 0
  extraViewsSearch = 0
>>>>>>> efbc82d95da3f010936e405e13e52128171d5b16
  if(intervalSize > broadTime){ # include views from prev. day if close to midnight
    extraViewsDirect = length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry))
    extraViewsSearch = length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry))
  }
  broad$preVisitorsDirect[[index]] = length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViewsDirect
  broad$preVisitorsSearch[[index]] = length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViewsSearch
  
  if(index %% 100 == 0) {print(Sys.time() - start)}
}

#count visits post-commercial
broad['postVisitorsDirect'] = 0
broad['postVisitorsSearch'] = 0
start = Sys.time()
for (index in 1:BroadCountAmount) { #nBroad
  broadDate = broad$date[[index]]
  broadTime = broad$time_min[[index]]
  broadCountry = broad$country[[index]]
  extraViewsDirect = 0
  extraViewsSearch = 0
  if(broadTime > 60*24 - intervalSize){ # include views from next day if close to midnight
    extraViewsDirect = length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime & traffic$country == broadCountry))
    extraViewsSearch = length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime & traffic$country == broadCountry))
  }
  broad$postVisitorsDirect[[index]] = length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize)) + extraViewsDirect
  broad$postVisitorsSearch[[index]] = length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize)) + extraViewsSearch
  
  if(index %% 100 == 0) {print(Sys.time() - start)}
}

# first analysis pre- and post-visitors
broad['preVisitors'] = broad$preVisitorsDirect + broad$preVisitorsSearch
broad['postVisitors'] = broad$postVisitorsDirect + broad$postVisitorsSearch
mean(broad$postVisitors[1:BroadCountAmount] - broad$preVisitors[1:BroadCountAmount])
dataInterval = cbind(broad$preVisitors[1:BroadCountAmount], broad$postVisitors[1:BroadCountAmount])
#data = cbind(log(broad$postVisitors[1:500]), log(broad$preVisitors[1:500]))
dataInterval = as.data.frame(dataInterval)
colnames(dataInterval) = c("preVisitors", "postVisitors") #@Len I think it would make more sense to first display "pre" and than "post"

# data plotting
plot(dataInterval$preVisitors, dataInterval$postVisitors)
lines(cbind(0,10000), cbind(0,10000))

# simple regression model
modelVisitors = lm(postVisitors[1:BroadCountAmount] ~ 0 + preVisitors[1:BroadCountAmount], data = broad) #DataFlair
summary(modelVisitors)
coefficients(modelVisitors)
hist(broad$postVisitors[1:BroadCountAmount])
hist(broad$preVisitors[1:BroadCountAmount])

# split data in training and test
data_split = sample.split(dataInterval$postVisitors, SplitRatio = 0.8)
train = subset(dataInterval, data_split == TRUE)
test = subset(dataInterval, data_split == FALSE)

#more advanced regression
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
modelVisitorsAdv = lm(broad$postVisitors[1:BroadCountAmount] ~ broad$preVisitors[1:BroadCountAmount] + regData)
summary(modelVisitorsAdv)
coefficients(modelVisitorsAdv)