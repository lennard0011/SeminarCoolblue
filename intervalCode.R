#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN

#count visits pre-commercial


intervalSize <- 2
start <- Sys.time()


#count visits pre-commercial
broad['preVisitorsDirect'] = 0
broad['preVisitorsOther'] = 0
broad['preVisitorsPaidSearch'] = 0
broad['preVisitorsFreeSearch'] = 0
intervalSize = 2
start = Sys.time()

for (index in 1:BroadCountAmount) { #nBroad
  broadDate = broad$date[[index]]
  broadTime = broad$time_min[[index]]
  broadCountry = broad$country[[index]]
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
broad['postVisitorsDirect'] <- 0
broad['postVisitorsOther'] <- 0
broad['postVisitorsPaidSearch'] <- 0
broad['postVisitorsFreeSearch'] <- 0
start <- Sys.time()
test = TRUE
BroadCountAmount = 1292 + 1
for (index in 3185:3185) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broadCountry <- broad$country[[index]]
  extraViews = 0 
  extraViewsDirect = 0
  extraViewsOther = 0
  extraViewsPaidSearch = 0
  extraViewsFreeSearch = 0
  
  if(broadTime > 60*24 - intervalSize){ # include views from next day if close to midnight
    extraViews = subset(traffic, traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime)
    print("Works")
    if(test == TRUE){
      #test = FALSE
      print(extraViews)
      print(broadTime)
      print(broadCountry)
    }
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