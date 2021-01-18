#change to intervals
library(chron)
library(caTools)


#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN
BroadCountAmount <- 100

#count visits pre-commercial
broad['preVisitorsDirect'] <- 0
broad['preVisitorsSearch'] <- 0
intervalSize <- 2
start <- Sys.time()
for (index in 1:BroadCountAmount) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broadCountry <- broad$country[[index]]
  extraViewsDirect <- 0
  extraViewsSearch <- 0
  if(intervalSize > broadTime ){
    extraViewsDirect <- length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry))
    extraViewsSearch <- length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry))
  }
  broad$preVisitorsDirect[[index]] <- length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViewsDirect
  broad$preVisitorsSearch[[index]] <- length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViewsSearch
  
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}
#count visits post-commercial
broad['postVisitorsDirect'] <- 0
broad['postVisitorsSearch'] <- 0
start <- Sys.time()
for (index in 1:BroadCountAmount) { #nBroad
  broadDate <- broad$date[[index]]
  broadTime <- broad$time_min[[index]]
  broadCountry <- broad$country[[index]]
  extraViewsDirect <- 0
  extraViewsSearch <- 0
  if(broadTime > 60*24 - intervalSize){
    extraViewsDirect <- length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime & traffic$country == broadCountry))
    extraViewsSearch <- length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime & traffic$country == broadCountry))
  }
  broad$postVisitorsDirect[[index]] <- length(which( (traffic$visit_source == "direct" | traffic$visit_source == "other") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize)) + extraViewsDirect
  broad$postVisitorsSearch[[index]] <- length(which( (traffic$visit_source == "paid search" | traffic$visit_source == "search") & traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize)) + extraViewsSearch
  
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}

mean(broad$postVisitors[1:500] - broad$preVisitors[1:500])
data = cbind(broad$postVisitors[1:500], broad$preVisitors[1:500])
#data = cbind(log(broad$postVisitors[1:500]), log(broad$preVisitors[1:500]))
data <- as.data.frame(data)
colnames(data) <- c("postVisitors", "preVisitors")

data_split = sample.split(data$postVisitors, SplitRatio = 0.8)
train <- subset(data, data_split == TRUE)
test <-subset(data, data_split == FALSE)
plot(data$preVisitors, data$postVisitors)
lines(cbind(0,10000), cbind(0,10000))

model <- lm(postVisitors[1:100] ~ 0 + preVisitors[1:100], data = broad) #DataFlair
summary(model)
coefficients(model)
hist(broad$postVisitors[1:500])
hist(broad$preVisitors[1:500])
