#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN
BroadCountAmount = 100
#count visits pre-commercial
broad['preVisitors'] = 0
intervalSize = 5
start = Sys.time()
for (index in 1:BroadCountAmount) { #nBroad
  broadDate = broad$date[[index]]
  broadTime = broad$time_min[[index]]
  broadCountry = broad$country[[index]]
  extraViews = 0
  if(intervalSize > broadTime ){
    extraViews = length(which(traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry))
  }
  broad$preVisitors[[index]] = length(which(traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViews
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}

#count visits post-commercial
broad['postVisitors'] = 0
start = Sys.time()
for (index in 1:BroadCountAmount) { #nBroad
  broadDate = broad$date[[index]]
  broadTime = broad$time_min[[index]]
  broadCountry = broad$country[[index]]
  extraViews = 0
  if(broadTime > 60*24 - intervalSize){
    extraViews = length(which(traffic$date == as.Date(broadDate) + 1 & traffic$country == broadCountry & traffic$time_min <= intervalSize - broadTime & traffic$country == broadCountry))
  }
  broad$postVisitors[[index]] = length(which(traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize))
  if(index %% 1000 == 0) {print(Sys.time() - start)}
}