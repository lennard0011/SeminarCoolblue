# Seminar Coolblue 2021 -- intervalCode1Hour

#get only commercials with: GRP > 1, only in NL, with BE control
broadMostViewed = broad[order(broad$gross_rating_point, decreasing = T),]
broadMostViewed = subset(broadMostViewed, gross_rating_point > 1)
broadMostViewed = subset(broadMostViewed, country == "Netherlands")
BE_ad_dates = unique(subset(broad, country == "Belgium")$date)
for (i in 1:nrow(broadMostViewed)) {
  for (j in 1:length(BE_ad_dates)) {
    if (broadMostViewed[i,]$date == BE_ad_dates[j]) {
      broadMostViewed[i,]$channel = NA # to identify which commercials 
    }
  }
}
broadMostViewed = subset(broadMostViewed, !(is.na(channel)))
# moet ook alle commercials in de soldenperiode (03-01 tm 31-01 eruit omdat het
# dan geen goed vergelijkingsmateriaal is?\)         
             
#delete observations where belgium has no controls


#count visits PRE-commercial
broadMostViewed['preVisitors'] = 0
start = Sys.time()
intervalSize = 60
broadCountAmount = nrow(broadMostViewed) # 414

for (i in 1:broadCountAmount) {
  broadDate = broad$date[[i]]
  broadTime = broad$time_min[[i]]
  broadCountry = broad$country[[i]]
  extraViews = 0 
  
  if(intervalSize > broadTime){ # include views from prev. day if close to midnight
    extraViews = subset(traffic, traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize + broadTime & traffic$country == broadCountry)
  }
  
  preVisitors = subset(traffic, traffic$date == broadDate & traffic$country == broadCountry & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)
  
  broad$preVisitorsDirect[[index]] = length(which(preVisitors$visit_source == "direct")) + extraViewsDirect
  broad$preVisitorsOther[[index]] = length(which(preVisitors$visit_source == "other")) + extraViewsOther
  broad$preVisitorsPaidSearch[[index]] = length(which(preVisitors$visit_source == "paid search")) + extraViewsPaidSearch
  broad$preVisitorsFreeSearch[[index]] = length(which(preVisitors$visit_source == "search")) + extraViewsFreeSearch
  
  if(index %% 10 == 0) {
    print(paste0("Ad nr: ", index))
    print(paste0("Time: ", Sys.time() - start))
  }
}
