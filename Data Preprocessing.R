# Seminar Coolblue 2021 -- Data Preprocessing (variables, dummies)

#Installing packages (only once)
install.packages("chron")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("fastDummies")
install.packages("zoo")
install.packages("CausalImpact")
install.packages("factoextra")
install.packages("fastDummies")
install.packages("AIC")
install.packages("BIC")
install.packages("Metrics")

#Adding packages
library("chron")
library("ggplot2")
library("lubridate")
library("data.table")
library("caTools")
library("fastDummies")
library("zoo")
library("CausalImpact")
library("factoextra")
library("fastDummies")
library("lmtest") # use the variance estimator in a linear model
library("sandwich") # computes robust covariance matrix estimators
library("stats") # AIC, BIC
library("Metrics") #rmse calc
library("plyr")

#loading the data
traffic = read.csv(file.choose(), header = T)
broad = read.csv(file.choose(), header = T)

traffic = subset(traffic, bounces != 1 | is.na(bounces))
#option to leave out zero gross rating points
broad = broad[broad[, "gross_rating_point"] > 0,]

nTraffic = nrow(traffic)
nBroad = nrow(broad)


#now that packages and data has been loaded, we start by creating new features and set the data up in a nice and usable way

#now that packages and data has been loaded, we start by creating new features and set the data up in a nice and usable way
#First we add the column time_min to traffic and broad, which is the time in a scale of minutes (from 0 to 24*60= 1440)
#Also we add the column date to traffic
#add date+time to every broadcast

broad$date_time = 0
for (i in 1:nBroad){
  broad$date_time[i] = paste(broad$date[i], broad$time[i], sep = " ")
}

#add time_min to every broadcast
broad$time_min = 0
for (i in 1:nBroad) { #nBroad
  time = broad$time[i]
  timeMinute = 60 * 24 * as.numeric(times(time))
  broad$time_min[i] = timeMinute
}
rm(timeMinute)
rm(time)
#add time_min and date to every travel
traffic['time_min'] = 0
traffic$date_time = as.character(traffic$date_time)
trafficDateSplitWhole = strsplit(traffic$date_time, "\\s+")
trafficDateSplitUnlist = unlist(trafficDateSplitWhole)
traffictime = trafficDateSplitUnlist[seq(2, length(trafficDateSplitUnlist), 2)]
traffic$date = trafficDateSplitUnlist[seq(1, length(trafficDateSplitUnlist), 2)]
traffic$time_min = 60 * 24 * as.numeric(times(traffictime))
rm(trafficDateSplitWhole)
rm(trafficDateSplitUnlist)
rm(traffictime)

# Separate the data in website/app and Netherlands/Belgium
visWebNet = traffic[traffic$medium == "website" & traffic$country == "Netherlands" & traffic$visit_source != "push notification", ]
visAppNet = traffic[traffic$medium == "app" & traffic$country == "Netherlands" & traffic$visit_source != "push notification", ]
visWebBel = traffic[traffic$medium == "website" & traffic$country == "Belgium" & traffic$visit_source != "push notification", ]
visAppBel = traffic[traffic$medium == "app" & traffic$country == "Belgium" & traffic$visit_source != "push notification", ]

# Aggregate the visit_index per minute
amountDays = 31 + 28 + 31 + 30 + 31 + 30
maxPairs = amountDays * 1440
maxPairs
# Netherlands - Website
visWebNetSum = aggregate(visits_index ~ date + time_min, data = visWebNet, FUN=sum, simplify = TRUE, drop = TRUE)
names(visWebNetSum) = cbind("date", "time_min", "visitsWebNet")
print(paste0("Missing pairs Website-Netherlands (incl. summer time): ", maxPairs-nrow(visWebNetSum)))

# Netherlands - App
visAppNetSum = aggregate(visits_index ~ date +  time_min + date, data = visAppNet, FUN=sum, simplify = TRUE, drop = TRUE)
names(visAppNetSum) = cbind("date", "time_min", "visitsAppNet")
print(paste0("Missing pairs App-Netherlands (incl. summer time): ", maxPairs-nrow(visAppNetSum)))

# Belgium - Website
visWebBelSum = aggregate(visits_index ~ date + time_min + date, data = visWebBel, FUN=sum, simplify = TRUE, drop = TRUE)
names(visWebBelSum) = cbind("date", "time_min", "visitsWebBel")
print(paste0("Missing pairs Website-Belgium (incl. zomertijd): ", maxPairs-nrow(visWebBelSum)))

# Belgium - App
visAppBelSum = aggregate(visits_index ~ date + time_min + date, data = visAppBel, FUN=sum, simplify = TRUE, drop = TRUE)
names(visAppBelSum) = cbind("date", "time_min", "visitsAppBel")
print(paste0("Missing pairs App-Belgium (incl. zomertijd): ", maxPairs-nrow(visAppBelSum)))

visitorsSum = merge(merge(visWebNetSum, visAppNetSum, all = TRUE), merge(visWebBelSum, visAppBelSum, all = TRUE), all = TRUE)
visitorsSum[is.na(visitorsSum)] = 0
# insert summer time na obs. 128280 (wie hier zin in heeft mag het doen)

## Aggregate visit density over the days, 4 pairs of combinations
amountDays = 31 + 28 + 31 + 30 + 31 + 30
uniqueDates = unique(traffic$date) 
uniqueDates = sort(uniqueDates)
daysVisitorsSum = matrix(0.0, nrow = amountDays, ncol = 5)
colnames(daysVisitorsSum) = cbind("date", "visitsWebNetDay", "visitsAppNetDay", "visitsWebBelDay", "visitsAppBelDay")
daysVisitorsSum[,1] = uniqueDates
for (i in 1:nrow(visitorsSum)) { # takes at most 2min to run
  day = yday(visitorsSum$date[i])
  visitIndexWebNet = visitorsSum$visitsWebNet[i]
  if (is.na(visitIndexWebNet)) {visitIndexWebNet = 0.0}
  visitIndexAppNet = visitorsSum$visitsAppNet[i]
  if (is.na(visitIndexAppNet)) {visitIndexAppNet = 0.0}
  visitIndexWebBel = visitorsSum$visitsWebBel[i]
  if (is.na(visitIndexWebBel)) {visitIndexWebBel = 0.0}
  visitIndexAppBel = visitorsSum$visitsAppBel[i]
  if (is.na(visitIndexAppBel)) {visitIndexAppBel = 0.0}
  daysVisitorsSum[day, 2] = as.numeric(daysVisitorsSum[day, 2]) + visitIndexWebNet
  daysVisitorsSum[day, 3] = as.numeric(daysVisitorsSum[day, 3]) + visitIndexAppNet
  daysVisitorsSum[day, 4] = as.numeric(daysVisitorsSum[day, 4]) + visitIndexWebBel
  daysVisitorsSum[day, 5] = as.numeric(daysVisitorsSum[day, 5]) + visitIndexAppBel
}

#Further country specific variables + Aggregate clicks no a day
trafficNet = subset(traffic, country == 'Netherlands')
trafficBel = subset(traffic, country == 'Belgium')
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

#amount of days in time-frame
amountDays = 31 + 28 + 31 + 30 + 31 + 30
#amountDays = amountDays op 27-01 verwijderen
#set of unique advertising dates
uniqueDates = unique(broad$date)
uniqueDatesBel = unique(broadBel$date)
uniqueDatesNet = unique(broadNet$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) #adverts in both on certain day
uniqueDatesOnlyBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) #adverts only in Belgium on certain day
uniqueDatesOnlyNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) #adverts only in Netherlands on certain day

#calculate average for different searches
#visitorsSum$time_min = 60 * 24 * as.numeric(times(substr(visitorsSum[, 1], 12, 19))) delete 27-01
maxWebsiteNet = max(visitorsSum$visitsWebNet)
maxAppNet = max(visitorsSum$visitsAppNet)
maxWebsiteBel = max(visitorsSum$visitsWebBel)
maxAppBel = max(visitorsSum$visitsAppBel)

#Data for plot average of hour over the days
#calculate average for different searches -- Netherlands - website
avTrafficDayNetWebsite = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 3]
  avTrafficDayNetWebsite[i] = mean(visitorsSumSubset)
}

#calculate average for different searches -- Netherlands - app
avTrafficDayNetApp = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 4]
  avTrafficDayNetApp[i] = mean(visitorsSumSubset)
}

#calculate average for different searches -- Belgium - website
avTrafficDayBelWebsite = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 5]
  avTrafficDayBelWebsite[i] = mean(visitorsSumSubset)
}

#calculate average for different searches -- Belgium - app
avTrafficDayBelApp = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 6]
  avTrafficDayBelApp[i] = mean(visitorsSumSubset)
}

#calculate average amount of broadcasts -- Netherlands
avBroadDayNet = matrix(NA, 24)
broadNet = broadNet[order(broadNet$date),]
for (i in 1:24){
  print(i)
  broadSubset = subset(broadNet, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayNet[i] = nrow(broadSubset)/amountDays
}
broadNet = broadNet[order(as.numeric(row.names(broadNet))),]

#calculate average amount of broadcasts -- Belgium
avBroadDayBel = matrix(NA, 24)
broadBel = broad_bel[order(broadBel$date),]
for (i in 1:24){
  print(i)
  broadSubset = subset(broadBel, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayBel[i] = nrow(broadSubset)/amountDays
}
broadBel = broadBel[order(as.numeric(row.names(broadBel))),]

#amount of advertisements per day -- Total
adAmount = matrix(0, amountDays) 
for (i in 1:amountDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broad$date == iDate)
  adAmount[i] = adsIDate
}

#amount of advertisements per day -- Netherlands
adAmountNet = as.matrix(table(broad_net$date))

#amount of advertisements per day -- Belgium
adAmountBel = as.matrix(table(broad_bel$date))

#amount of traffic per day -- Netherlands (approx. running time 5 seconds)
#trafAmountNet = as.matrix(table(traffic_net$date)) #how much traffic per day DOES NOT WORK CORRECTLY

#amount of traffic per day -- Belgium (approx. running time 5 seconds)
#trafAmountBel = as.matrix(table(traffic_bel$date)) DOES NOT WORK CORRECTLY

#amount of traffic per day -- Total
#NOTE: you can only run this if you have run both Net and Bel
#trafAmount = trafAmountNet + trafAmountBel

## ADDING DUMMIES FOR DAILY TRAFFIC (time series)

#national holidays
holidaysNames = c("Nieuwjaarsdag", "Goede Vrijdag", "Eerste Paasdag", 
                  "Tweede Paasdag", "Koningsdag", "Bevrijdingsdag", 
                  "Hemelvaartsdag", "Eerste Pinksterdag", 
                  "Tweede Pinksterdag")
holidaysDates = c("2019-01-01", "2019-04-19", "2019-04-21", "2019-04-22", 
                  "2019-04-27", "2019-05-05", "2019-05-30", "2019-06-09",
                  "2019-06-10")
dummyHolidays = matrix(rep(0), nrow = amountDays)
for (i in 1:length(holidaysDates)) {
  index = yday(holidaysDates[i])
  dummyHolidays[index] = 1
}
indexHemelvaart = yday(holidaysDates[7])
dummyHemelvaartsdag = matrix(rep(0), nrow = amountDays)
dummyHemelvaartsdag[indexHemelvaart] = 1

#weekday and week dummies
allDates = sort(unique(traffic$date))
allweekdays = weekdays(as.Date(allDates))

dummyWeekdays = dummy_cols(allweekdays) # column 2 = monday, 8 = sunday
dummyWeekdays = cbind(allDates, dummyWeekdays[, c(4, 2, 6, 3, 5, 7, 8)])
colnames(dummyWeekdays) = c("data", "mondag", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

#variable with weekdaysnames
broad$weekdays = 0
for (i in 1:nBroad){
  broad$weekdays[i] = weekdays(as.Date(broad$date[i]))
}

#month dummies
dummyMonths = dummy_cols(month(allDates))
dummyMonths = cbind(allDates, dummyMonths[, 2:7]) # column 2 = Jan, 7 = Jun
colnames(dummyMonths) = c("data", "January", "February", "March", "April", "May", "June")

#ads dummies
dummyAdsTot = matrix(rep(0), nrow = amountDays)
for (i in 1:length(uniqueDates)) {
  index = yday(uniqueDates[i])
  dummyAdsTot[index] = 1
}
dummyAdsNet = matrix(rep(0), nrow = amountDays)
for (i in 1:length(uniqueDatesNet)) {
  index = yday(uniqueDatesNet[i])
  dummyAdsNet[index] = 1
}
dummyAdsBel = matrix(rep(0), nrow = amountDays)
for (i in 1:length(uniqueDatesBel)) {
  index = yday(uniqueDatesBel[i])
  dummyAdsBel[index] = 1
}
dummyAds = cbind(dummyAdsTot, dummyAdsNet, dummyAdsBel) #1=Tot, 2=NL, 3=BE
colnames(dummyAds) = c("Ads Total","Ads Netherlands","Ads Belgium")
rm(dummyAdsTot); rm(dummyAdsNet); rm(dummyAdsBel)

## ADDING DUMMIES FOR COMMERCIALS (direct effects)

# Begin, middle and end for position in break
broad['position_in_break_3option'] = 0
for (i in 1:nBroad) {
  if(broad$position_in_break[i] == "0" || broad$position_in_break[i] == "1" || broad$position_in_break[i] == "2" ||
     broad$position_in_break[i] == "First Position" || broad$position_in_break[i] == "Second Position") {
    broad$position_in_break_3option[i] = "begin"
  } else if (broad$position_in_break[i] == "3" || broad$position_in_break[i] == "4" || broad$position_in_break[i] == "5" ||
             broad$position_in_break[i] == "6" || broad$position_in_break[i] == "7" || broad$position_in_break[i] == "8" ||
             broad$position_in_break[i] == "9" || broad$position_in_break[i] == "10" || broad$position_in_break[i] == "11" ||
             broad$position_in_break[i] == "12" || broad$position_in_break[i] == "13" || broad$position_in_break[i] == "14" ||
             broad$position_in_break[i] == "15" || broad$position_in_break[i] == "16" || broad$position_in_break[i] == "17" ||
             broad$position_in_break[i] == "18" || broad$position_in_break[i] == "19" || broad$position_in_break[i] == "20" ||
             broad$position_in_break[i] == "Any Other Position") {
    broad$position_in_break_3option[i] = "middle"
  } else if (broad$position_in_break[i] == "21" || broad$position_in_break[i] == "22" || broad$position_in_break[i] == "23" ||
             broad$position_in_break[i] == "24" || broad$position_in_break[i] == "25" || broad$position_in_break[i] == "98" ||
             broad$position_in_break[i] == "99" || broad$position_in_break[i] == "Before Last Position" || 
             broad$position_in_break[i] == "Last Position") {
    broad$position_in_break_3option[i] = "end"
  }
}

#overlap dummy
intervalSize = 2
intervalSizeOverlap = 2*intervalSize
broad = broad[order(broad$date_time),]
broad$overlap = 0
for (i in 1:nBroad){
  if (i %% 100 == 0){
    print(i)
  }
  datetime = broad$date_time[i]
  datetime = as.POSIXct(datetime)
  four_earlier = datetime - intervalSizeOverlap * 60
  four_later = datetime + intervalSizeOverlap * 60
  #4 minutes before
  if (i > 1){
    if (four_earlier <= broad$date_time[i - 1] && broad$date_time[i - 1] <= datetime){
      broad$overlap[i] = 1
    }
  }
  #4 minutes after
  if (i < nBroad){
    if (datetime <= broad$date_time[i + 1] && broad$date_time[i + 1] <= four_later){
      broad$overlap[i] = 1
    }
  }
}
broad = broad[order(as.numeric(row.names(broad))),]
broad = subset(broad, select = -date_time)

#hourly dummies
broad$hours = factor(floor(24*as.numeric(times(broad$time))))

#1. Product: Wasmachines, television, laptop
#2. Broadcast category: 7 
#3. TV channel: 51
#4. Commercial length: 30, 30+10, 30+10+5
#5. Position in break: beginning (1-3), middle (4-15), last (15-25??)
#6. Hour dummies

dummiesDirectModel = dummy_cols(.data = broad, select_columns = c("product_category", "channel", "length_of_spot", "position_in_break_3option", "weekdays"), remove_most_frequent_dummy = T)

dummiesDirectModelNeeded = dummiesDirectModel[,((ncol(broad)+1):ncol(dummiesDirectModel))]

dummiesDirectModelNeeded = as.data.frame(dummiesDirectModelNeeded)
#automate with non singular names \/
dummiesDirectModelNeeded = subset(dummiesDirectModelNeeded, select = -c(`channel_MTV (NL)`, `channel_RTL 5`, channel_SPIKE, 
                                                                        channel_Viceland, channel_VIER, channel_ZES)) # Exclude singularities
#broad = dummiesDirectModel # I am afraid to press this BUT this should include the dummy
dummiesDirectModelNoChannel = dummy_cols(.data = broad, select_columns = c("cluster", "product_category", "length_of_spot", "position_in_break_3option"), remove_first_dummy = T)
dummiesDirectModelNoChannel = dummiesDirectModelNoChannel[,((ncol(broad)+1):ncol(dummiesDirectModelNoChannel))]
dummiesDirectModelNoCluster = dummy_cols(.data = broad, select_columns = c("channel", "product_category", "length_of_spot", "position_in_break_3option", "program_category_before"), remove_most_frequent_dummy = T)
dummiesDirectModelNoCluster = dummiesDirectModelNoCluster[,((ncol(broad)+1):ncol(dummiesDirectModelNoCluster))]
dummiesDirectModelNoChannelNoProduct = subset(dummiesDirectModelNoChannel, select = -c(product_category_wasmachines, product_category_televisies)) # Exclude prod. cat

#broad data with broadcasts that have a gross rating higher than 0
#broadNonZeroGross = broad[broad[, "gross_rating_point"] > 0,]

dummiesDirectModelTime = dummy_cols(.data = broad, select_columns = c("cluster", "product_category", "length_of_spot", "position_in_break_3option", "weekdays"), remove_most_frequent_dummy = T)
dummiesDirectModelTime = dummiesDirectModelTime[,((ncol(broad)+1):ncol(dummiesDirectModelTime))]