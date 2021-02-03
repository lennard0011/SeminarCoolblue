# Seminar Coolblue 2021 -- Data Preprocessing (variables, dummies)
# @authors:

# Installing packages (only once, commented such that you can run full code at once)
#install.packages("chron")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("fastDummies")
#install.packages("zoo")
#install.packages("CausalImpact")
#install.packages("factoextra")
#install.packages("fastDummies")
#install.packages("AIC")
#install.packages("BIC")
#install.packages("Metrics")

# Adding packages
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
library("Metrics") # rmse calc
library("plyr")

# loading the data
traffic = read.csv(file.choose(), header = T)
broad = read.csv(file.choose(), header = T)

# option to leave out bounces
traffic = subset(traffic, bounces != 1 | is.na(bounces))
# option to leave out zero gross rating points
broad = broad[broad[, "gross_rating_point"] > 0,]

nTraffic = nrow(traffic)
nBroad = nrow(broad)


# now that packages and data have been loaded, we start by creating new features and set the data up in a nice and usable way
# First we add the column time_min to traffic and broad, which is the time in a scale of minutes (from 0 to 24*60= 1440)
# Also we add the column date to traffic
# add date+time to every broadcast

broad$date_time = 0
for (i in 1:nBroad){
  broad$date_time[i] = paste(broad$date[i], broad$time[i], sep = " ")
}

# add time_min to every broadcast
broad$time_min = 0
for (i in 1:nBroad) { # nBroad
  time = broad$time[i]
  timeMinute = 60 * 24 * as.numeric(times(time))
  broad$time_min[i] = timeMinute
}
rm(timeMinute)
rm(time)
# add time_min and date to every travel
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
print(paste0("Missing pairs Website-Belgium (incl. summer time): ", maxPairs-nrow(visWebBelSum)))

# Belgium - App
visAppBelSum = aggregate(visits_index ~ date + time_min + date, data = visAppBel, FUN=sum, simplify = TRUE, drop = TRUE)
names(visAppBelSum) = cbind("date", "time_min", "visitsAppBel")
print(paste0("Missing pairs App-Belgium (incl. summer time): ", maxPairs-nrow(visAppBelSum)))

# Concetenate all 4 pairs
visitorsSum = merge(merge(visWebNetSum, visAppNetSum, all = TRUE), merge(visWebBelSum, visAppBelSum, all = TRUE), all = TRUE)
visitorsSum[is.na(visitorsSum)] = 0

## ========================================================
## Insert summertime + 2 missing observations to visitorsSum
## ========================================================

summerTime = matrix(0.0, nrow = 60, ncol = 6)
colnames(summerTime) <- colnames(visitorsSum)
summerTime[,1] = "2019-03-31"
for (i in 1:60) {
  summerTime[i,2] = 119+i
  summerTime[i,3] = visitorsSum[(128279+i-1440),3]
  summerTime[i,4] = visitorsSum[(128279+i-1440),4]
  summerTime[i,5] = visitorsSum[(128279+i-1440),5]
  summerTime[i,6] = visitorsSum[(128279+i-1440),6]
}
visitorsSum = rbind(visitorsSum[1:128279,], summerTime, visitorsSum[128280:nrow(visitorsSum),])
row.names(visitorsSum) <- NULL # resets rownrs
rm(summerTime)
nrow(visitorsSum)

# Insert 2 remaining missing values
visitorsSum = rbind(visitorsSum[1:118393,], c("2019-03-24",313,0.0,0.0,0.0,0.0), visitorsSum[118394:nrow(visitorsSum),])
visitorsSum = rbind(visitorsSum[1:148547,], c("2019-04-14",228,0.0,0.0,0.0,0.0), visitorsSum[148548:nrow(visitorsSum),])
row.names(visitorsSum) <- NULL

# Convert non-numeric vectors to doubles
visitorsSum$visitsWebNet = as.numeric(visitorsSum$visitsWebNet)
visitorsSum$visitsAppNet = as.numeric(visitorsSum$visitsAppNet)
visitorsSum$visitsWebBel = as.numeric(visitorsSum$visitsWebBel)
visitorsSum$visitsAppBel = as.numeric(visitorsSum$visitsAppBel)

## Aggregate visit density over the days, 4 pairs of combinations
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
  daysVisitorsSum[day, 2] = as.numeric(daysVisitorsSum[day, 2]) + as.numeric(visitIndexWebNet)
  daysVisitorsSum[day, 3] = as.numeric(daysVisitorsSum[day, 3]) + as.numeric(visitIndexAppNet)
  daysVisitorsSum[day, 4] = as.numeric(daysVisitorsSum[day, 4]) + as.numeric(visitIndexWebBel)
  daysVisitorsSum[day, 5] = as.numeric(daysVisitorsSum[day, 5]) + as.numeric(visitIndexAppBel)
}

# Further country specific variables + Aggregate clicks no a day
trafficNet = subset(traffic, country == 'Netherlands')
trafficBel = subset(traffic, country == 'Belgium')
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

# set of unique advertising dates
uniqueDates = unique(broad$date)
uniqueDatesBel = unique(broadBel$date)
uniqueDatesNet = unique(broadNet$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) # adverts in both on certain day
uniqueDatesOnlyBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) # adverts only in Belgium on certain day
uniqueDatesOnlyNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) # adverts only in Netherlands on certain day

# TODO: this code doesn't work ATM.
# Data for plot average of hour over the days
# calculate average for different searches -- Netherlands - website
avTrafficDayNetWebsite = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 3]
  avTrafficDayNetWebsite[i] = mean(visitorsSumSubset)
}

# calculate average for different searches -- Netherlands - app
avTrafficDayNetApp = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 4]
  avTrafficDayNetApp[i] = mean(visitorsSumSubset)
}

# calculate average for different searches -- Belgium - website
avTrafficDayBelWebsite = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 5]
  avTrafficDayBelWebsite[i] = mean(visitorsSumSubset)
}

# calculate average for different searches -- Belgium - app
avTrafficDayBelApp = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 6]
  avTrafficDayBelApp[i] = mean(visitorsSumSubset)
}

# calculate average amount of broadcasts -- Netherlands
avBroadDayNet = matrix(NA, 24)
broadNet = broadNet[order(broadNet$date),]
for (i in 1:24){
  broadSubset = subset(broadNet, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayNet[i] = nrow(broadSubset)/amountDays
}
broadNet = broadNet[order(as.numeric(row.names(broadNet))),]

# calculate average amount of broadcasts -- Belgium
avBroadDayBel = matrix(NA, 24)
broadBel = broadBel[order(broadBel$date),]
for (i in 1:24){
  broadSubset = subset(broadBel, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayBel[i] = nrow(broadSubset)/amountDays
}
broadBel = broadBel[order(as.numeric(row.names(broadBel))),]


# amount of advertisements per day -- Total
adAmount = matrix(0, amountDays) 
for (i in 1:amountDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broad$date == iDate)
  adAmount[i] = adsIDate
}

# amount of advertisements per day 
adAmountNet = as.matrix(table(broadNet$date))
adAmountBel = as.matrix(table(broadBel$date))

## ========================================================
##      CREATING DUMMIES FOR DAILY TRAFFIC (time series)
## ========================================================

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

# weekday and week dummies
allDates = sort(unique(traffic$date))
allweekdays = weekdays(as.Date(allDates))

dummyWeekdays = dummy_cols(allweekdays) # column 2 = monday, 8 = sunday
dummyWeekdays = cbind(allDates, dummyWeekdays[, c(4, 2, 6, 3, 5, 7, 8)])
colnames(dummyWeekdays) = c("data", "mondag", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

# month dummies
dummyMonths = dummy_cols(month(allDates))
dummyMonths = cbind(allDates, dummyMonths[, 2:7]) # column 2 = Jan, 7 = Jun
colnames(dummyMonths) = c("data", "January", "February", "March", "April", "May", "June")

# ads dummies
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
dummyAds = cbind(dummyAdsTot, dummyAdsNet, dummyAdsBel) # 1=Tot, 2=NL, 3=BE
colnames(dummyAds) = c("Ads Total","Ads Netherlands","Ads Belgium")
rm(dummyAdsTot); rm(dummyAdsNet); rm(dummyAdsBel)

## ========================================================
##    ADDING DUMMIES FOR COMMERCIALS (direct effects)
## ========================================================

# Weekdays dummiess
broad$weekdays = 0
for (i in 1:nBroad){
  broad$weekdays[i] = weekdays(as.Date(broad$date[i]))
}

# Hourly dummies
broad$hours = factor(floor(24*as.numeric(times(broad$time))))

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

# Overlap dummy
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')
intervalSize = 5
#intervalSize = 20
iNet = 0
iBel = 0
broad = broad[order(broad$date_time),]
broadNet = broadNet[order(broadNet$date_time),]
broadBel = broadBel[order(broadBel$date_time),]
broad$overlapBefore = 0
broad$overlapAfter = 0
for (i in 1:nrow(broad)){
  # en wat bij middernacht?
  if (broad$country[i] == 'Netherlands'){
    iNet = iNet + 1
    #print(i)
    datetime = broad$date_time[i]
    datetime = as.POSIXct(datetime)
    timeEarlier = datetime - intervalSize * 60
    timeLater = datetime + intervalSize * 60
    # Interval before
    if (iNet > 1){ # exclude first dutch commercial
      if (timeEarlier <= broadNet$date_time[iNet - 1] && broadNet$date_time[iNet - 1] <= datetime){
        broad$overlapBefore[i] = 1
      }
    }
    # Interval after
    if (iNet < nrow(broadNet)){ # exclude last dutch commercial
      if (datetime <= broadNet$date_time[iNet + 1] && broadNet$date_time[iNet + 1] <= timeLater){
        broad$overlapAfter[i] = 1
      }
    }
  }
  if (broad$country[i] == 'Belgium'){
    iBel = iBel + 1
    #print(i)
    datetime = broad$date_time[i]
    datetime = as.POSIXct(datetime)
    timeErlier = datetime - intervalSize * 60
    timeLater = datetime + intervalSize * 60
    # Interval before
    if (iBel > 1){
      if (timeEarlier <= broadBel$date_time[iBel - 1] && broadBel$date_time[iBel - 1] <= datetime){
        broad$overlapBefore[i] = 1
      }
    }
    # Interval after
    if (iBel < nrow(broadBel)){
      if (datetime <= broadBel$date_time[iBel + 1] && broadBel$date_time[iBel + 1] <= timeLater){
        broad$overlapAfter[i] = 1
      }
    }
  }
}
broad = broad[order(as.numeric(row.names(broad))),]
broadNet = broadNet[order(as.numeric(row.names(broadNet))),]
broadBel = broadBel[order(as.numeric(row.names(broadBel))),]

## CREATING MATRICES WITH DUMMIES ##########################################

# Include in baseline models
# preVisitors
# hour (7-1)
# weekday (23-1)

# Include in full model
# The dummies we include in the treatment effect (we EXCLUDE the most freq. dummy)
  # 1. Product category: (3-1) ("wasmachines", "televisies", "laptops")
  # 2. TV channel (max. 51-1)
  # 2. Length of spot: (3-1) ("30", "30+10", "30+10+5")
  # 3. Position in break: (3-1) ("begin", "middle", "end")
  # 5. Overlap with other commercial: 2 (overlap_before, overlap_after)
  # 6. GRP (no dummy)

# We will continue with Web-NL
broadNet = subset(broad, country == "Netherlands")

# dummiesDirectModel Netherlands contains the treatment variables
variablesDirectModel = c("product_category", "channel", "length_of_spot", "position_in_break_3option", "weekdays", "overlapBefore", "overlapAfter")
dummiesDirectModelPre = dummy_cols(.data = broadNet, select_columns = variablesDirectModel, remove_most_frequent_dummy = T)
dummiesDirectModel = dummiesDirectModelPre[,((ncol(broadNet)+1):ncol(dummiesDirectModelPre))]
dummiesDirectModel = as.data.frame(dummiesDirectModel)
rm(dummiesDirectModelPre); rm(variablesDirectModel)

# dummiesDirectModel for Belgium
# TODO: overlap dummies komen niet in Belgische data
variablesDirectModel = c("channel", "position_in_break_3option", "weekdays", "overlapBefore", "overlapAfter") # waarom missen 2 dummie-var?
dummiesDirectModelPre = dummy_cols(.data = broadBel, select_columns = variablesDirectModel, remove_most_frequent_dummy = T)
dummiesDirectModel = dummiesDirectModelPre[,((ncol(broadBel)+1):ncol(dummiesDirectModelPre))]
dummiesDirectModel = as.data.frame(dummiesDirectModel)
rm(dummiesDirectModelPre); rm(variablesDirectModel)

# automate with non singular names \/
removeNonSingular <- function(model, data) {
  naCoef = names(which(is.na(coef(model))))
  naCoef = gsub('`', '', naCoef)
  data = data[, !(names(data) %in% naCoef )]
  data
}
