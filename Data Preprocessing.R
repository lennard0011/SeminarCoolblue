# Seminar Coolblue 2021 -- Data Preprocessing
# @authors: Lennard van der Plas, Erik van der Heide, Marjolein de With, Daniel Buijs

# install packages (uncomment to install a package)
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
#install.packages("gtrendsR")
#install.packages("seastests")
#install.packages("pastecs")
#install.packages("plotrix")
#install.packages("varhandle")
#install.packages('write.xl')

# loading packages
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
library("lmtest") 
library("sandwich") 
library("stats")
library("Metrics")
library("plyr")
library("gtrendsR")
library("tseries")
library("seastests")
library("pastecs")
library("plotrix")
library("varhandle")
library("write.xl")

## ====================================================
##         Loading & subsetting the data
## ====================================================

# Loading data
traffic = read.csv(file.choose(), header = T)
broad = read.csv(file.choose(), header = T)

# Subset data
traffic = subset(traffic, bounces != 1 | is.na(bounces)) # Leave out bounces = 1

broad = broad[broad[, "gross_rating_point"] > 0, ]  # Leave out GRP = 0

broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

# Broad for Netherlands and Belgium
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

fiveBeforeMidnight = 60 * 24 - 5
broadNetNoMidnight = subset(broadNet, broadNet$time_min < fiveBeforeMidnight)
broadNetNoMidnight = subset(broadNetNoMidnight, broadNetNoMidnight$time_min > 5)

broadNet = broadNetNoMidnight

broadBelNoMidnight = subset(broadBel, broadBel$time_min < fiveBeforeMidnight)
broadBelNoMidnight = subset(broadBelNoMidnight, broadBelNoMidnight$time_min > 5)

nTraffic = nrow(traffic)
nBroad = nrow(broad)

# Now that packages and data have been loaded, we start by creating new features and set the data up in a nice and usable way
# First we add the column time_min to traffic and broad, which is the time in a scale of minutes (from 0 to 24*60= 1440)
# Also we add the column date to traffic

## ============================================================
##      Data manipulation: Calculate minute of day
## ============================================================

# Add date+time to every broadcast
broad$date_time = 0
for (i in 1:nBroad){
  broad$date_time[i] = paste(broad$date[i], broad$time[i], sep = " ")
}

# Add time_min to every broadcast
broad$time_min = 0
for (i in 1:nBroad) {
  time = broad$time[i]
  timeMinute = 60 * 24 * as.numeric(times(time))
  broad$time_min[i] = timeMinute
}
rm(timeMinute)
rm(time)

# Add time_min and date to every traffic
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

## ========================================================
##   Separate Web/App/Net/Bel and aggregate visit_index
## ========================================================

# Separate in 4 combinations
visWebNet = traffic[traffic$medium == "website" & traffic$country == "Netherlands" & traffic$visit_source != "push notification", ]
visAppNet = traffic[traffic$medium == "app" & traffic$country == "Netherlands" & traffic$visit_source != "push notification", ]
visWebBel = traffic[traffic$medium == "website" & traffic$country == "Belgium" & traffic$visit_source != "push notification", ]
visAppBel = traffic[traffic$medium == "app" & traffic$country == "Belgium" & traffic$visit_source != "push notification", ]

# Aggregate the visit_index per minute
amountDays = NROW(unique(traffic$date))
minutesPerDay = 60 * 24
maxPairs = amountDays * minutesPerDay

# Website - Netherlands
visWebNetSum = aggregate(visits_index ~ date + time_min, data = visWebNet, FUN=sum, simplify = T, drop = T)
names(visWebNetSum) = cbind("date", "time_min", "visitsWebNet")
print(paste0("Missing pairs Website-Netherlands (incl. summer time): ", maxPairs-nrow(visWebNetSum)))

# App - Netherlands
visAppNetSum = aggregate(visits_index ~ date +  time_min + date, data = visAppNet, FUN=sum, simplify = T, drop = T)
names(visAppNetSum) = cbind("date", "time_min", "visitsAppNet")
print(paste0("Missing pairs App-Netherlands (incl. summer time): ", maxPairs-nrow(visAppNetSum)))

# Website - Belgium
visWebBelSum = aggregate(visits_index ~ date + time_min + date, data = visWebBel, FUN=sum, simplify = T, drop = T)
names(visWebBelSum) = cbind("date", "time_min", "visitsWebBel")
print(paste0("Missing pairs Website-Belgium (incl. summer time): ", maxPairs-nrow(visWebBelSum)))

# App - Belgium
visAppBelSum = aggregate(visits_index ~ date + time_min + date, data = visAppBel, FUN=sum, simplify = T, drop = T)
names(visAppBelSum) = cbind("date", "time_min", "visitsAppBel")
print(paste0("Missing pairs App-Belgium (incl. summer time): ", maxPairs-nrow(visAppBelSum)))

# Concatenate all 4 pairs in visitorsSum
visitorsSum = merge(merge(visWebNetSum, visAppNetSum, all = T), merge(visWebBelSum, visAppBelSum, all = T), all = T)
visitorsSum[is.na(visitorsSum)] = 0

## ============================================================
##  Insert summertime + missing observations to visitorsSum
## ============================================================

# Include summertime by taking the value of previous day
summerTime = matrix(0.0, nrow = 60, ncol = 6)
colnames(summerTime) = colnames(visitorsSum)
summerTime[, 1] = "2019-03-31"
for (i in 1:60) {
  summerTime[i, 2] = 119 + i
  summerTime[i, 3] = visitorsSum[(128279 + i - 1440), 3]
  summerTime[i, 4] = visitorsSum[(128279 + i - 1440), 4]
  summerTime[i, 5] = visitorsSum[(128279 + i - 1440), 5]
  summerTime[i, 6] = visitorsSum[(128279 + i - 1440), 6]
}
visitorsSum = rbind(visitorsSum[1:128279, ], summerTime, visitorsSum[128280:nrow(visitorsSum), ])
row.names(visitorsSum) = NULL # Resets rownumbers
rm(summerTime)

# Insert 2 remaining missing values
visitorsSum = rbind(visitorsSum[1:118393, ], c("2019-03-24", 313, 0.0, 0.0, 0.0, 0.0), visitorsSum[118394:nrow(visitorsSum), ])
visitorsSum = rbind(visitorsSum[1:148547, ], c("2019-04-14", 228, 0.0, 0.0, 0.0, 0.0), visitorsSum[148548:nrow(visitorsSum), ])
row.names(visitorsSum) = NULL

# Convert non-numeric vectors to doubles
visitorsSum$time_min = as.numeric(visitorsSum$time_min)
visitorsSum$visitsWebNet = as.numeric(visitorsSum$visitsWebNet)
visitorsSum$visitsAppNet = as.numeric(visitorsSum$visitsAppNet)
visitorsSum$visitsWebBel = as.numeric(visitorsSum$visitsWebBel)
visitorsSum$visitsAppBel = as.numeric(visitorsSum$visitsAppBel)

## ===============================================
##   Aggregate visit densities over the days
## ===============================================

# Aggregate visit density over the days, 4 pairs of combinations
uniqueDates = unique(traffic$date) 
uniqueDates = sort(uniqueDates)
daysVisitorsSum = matrix(0.0, nrow = amountDays, ncol = 5)
colnames(daysVisitorsSum) = c("date", "visitsWebNetDay", "visitsAppNetDay", "visitsWebBelDay", "visitsAppBelDay")
daysVisitorsSum[, 1] = uniqueDates
for (i in 1:nrow(visitorsSum)) { # Takes at most 2 minutes to run
  day = yday(visitorsSum$date[i])
  daysVisitorsSum[day, 2] = as.numeric(daysVisitorsSum[day, 2]) + visitorsSum$visitsWebNet[i]
  daysVisitorsSum[day, 3] = as.numeric(daysVisitorsSum[day, 3]) + visitorsSum$visitsAppNet[i]
  daysVisitorsSum[day, 4] = as.numeric(daysVisitorsSum[day, 4]) + visitorsSum$visitsWebBel[i]
  daysVisitorsSum[day, 5] = as.numeric(daysVisitorsSum[day, 5]) + visitorsSum$visitsAppBel[i]
}

## ==========================================
##     Calculate averages for Net & Bel
## ==========================================

broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

# Set of unique advertising dates
uniqueDates = unique(broad$date)
uniqueDatesBel = unique(broadBel$date)
uniqueDatesNet = unique(broadNet$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) # Adverts in both on certain day
uniqueDatesOnlyBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) # Adverts only in Belgium on certain day
uniqueDatesOnlyNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) # Adverts only in Netherlands on certain day

# Data for plot average of hour over the days
# Calculate average for different searches Netherlands - website
avTrafficDayNetWebsite = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 3]
  avTrafficDayNetWebsite[i] = mean(visitorsSumSubset)
}

# Calculate average for different searches Netherlands - app
avTrafficDayNetApp = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 4]
  avTrafficDayNetApp[i] = mean(visitorsSumSubset)
}

# Calculate average for different searches Belgium - website
avTrafficDayBelWebsite = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 5]
  avTrafficDayBelWebsite[i] = mean(visitorsSumSubset)
}

# Calculate average for different searches Belgium - app
avTrafficDayBelApp = matrix(NA, 24)
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))[, 6]
  avTrafficDayBelApp[i] = mean(visitorsSumSubset)
}

# Calculate average amount of broadcasts Netherlands
avBroadDayNet = matrix(NA, 24)
broadNet = broadNet[order(broadNet$date), ]
for (i in 1:24){
  broadSubset = subset(broadNet, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayNet[i] = nrow(broadSubset)/amountDays
}
broadNet = broadNet[order(as.numeric(row.names(broadNet))), ]

# Calculate average amount of broadcasts Belgium
avBroadDayBel = matrix(NA, 24)
broadBel = broadBel[order(broadBel$date), ]
for (i in 1:24){
  broadSubset = subset(broadBel, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayBel[i] = nrow(broadSubset)/amountDays
}
broadBel = broadBel[order(as.numeric(row.names(broadBel))), ]

# Amount of advertisements per day Total
adAmount = matrix(0, amountDays) 
for (i in 1:amountDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broad$date == iDate)
  adAmount[i] = adsIDate
}

# Amount of advertisements per day 
adAmountNet = as.matrix(table(broadNet$date))
adAmountBel = as.matrix(table(broadBel$date))

## ========================================================
##      CREATING DUMMIES FOR DAILY TRAFFIC (time series)
## ========================================================

# Weekday and week dummies
allDates = sort(unique(traffic$date))
allweekdays = weekdays(as.Date(allDates))

dummyWeekdays = dummy_cols(allweekdays) # Column 2 = monday, 8 = sunday
dummyWeekdays = cbind(allDates, dummyWeekdays[, c(4, 2, 6, 3, 5, 7, 8)])
colnames(dummyWeekdays) = c("data", "mondag", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

# Month dummies
dummyMonths = dummy_cols(month(allDates))
dummyMonths = cbind(allDates, dummyMonths[, 2:7]) # Column 2 = Jan, 7 = Jun
colnames(dummyMonths) = c("data", "January", "February", "March", "April", "May", "June")

# Ads dummies
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
dummyAds = cbind(dummyAdsTot, dummyAdsNet, dummyAdsBel) # 1 = Total, 2 = Netherlands, 3 = Belgium
colnames(dummyAds) = c("Ads Total", "Ads Netherlands", "Ads Belgium")
rm(dummyAdsTot); rm(dummyAdsNet); rm(dummyAdsBel)

## ========================================================
##    Create dummies for commercials (direct effects)
## ========================================================

# Weekdays dummiess
broad$weekdays = 0
for (i in 1:nBroad){
  broad$weekdays[i] = weekdays(as.Date(broad$date[i]))
}

# Hourly dummies
broad$hours = factor(floor(24 * as.numeric(times(broad$time))))

# Begin, middle and end for position in break
broad['position_in_break_3option'] = 0
for (i in 1:nBroad) {
  if(broad$position_in_break[i] == "0" || broad$position_in_break[i] == "1" || broad$position_in_break[i] == "2" ||
     broad$position_in_break[i] == "First Position" || broad$position_in_break[i] == "Second Position") {
    broad$position_in_break_3option[i] = "begin"
  } else if(broad$position_in_break[i] == "3" || broad$position_in_break[i] == "4" || broad$position_in_break[i] == "5" ||
             broad$position_in_break[i] == "6" || broad$position_in_break[i] == "7" || broad$position_in_break[i] == "8" ||
             broad$position_in_break[i] == "9" || broad$position_in_break[i] == "10" || broad$position_in_break[i] == "11" ||
             broad$position_in_break[i] == "12" || broad$position_in_break[i] == "13" || broad$position_in_break[i] == "14" ||
             broad$position_in_break[i] == "15" || broad$position_in_break[i] == "16" || broad$position_in_break[i] == "17" ||
             broad$position_in_break[i] == "18" || broad$position_in_break[i] == "19" || broad$position_in_break[i] == "20" ||
             broad$position_in_break[i] == "Any Other Position") {
    broad$position_in_break_3option[i] = "middle"
  } else if(broad$position_in_break[i] == "21" || broad$position_in_break[i] == "22" || broad$position_in_break[i] == "23" ||
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
iNet = 0
iBel = 0
broad = broad[order(broad$date_time), ]
broadNet = broadNet[order(broadNet$date_time), ]
broadBel = broadBel[order(broadBel$date_time), ]
broad$overlapBefore = 0
broad$overlapAfter = 0
for (i in 1:nrow(broad)){
  if(broad$country[i] == 'Netherlands'){
    iNet = iNet + 1
    datetime = broad$date_time[i]
    datetime = as.POSIXct(datetime)
    timeEarlier = datetime - intervalSize * 60
    timeLater = datetime + intervalSize * 60
    # Interval before
    if(iNet > 1){ # Exclude first Dutch commercial
      if(timeEarlier <= broadNet$date_time[iNet - 1] && broadNet$date_time[iNet - 1] <= datetime){
        broad$overlapBefore[i] = 1
      }
    }
    # Interval after
    if(iNet < nrow(broadNet)){ # Exclude last Dutch commercial
      if(datetime <= broadNet$date_time[iNet + 1] && broadNet$date_time[iNet + 1] <= timeLater){
        broad$overlapAfter[i] = 1
      }
    }
  }
  if(broad$country[i] == 'Belgium'){
    iBel = iBel + 1
    datetime = broad$date_time[i]
    datetime = as.POSIXct(datetime)
    timeEarlier = datetime - intervalSize * 60
    timeLater = datetime + intervalSize * 60
    # Interval before
    if(iBel > 1){
      if(timeEarlier <= broadBel$date_time[iBel - 1] && broadBel$date_time[iBel - 1] <= datetime){
        broad$overlapBefore[i] = 1
      }
    }
    # Interval after
    if(iBel < nrow(broadBel)){
      if(datetime <= broadBel$date_time[iBel + 1] && broadBel$date_time[iBel + 1] <= timeLater){
        broad$overlapAfter[i] = 1
      }
    }
  }
}
broad = broad[order(as.numeric(row.names(broad))), ]
broadNet = broadNet[order(as.numeric(row.names(broadNet))), ]
broadBel = broadBel[order(as.numeric(row.names(broadBel))), ]
broadNet = subset(broad, country == 'Netherlands')
broadBel = subset(broad, country == 'Belgium')

## =================================================
##   Create matrices with dummies (Direct Effects)
## =================================================

# We will continue with Web-NL
broadNet = subset(broad, country == "Netherlands")
broadBel = subset(broad, country == "Belgium")

# DummiesDirectModel Netherlands contains the treatment variables
variablesDirectModel = c("product_category", "channel", "length_of_spot", "position_in_break_3option", "weekdays", "overlapBefore", "overlapAfter")
dummiesDirectModelPre = dummy_cols(.data = broadNet, select_columns = variablesDirectModel, remove_most_frequent_dummy = T)
dummiesDirectModel = dummiesDirectModelPre[,((ncol(broadNet) + 1):ncol(dummiesDirectModelPre))]
dummiesDirectModel = as.data.frame(dummiesDirectModel)
rm(dummiesDirectModelPre); rm(variablesDirectModel)

# DummiesDirectModel for Belgium
variablesDirectModelBel = c("channel", "position_in_break_3option", "weekdays", "overlapBefore", "overlapAfter") # waarom missen 2 dummie-var?
dummiesDirectModelPreBel = dummy_cols(.data = broadBel, select_columns = variablesDirectModelBel, remove_most_frequent_dummy = T)
dummiesDirectModelBel = dummiesDirectModelPreBel[, ((ncol(broadBel) + 1):ncol(dummiesDirectModelPreBel))]
dummiesDirectModelBel = as.data.frame(dummiesDirectModelBel)
rm(dummiesDirectModelPreBel); rm(variablesDirectModelBel)

# Matrix for table statistics Netherlands
uni = unique(broadNet$channel)
mat = data.frame(matrix(0, nrow=length(uni), ncol = 14))
for(i in 1:length(uni)){
  mat[i, 1] = uni[i]
  mat[i, 2] = sum(broadNet$channel == uni[i])
  sub = subset(broadNet, channel == uni[i])
  mat[i, 3] = sum(sub$position_in_break_3option == "begin")
  mat[i, 4] = sum(sub$position_in_break_3option == "middle")
  mat[i, 5] =  sum(sub$position_in_break_3option == "end")
  mat[i, 6] = sum(sub$product_category == "televisies")
  mat[i, 7] = sum(sub$product_category == "laptops")
  mat[i, 8] = sum(sub$product_category == "wasmachines")
  uniprod = unique(sub$program_category_before)
  mat2 = data.frame(matrix(0,nrow = length(uniprod), ncol = 2))
  colnames(mat2)= c("category", "number")
  for(j in 1:length(uniprod)){
    mat2[j, 1] = uniprod[j]
    mat2[j, 2] = sum(sub$program_category_before == uniprod[j])
  }
  mat2 =  mat2[order(mat2$number, decreasing = T), ]
  mat[i, 9] = mat2[1, 1]
  
  uniprod = unique(sub$program_category_after)
  mat2 = data.frame(matrix(0,nrow = length(uniprod), ncol = 2))
  colnames(mat2)= c("category", "number")
  for(j in 1:length(uniprod)){
    mat2[j,1] = uniprod[j]
    mat2[j,2] = sum(sub$program_category_after == uniprod[j])
  }
  mat2 =  mat2[order(mat2$number, decreasing = T), ]
  mat[i, 10] = mat2[1, 1]
  mat[i, 11] = sum(sub$length_of_spot == "30")
  mat[i, 12] = sum(sub$length_of_spot == "30 + 10")
  mat[i, 13] = sum(sub$length_of_spot == "30 + 10 + 5")
  mat[i, 14] = mean(sub$gross_rating_point)
}
colnames(mat) = c("Channel", "Commercials", "Begin", "Middle", "End", "televisions", "laptops", "wasching machines",
                  "Freq. category before", "Freq. category after", "30", "30 + 10", "30 + 10 + 5", "GRP")
row.names(mat) = mat[, 1]
statNet = mat[-1]

# Matrix for table statistics Belgium
uni = unique(broadBel$channel)
mat = data.frame(matrix(0,nrow=length(uni), ncol = 14))
for(i in 1:length(uni)){
  mat[i, 1] = uni[i]
  mat[i, 2] = sum(broadBel$channel == uni[i])
  sub = subset(broadBel, channel == uni[i])
  mat[i, 3] = sum(sub$position_in_break_3option == "begin")
  mat[i, 4] = sum(sub$position_in_break_3option == "middle")
  mat[i, 5] =  sum(sub$position_in_break_3option == "end")
  mat[i, 6] = sum(sub$product_category == "televisies")
  mat[i, 7] = sum(sub$product_category == "laptops")
  mat[i, 8] = sum(sub$product_category == "wasmachines")
  uniprod = unique(sub$program_category_before)
  mat2 = data.frame(matrix(0,nrow = length(uniprod), ncol = 2))
  colnames(mat2)= c("category", "number")
  for(j in 1:length(uniprod)){
    mat2[j, 1] = uniprod[j]
    mat2[j, 2] = sum(sub$program_category_before == uniprod[j])
  }
  mat2 =  mat2[order(mat2$number, decreasing = T), ]
  mat[i, 9] = mat2[1, 1]
  
  uniprod = unique(sub$program_category_after)
  mat2 = data.frame(matrix(0,nrow = length(uniprod), ncol = 2))
  colnames(mat2)= c("category", "number")
  for(j in 1:length(uniprod)){
    mat2[j, 1] = uniprod[j]
    mat2[j, 2] = sum(sub$program_category_after == uniprod[j])
  }
  mat2 =  mat2[order(mat2$number, decreasing = T), ]
  mat[i, 10] = mat2[1, 1]
  mat[i, 11] = sum(sub$length_of_spot == "30")
  mat[i, 12] = sum(sub$length_of_spot == "30 + 10")
  mat[i, 13] = sum(sub$length_of_spot == "30 + 10 + 5")
  mat[i, 14] = mean(sub$gross_rating_point)
}
colnames(mat) = c("Channel", "Commercials", "Begin", "Middle", "End", "televisions", "laptops", "wasching machines",
                  "Freq. category before", "Freq. category after", "30", "30 + 10", "30 + 10 + 5", "GRP")
row.names(mat) = mat[, 1]
statBel = mat[-1]

save(broadNet, file = "broadNet.rda")
save(visitorsSum, file = "visitorsSum.rda")