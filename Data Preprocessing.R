#Installing packages (only once)
install.packages("chron")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("fastDummies")
install.packages("zoo")
install.packages("CausalImpact")
install.packages("factoextra")

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

#loading the data
traffic = read.csv(file.choose(), header = T)
broad = read.csv(file.choose(), header = T)

nTraffic = nrow(traffic)
nBroad = nrow(broad)

#now that packages and data has been loaded, we start by creating new features and set the data up in a nice and usable way

#First we add the column time_min to traffic and broad, which is the time in a scale of minutes (from 0 to 24*60= 1440)
#Also we add the column date to traffic

#add time_min to every broadcast
broad['time_min'] = 0
for (index in 1:nBroad) { #nBroad
  time = broad$time[[index]]
  timeMinute = 60 * 24 * as.numeric(times(time))
  broad$time_min[[index]] = timeMinute
}
rm(timeMinute)
rm(time)
rm(index)
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

#Further country specific variables + Aggregate clicks no a day

traffic_net = subset(traffic, country == 'Netherlands')
traffic_bel = subset(traffic, country == 'Belgium')
broad_net = subset(broad, country == 'Netherlands')
broad_bel = subset(broad, country == 'Belgium')

#amount of days in time-frame
amountDays = 31 + 28 + 31 + 30 + 31 + 30
scopeDays = amountDays
#set of unique advertising dates
uniqueDates = unique(broad$date)
uniqueDatesBel = unique(broad_bel$date)
uniqueDatesNet = unique(broad_net$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) #adverts in both on certain day
uniqueDatesOnlyBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) #adverts only in Belgium on certain day
uniqueDatesOnlyNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) #adverts only in Netherlands on certain day

#amount of advertisements per day -- Total
adAmount = matrix(0, scopeDays)
for (i in 1:scopeDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broad$date == iDate)
  adAmount[i] = adsIDate
}

#amount of advertisements per day -- Netherlands
adAmountNet = as.matrix(table(broad_net$date))

#amount of advertisements per day -- Belgium
adAmountBel = as.matrix(table(broad_bel$date))

#amount of traffic per day -- Netherlands (approx. running time 5 seconds)
trafAmountNet = as.matrix(table(traffic_net$date)) #how much traffic per day

#amount of traffic per day -- Belgium (approx. running time 5 seconds)
trafAmountBel = as.matrix(table(traffic_bel$date))

#amount of traffic per day -- Total
#NOTE: you can only run this if you have run both Net and Bel
trafAmount = trafAmountNet + trafAmountBel

## Adding (time series) dummy to the data

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

spotLengthDummies = matrix(rep(0),ncol = 3, nrow = nrow(broad), )
for (i in 1:nrow(broad)) {
  if(broad$length_of_spot[i] == "30"){spotLengthDummies[i,] = c(1,0,0) }
  if(broad$length_of_spot[i] == "30 + 10"){spotLengthDummies[i,] = c(0,1,0) }
  if(broad$length_of_spot[i] == "30 + 10 + 5"){spotLengthDummies[i,] = c(0,0,1) }
}

