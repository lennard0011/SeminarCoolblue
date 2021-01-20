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

## K-MEANS dummies for different channel categories
channel = unique(broad$channel)
channel = as.data.frame(channel)
nChan = nrow(channel)
#channel country
channel$country = 0
for (i in 1:nChan){
  print(i)
  for (j in 1:nBroad){
    if (channel[i, 1] == broad[j, ]$channel){
      if (broad[j, ]$country == "Netherlands"){
        channel$country[i] = 1
      }
      break
    }
  }
}
#gender
channel$women = 0
channel$men = 0
channel$men[3] = 1
channel$women[7] = 1
channel$women[13] = 1
channel$women[15] = 1
channel$men[19] = 1
channel$women[20] = 1
channel$men[24] = 1
channel$men[25] = 1
channel$women[27] = 1
channel$women[33] = 1
channel$men[35] = 1
#music
channel$music = 0
channel$music[16] = 1
channel$music[26] = 1
channel$music[29] = 1
channel$music[38] = 1
#sport
channel$sport = 0
channel$sport[36] = 1
channel$sport[40] = 1
channel$sport[41] = 1
channel$sport[42] = 1
#age
channel$youth = 0
channel$youth[6] = 1
channel$youth[12] = 1
channel$youth[18] = 1
#kmeans
# Elbow method
scaled_channel = scale(channel[2:7])
set.seed(21)
kmean = kmeans(scaled_channel, 6)
cluster_1 = channel[,1][kmean$cluster == 1]
cluster_2 = channel[,1][kmean$cluster == 2]
cluster_3 = channel[,1][kmean$cluster == 3]
cluster_4 = channel[,1][kmean$cluster == 4]
cluster_5 = channel[,1][kmean$cluster == 5]
cluster_6 = channel[,1][kmean$cluster == 6]
cluster_7 = channel[,1][kmean$cluster == 7]
broad$cluster = 0
for (i in 1:nBroad){
  if (broad$channel[i] %in% cluster_1){
    broad$cluster[i] = 1
  }
  if (broad$channel[i] %in% cluster_2){
    broad$cluster[i] = 2
  }
  if (broad$channel[i] %in% cluster_3){
    broad$cluster[i] = 3
  }
  if (broad$channel[i] %in% cluster_4){
    broad$cluster[i] = 4
  }
  if (broad$channel[i] %in% cluster_5){
    broad$cluster[i] = 5
  }
  if (broad$channel[i] %in% cluster_6){
    broad$cluster[i] = 6
  }
}

# Begin, middle and end for position in break
broad['position_in_break_3option'] = 0
for (i in 1:nBroad) {
  if(broad$position_in_break[i] == "0" || broad$position_in_break[i] == "1" || broad$position_in_break[i] == "2" ||
     broad$position_in_break[i] == "First Position" || broad$position_in_break[i] == "Second Position") {
    broad$position_in_break_3option[i] <- "begin"
  } else if (broad$position_in_break[i] == "3" || broad$position_in_break[i] == "4" || broad$position_in_break[i] == "5" ||
             broad$position_in_break[i] == "6" || broad$position_in_break[i] == "7" || broad$position_in_break[i] == "8" ||
             broad$position_in_break[i] == "9" || broad$position_in_break[i] == "10" || broad$position_in_break[i] == "11" ||
             broad$position_in_break[i] == "12" || broad$position_in_break[i] == "13" || broad$position_in_break[i] == "14" ||
             broad$position_in_break[i] == "15" || broad$position_in_break[i] == "16" || broad$position_in_break[i] == "17" ||
             broad$position_in_break[i] == "18" || broad$position_in_break[i] == "19" || broad$position_in_break[i] == "20" ||
             broad$position_in_break[i] == "Any Other Position") {
    broad$position_in_break_3option[i] <- "middle"
  } else if (broad$position_in_break[i] == "21" || broad$position_in_break[i] == "22" || broad$position_in_break[i] == "23" ||
             broad$position_in_break[i] == "24" || broad$position_in_break[i] == "25" || broad$position_in_break[i] == "98" ||
             broad$position_in_break[i] == "99" || broad$position_in_break[i] == "Before Last Position" || 
             broad$position_in_break[i] == "Last Position") {
    broad$position_in_break_3option[i] = "end"
  }
}

#1. Product: Wasmachines, television, laptop
#2. Broadcast category: 7 
#3. TV channel: 51
#4. Commercial length: 30, 30+10, 30+10+5
#5. Position in break: beginning (1-3), middle (4-15), last (15-25??)
dummiesDirectModel = dummy_cols(.data = broad, select_columns = c("cluster", "product_category", "channel", "length_of_spot", "position_in_break_3option"), remove_most_frequent_dummy = T)
dummiesDirectModelNeeded = dummiesDirectModel[,33:94]
dummiesDirectModelNeeded = as.data.frame(dummiesDirectModelNeeded)
dummiesDirectModelNeeded = subset(dummiesDirectModelNeeded, select = -c(`channel_MTV (NL)`, `channel_RTL 5`, channel_SPIKE, 
                                                                        channel_Viceland, channel_VIER, channel_ZES)) # Exclude singularities
#broad = dummiesDirectModel # I am afraid to press this BUT this should include the dummy
dummiesDirectModelNoChannel = dummy_cols(.data = broad, select_columns = c("cluster", "product_category", "length_of_spot", "position_in_break_3option"), remove_most_frequent_dummy = T)
dummiesDirectModelNoChannel = dummiesDirectModelNoChannel[,33:44]
dummiesDirectModelNoChannelNoProduct = subset(dummiesDirectModelNoChannel, select = -c(product_category_laptops, product_category_televisies)) # Exclude prod. cat
