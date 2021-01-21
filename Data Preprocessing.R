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

#loading the data
traffic = read.csv(file.choose(), header = T)
broad = read.csv(file.choose(), header = T)

nTraffic = nrow(traffic)
nBroad = nrow(broad)

#now that packages and data has been loaded, we start by creating new features and set the data up in a nice and usable way

#First we add the column time_min to traffic and broad, which is the time in a scale of minutes (from 0 to 24*60= 1440)
#Also we add the column date to traffic

#add date+time to every broadcast
broad$date_time = 0
for (i in 1:nBroad){
  broad$date_time[i] = paste(broad$date[i], broad$time[i], sep = " ")
}

#add time_min to every broadcast
broad['time_min'] = 0
for (i in 1:nBroad) { #nBroad
  time = broad$time[[i]]
  timeMinute = 60 * 24 * as.numeric(times(time))
  broad$time_min[[i]] = timeMinute
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

#calculate average per day for different searches -- Netherlands
av_traffic_day_net = matrix(NA, 24)
traffic_net = traffic_net[order(traffic_net$date),]
for (i in 1:24){
  print(i)
  traffic_subset = subset(traffic_net, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  av_traffic_day_net[i] = nrow(traffic_subset)/amountDays
}
traffic_net = traffic_net[order(as.numeric(row.names(traffic_net))),]

#calculate average per day for different searches -- Belgium
av_traffic_day_bel = matrix(NA, 24)
traffic_bel = traffic_bel[order(traffic_bel$date),]
for (i in 1:24){
  print(i)
  traffic_subset = subset(traffic_bel, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  av_traffic_day_bel[i] = nrow(traffic_subset)/amountDays
}
traffic_bel = traffic_bel[order(as.numeric(row.names(traffic_bel))),]


hours = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00")
cat_net = rep("Netherlands", 24)
cat_bel = rep("Belgium", 24)
categories = c(cat_net, cat_bel)
value = c(av_traffic_day_net, av_traffic_day_bel)
data = data.frame(categories, hours, value)
hourly_traffic = ggplot(data, aes(fill=categories, y=value, x=hours)) + scale_fill_grey(start = 0.7, end = 0.4)  +  geom_bar(position="stack", stat="identity")
print(hourly_traffic + 
        labs(fill = "Countries", title = "Average amount of website visitors per hour", y = "Average amount of website visitors", x = "Hour of the day")) + 
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.title.x = element_text(vjust = -0.5)) + 
        theme(axis.ticks = element_blank()) + 
        theme(axis.text.x = element_text(color=c("black","transparent","transparent","transparent", "transparent","transparent", "black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent")))
        

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

broad$weekdays = 0
for (i in 1:nBroad){
  yearDay = yday(broad$date[i])
  broad$weekdays[i] = weekdays(as.Date(yearDay))
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
channel$youth[30] = 1
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

#overlap dummy
intervalSize = 4
broad = broad[order(broad$date_time),]
broad$overlap = 0
for (i in 1:nBroad){
  if (i %% 100 == 0){
    print(i)
  }
  datetime = broad$date_time[i]
  datetime = as.POSIXct(datetime)
  four_earlier = datetime - intervalSize * 60
  four_later = datetime + intervalSize * 60
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

dummiesDirectModel = dummy_cols(.data = broad, select_columns = c("cluster", "product_category", "channel", "length_of_spot", "position_in_break_3option", "hours"), remove_most_frequent_dummy = T)

dummiesDirectModelNeeded = dummiesDirectModel[,((ncol(broad)+1):ncol(dummiesDirectModel))]

dummiesDirectModelNeeded = as.data.frame(dummiesDirectModelNeeded)
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
dummiesDirectModelTime = dummiesDirectModelTime[,32:49]

corTabel <- cor(dummiesDirectModelNeeded)

