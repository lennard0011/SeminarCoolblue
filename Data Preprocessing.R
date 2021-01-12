#Installing packages (hoeft maar één keer)
install.packages("chron")
install.packages("ggplot2")
install.packages("lubridate")

#Adding packages
library("chron")
library("ggplot2")
library("lubridate")
library("data.table")

m#Inladen van de twee tabellen
traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

nTraffic <- nrow(traffic)
nBroad <- nrow(broad)

#now that packages and data has been loaded, we start by creating new features and set the data up in a nice and usable way

#First we add the column time_min to traffic and broad, which is the time in a scale of minutes (from 0 to 24*60= 1440)
#Also we add the column date to traffic

  #add time_min to every broadcast
  broad['time_min'] <- 0
  for (index in 1:nBroad) { #nBroad
    time <- broad$time[[index]]
    timeMinute <- 60 * 24 * as.numeric(times(time))
    broad$time_min[[index]] <- timeMinute
  }
  #add time_min and date to every travel
  traffic['time_min'] <- 0
  traffic$date_time <- as.character(traffic$date_time)
  trafficDateSplitWhole <- strsplit(traffic$date_time, "\\s+")
  trafficDateSplitUnlist <- unlist(trafficDateSplitWhole)
  traffictime <- trafficDateSplitUnlist[seq(2, length(trafficDateSplitUnlist), 2)]
  traffic$date <- trafficDateSplitUnlist[seq(1, length(trafficDateSplitUnlist), 2)]
  traffic$time_min <- 60 * 24 * as.numeric(times(traffictime))
  trafficDateSplitWhole <-  NULL
  trafficDateSplitUnlist <- NULL
  traffictime <- NULL
  
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
  
  #amount of advertisemetns per day -- Belgium
  adAmountBel <- as.matrix(table(broad_bel$date))
  
  #amount of traffic per day -- Netherlands (approx. running time 5 seconds)
  trafAmountNet <- as.matrix(table(traffic_net$date)) #how much traffic per day
  
  #amount of traffic per day -- Belgium (approx. running time 5 seconds)
  trafAmountBel <- as.matrix(table(traffic_bel$date))
  
  #amount of traffic per day -- Total
  #NOTE: you can only run this if you have run both Net and Bel
  trafAmount = trafAmountNet + trafAmountBel

## Adding (time series) dummy to the data
  
  #national holidays
  holidaysNames <- c("Nieuwjaarsdag", "Goede Vrijdag", "Eerste Paasdag", 
                     "Tweede Paasdag", "Koningsdag", "Bevrijdingsdag", 
                     "Hemelvaartsdag", "Eerste Pinksterdag", 
                     "Tweede Pinksterdag")
  holidaysDates <- c("2019-01-01", "2019-04-19", "2019-04-21", "2019-04-22", 
                     "2019-04-27", "2019-05-05", "2019-05-30", "2019-06-09",
                     "2019-06-10")
  dummyHolidays <- matrix(rep(0), nrow = amountDays)
  for (i in 1:length(holidaysDates)) {
    index <- yday(holidaysDates[i])
    dummyHolidays[index] <- 1
  }
  dummyHemelvaartsdag <- matrix(rep(0), nrow = amountDays)
  dummyHemelvaartsdag[150] <- 1
  #weekday and week dummies (might be simplified with a package?)
  dummyMonday <- matrix(rep(0), nrow = amountDays) 
  dummyTuesday <- matrix(rep(0), nrow = amountDays) 
  dummyWednesday <- matrix(rep(0), nrow = amountDays) 
  dummyThursday <- matrix(rep(0), nrow = amountDays) 
  dummyFriday <- matrix(rep(0), nrow = amountDays) 
  dummySaturday <- matrix(rep(0), nrow = amountDays) 
  dummySunday <- matrix(rep(0), nrow = amountDays) 
  allDates <- sort(unique(traffic$date))
  for (i in 1:amountDays) {
    if (wday(allDates[i]) == 1) { dummyMonday[i] <- 1 }
    if (wday(allDates[i]) == 2) { dummyTuesday[i] <- 1 }
    if (wday(allDates[i]) == 3) { dummyWednesday[i] <- 1 }
    if (wday(allDates[i]) == 4) { dummyThursday[i] <- 1 }
    if (wday(allDates[i]) == 5) { dummyFriday[i] <- 1 }
    if (wday(allDates[i]) == 6) { dummySaturday[i] <- 1 }
    if (wday(allDates[i]) == 7) { dummySunday[i] <- 1 }
  }
  #month dummies
  dummyJanuary <- matrix(rep(0), nrow = amountDays)
  dummyFebruary <- matrix(rep(0), nrow = amountDays)
  dummyMarch <- matrix(rep(0), nrow = amountDays)
  dummyApril <- matrix(rep(0), nrow = amountDays)
  dummyMay <- matrix(rep(0), nrow = amountDays)
  dummyJune <- matrix(rep(0), nrow = amountDays)
  for (i in 1:amountDays) {
    if (month(allDates[i]) == 1) { dummyJanuary[i] <- 1 }
    if (month(allDates[i]) == 2) { dummyFebruary[i] <- 1 }
    if (month(allDates[i]) == 3) { dummyMarch[i] <- 1 }
    if (month(allDates[i]) == 4) { dummyApril[i] <- 1 }
    if (month(allDates[i]) == 5) { dummyMay[i] <- 1 }
    if (month(allDates[i]) == 6) { dummyJune[i] <- 1 }
  }
  #ads dummies
  dummyAds <- matrix(rep(0), nrow = amountDays)
  for (i in 1:length(uniqueDates)) {
    index <- yday(uniqueDates[i])
    dummyAds[index] <- 1
  }
  dummyAdsNet <- matrix(rep(0), nrow = amountDays)
  for (i in 1:length(uniqueDatesNet)) {
    index <- yday(uniqueDatesNet[i])
    dummyAdsNet[index] <- 1
  }
  dummyAdsBel <- matrix(rep(0), nrow = amountDays)
  for (i in 1:length(uniqueDatesBel)) {
    index <- yday(uniqueDatesBel[i])
    dummyAdsBel[index] <- 1
  }
  
#For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
#Results are stored in the column preVisitors and postVisitors in the dataframe broad
#BEWARE IT TAKES A LONG TIME TO RUN
  
  #count visits pre-commercial
  broad['preVisitors'] <- 0
  intervalSize <- 5
  start <- Sys.time()
  for (index in 1:500) { #nBroad
    broadDate <- broad$date[[index]]
    broadTime <- broad$time_min[[index]]
    extraViews <- 0
    if(broadTime - intervalSize < 0){
      extraViews <- length(which(traffic$date == as.Date(broadDate) - 1 & traffic$time_min >= 60*24 - intervalSize))
    } else{
      broad$preVisitors[[index]] <- length(which(traffic$date == broadDate & traffic$time_min < broadTime & traffic$time_min >= broadTime - intervalSize)) + extraViews
    }
    if(index %% 1000 == 0) {print(Sys.time() - start)}
  }
  #count visits post-commercial
  broad['postVisitors'] <- 0
  start <- Sys.time()
  for (index in 1:500) { #nBroad
    broadDate <- broad$date[[index]]
    broadTime <- broad$time_min[[index]]
    broad$postVisitors[[index]] <- length(which(traffic$date == broadDate & traffic$time_min >= broadTime & traffic$time_min < broadTime + intervalSize))
    if(index %% 1000 == 0) {print(Sys.time() - start)}
  }
