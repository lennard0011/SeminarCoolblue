#Installing packages (hoeft maar één keer)
install.packages("chron")
install.packages("ggplot2")
install.packages("lubridate")

#Adding packages
library("chron")
library("ggplot2")
library("lubridate")
library("data.table")

#Inladen van de twee tabellen
traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

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
  
#For the direct effects model we calculate the amount of traffic in a interval before the broadcast and after the broadcast
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
  
#Further country specific variables
  
  traffic_netherlands = subset(traffic, country == 'Netherlands')
  traffic_belgium = subset(traffic, country == 'Belgium')
  broadcast_netherlands = subset(broad, country == 'Netherlands')
  broadcast_belgium = subset(broad, country == 'Belgium')
  
  #amount of days in time-frame
  amountDays = 31 + 28 + 31 + 30 + 31 + 30
  scopeDays = amountDays
  #set of unique advertising dates
  uniqueDates = unique(broad$date)
  uniqueDatesBel = unique(broadcast_belgium$date)
  uniqueDatesNet = unique(broadcast_netherlands$date)
  uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) #adverts in both on certain day
  uniqueDatesOnlyBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) #adverts only in Belgium on certain day
  uniqueDatesOnlyNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) #adverts only in Netherlands on certain day
  
  #amount of advertisments per day 
  adAmount = matrix(0, scopeDays)
  for (i in 1:scopeDays){
    iDate = as.Date(i - 1, origin = "2019-01-01")
    adsIDate = sum(broad$date == iDate)
    adAmount[i] = adsIDate
  }
  
  #amount of traffic per day for belgium
  dayNrs = matrix(0, amountDays) #how much traffic per day
  for (i in 1:nrow(traffic_belgium)){
    if(i %% 10000 == 0){
      print(i)
    }
    dayNr = yday(traffic_belgium[i,]$date_time) 
    dayNrs[dayNr] = dayNrs[dayNr] + 1
  }