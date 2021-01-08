# Load data
library(stringr)
library(dplyr)
library(tidyr)
library(CausalImpact)


traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

traforder <- traffic[order(traffic$date_time),]
broad <- broad[order(broad$date),]

###############################################################################################
###############################################################################################

# Plots of two days around a specific commercial time, to see the difference in visits

# Select one day from data (19 Jan, 2019):
traffic_daycom <- subset(traffic, grepl("2019-02-14", traffic$date_time) == TRUE)
traffic_daycom <- traffic_daycom[order(traffic_daycom$date_time),]
#traffic_daycom <- subset(traffic_daycom,grepl("product",traffic_daycom$page_category)==TRUE)

# Select one day from data (12 Jan, 2019)
traffic_day <- subset(traffic, grepl("2019-02-03", traffic$date_time) == TRUE)
traffic_day <- traffic_day[order(traffic_day$date_time),]
#traffic_day <- subset(traffic_day,grepl("product", traffic_day$page_category) == TRUE)

# Aggregate visits for every minute of the day 
# assumption: there is at least 1 visit every minute of the day
minute_counter = 1 # will go up to 1440
visit_densitycom <- vector(mode="integer", length=1440)
# small neglection: the first obsv. is not counted
for (i in 2:nrow(traffic_daycom)) {
  if (traffic_daycom$date_time[i] == traffic_daycom$date_time[i-1]) {
    visit_densitycom[minute_counter] = visit_densitycom[minute_counter] + 1
  } else {
    minute_counter = minute_counter + 1
  }
}

minute_counter = 1 # will go up to 1440
visit_density <- vector(mode="integer", length=1440)
print(nrow(traffic_day))
# small neglection: the first obsv. is not counted
for (i in 2:nrow(traffic_day)) {
  if (traffic_day$date_time[i] == traffic_day$date_time[i-1]) {
    visit_density[minute_counter] = visit_density[minute_counter] + 1
  } else {
    minute_counter = minute_counter + 1
  }
}
# Example
beginTime <- "00:15"
endTime <- "23:59"
timeCom <- "20:28"

beginTime <- str_split_fixed(beginTime, ":", 2)
endTime <- str_split_fixed(endTime, ":", 2)
timeCom <- str_split_fixed(timeCom, ":", 2)
colnames(beginTime)<- c("hour", "minute")
colnames(endTime) <- c("hour", "minute")
colnames(timeCom) <- c("hour", "minute")
beginTime <- data.frame(beginTime)
endTime <- data.frame(endTime)
timeCom <- data.frame(timeCom)
beginTimeValue <- 60*as.numeric(beginTime[1,"hour"]) + as.numeric(beginTime[1,"minute"]) + 1
endTimeValue <- 60*as.numeric(endTime[1,"hour"]) + as.numeric(endTime[1,"minute"]) + 1
timeComValue <- 60*as.numeric(timeCom[1,"hour"]) + as.numeric(timeCom[1,"minute"]) + 1

plot(visit_densitycom, xlim = c(beginTimeValue,endTimeValue), col = 'blue', main = "Red: 13 mei. Blue: 20 mei, with commercial", type = "l")
lines(visit_density, col = "red")
abline(v = timeComValue,col = "grey")

#####################################################################################################
#####################################################################################################

# Code for plot of the bayesian post-pre estimate for one commercial on one day
# Example time: 2019-02-14 19:58

time <- 1199  #time of commercial
preinterval <- 10
postinterval <- 5
timeBegin <- time - preinterval
timeEnd <- time + postinterval

traffic_day <- subset(traffic, grepl("2019-02-14", traffic$date_time) == TRUE)
traffic_day <- traffic_day[order(traffic_day$date_time),]

minute_counter = 1 # will go up to 1440
visit_density <- vector(mode="integer", length=1440)
print(nrow(traffic_day))
# small neglection: the first obsv. is not counted
for (i in 2:nrow(traffic_day)) {
  if (traffic_day$date_time[i] == traffic_day$date_time[i-1]) {
    visit_density[minute_counter] = visit_density[minute_counter] + 1
  } else {
    minute_counter = minute_counter + 1
  }
}

visit_inter <- visit_density[timeBegin:timeEnd]
pre <- c(1,preinterval)
post <- c(preinterval +1, postinterval + preinterval + 1)


impact <- CausalImpact(visit_inter, pre, post)
plot(impact)

##############################################################################################

# Bayesian for days period
# From begin no commercials to date with commercials
# Output plot

# IMPORT TRAFORDER: traforder <- traffic[order(traffic$date_time),]
beginDate <- "2019-02-01"
endDate <- "2019-02-16"
middenDate <- "2019-02-11"

interest <- traforder[traforder$date>=as.Date(beginDate) & traforder$date<=as.Date(endDate),]

numberVisits<- nrow(interest)
uniqueDates <- data.frame(unique(as.Date(interest$date)))
numberUniqueDates <- nrow(uniqueDates)
visitsDay <- matrix(0,numberUniqueDates)

for (i in 1:numberUniqueDates){
  print(i)
  iDate = as.Date(i - 1, origin = beginDate)
   visitsDay[i] = sum(interest$date == iDate)
}

index <- which(uniqueDates == middenDate)
pre <- c(1,index-1)
post <- c(index,numberUniqueDates)


impact <- CausalImpact(visitsDay, pre, post)
plot(impact)
  
##################################################################################################
##################################################################################################


# Bayesian for days period  WITH COUNTRY
# From begin no commercials to date with commercials
# Output plot

# IMPORT TRAFORDER: traforder <- traffic[order(traffic$date_time),]
beginDate <- "2019-05-06"
endDate <- "2019-05-30"
middenDate <- "2019-05-20"

country <- "Netherlands"

broadorder <- broad[order(broad$date),]
trafcountry <- traforder[which(traforder$country == country), ]
broadcountry <- broadorder[which(broadorder$country == country), ]
interest <- trafcountry[trafcountry$date>=as.Date(beginDate) & trafcountry$date<=as.Date(endDate),]

numberVisits<- nrow(interest)
uniqueDates <- data.frame(unique(as.Date(interest$date)))
numberUniqueDates <- nrow(uniqueDates)
visitsDay <- matrix(0,numberUniqueDates)

for (i in 1:numberUniqueDates){
  print(i)
  iDate = as.Date(i - 1, origin = beginDate)
  visitsDay[i] = sum(interest$date == iDate)
}

index <- which(uniqueDates == middenDate)
pre <- c(1,index-1)
post <- c(index,numberUniqueDates)


impact <- CausalImpact(visitsDay, pre, post)
plot(impact)

#####################################################################################################

