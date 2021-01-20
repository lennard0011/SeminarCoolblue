#install packages
install.packages("ggplot2")
install.packages("lubridate")
install.packages("its.analysis")
install.packages("CausalImpact")
install.packages("sandwich")

#load packages
library("ggplot2")
library("lubridate")
library("data.table")
library("its.analysis")
library("CausalImpact")
library("sandwich")
library("lmtest")
library("stringr")

#data
broadcasting = read.csv(file.choose())
traffic = read.csv(file.choose())

#subset data
traffic_netherlands = subset(traffic, country == 'Netherlands')
traffic_belgium = subset(traffic, country == 'Belgium')
broadcast_netherlands = subset(broad, country == 'Netherlands')
broadcast_belgium = subset(broad, country == 'Belgium')

#plot broadcasts nl-be
amountDays = 31 + 28 + 31 + 30 + 31 + 30
scopeDays = amountDays
uniqueDates = unique(broad)
uniqueDatesBel = unique(broadcast_belgium$date)
uniqueDatesNet = unique(broadcast_netherlands$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) #adverts in both on certain day
uniqueDatesBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) #adverts only in Belgium on certain day
uniqueDatesNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) #adverts only in Netherlands on certain day
adAmount = matrix(0, scopeDays)
for (i in 1:scopeDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broad$date == iDate)
  adAmount[i] = adsIDate
}
plot(adAmount)


#plot traffic nl-be
amountDays = 31 + 28 + 31 + 30 + 31 + 30
scopeDays = amountDays
uniqueDates = unique(traffic)
uniqueDatesBel = unique(traffic_belgium$date)
uniqueDatesNet = unique(traffic_netherlands$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) #adverts in both on certain day
uniqueDatesBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) #adverts only in Belgium on certain day
uniqueDatesNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) #adverts only in Netherlands on certain day
adAmount = matrix(0, scopeDays)
for (i in 1:scopeDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(traffic$date == iDate)
  adAmount[i] = adsIDate
}
plot(adAmount)
for (i in 1:NROW(uniqueDates)){
  abline(v = yday(uniqueDatesBel[i]), col = 'blue')
  abline(v = yday(uniqueDatesNet[i]), col = 'red')
  abline(v = yday(uniqueDatesBoth[i]), col = 'green')
}

#make interruption dummy
interruptionDummy = matrix(0, amountDays)
for (i in 1:amountDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  for (j in 1:NROW(uniqueDatesNet)){
    if (iDate == uniqueDatesNet[j]){
      interruptionDummy[i] = 1
    }
  }
}

#plot all broadcasts
scopeDays = amountDays
uniqueDates = unique(broadcasting$date)
adAmount = matrix(0, scopeDays)
for (i in 1:scopeDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broadcasting$date == iDate)
  adAmount[i] = adsIDate
}
plot(adAmount)
for (i in 1:NROW(uniqueDates)){
  abline(v = yday(uniqueDates[i]), col = 'blue')
}

#plot all hits
dayNrs = matrix(0, amountDays) #how much traffic per day
uniqueDatesBel = unique(broadcast_belgium$date)
uniqueDatesNet = unique(broadcast_netherlands$date)
for (i in 1:nrow(traffic_belgium)){
  if(i %% 10000 == 0){
    print(i)
  }
  dayNr = yday(traffic_belgium[i,]$date_time) 
  dayNrs[dayNr] = dayNrs[dayNr] + 1
}
plot(dayNrs)
for (i in 1:NROW(uniqueDatesBel)){
  abline(v = yday(uniqueDatesBel[i]), col = 'blue') #broadcast dates
}


#itsa
traffic_netherlands_sorted = traffic_netherlands[base::order(traffic_netherlands$date_time), ]
split_1_min_traffic = split(traffic_netherlands_sorted, cut(strptime(paste(traffic_netherlands_sorted$date_time), format = "%F %R"), "1 mins"))

broadcast_date_time = paste(broadcast_netherlands[, 3], broadcast_netherlands[ ,4])
broadcast_netherlands_sorted = broadcast_netherlands
broadcast_netherlands_sorted = cbind(broadcast_netherlands, broadcast_date_time)
broadcast_netherlands_sorted = broadcast_netherlands_sorted[base::order(broadcast_netherlands_sorted$broadcast_date_time), ]
split_1_min_broadcast = split(broadcast_netherlands_sorted, cut(strptime(paste(broadcast_netherlands_sorted$broadcast_date_time), format = "%F %R"), "1 mins"))

#count which minute of year
minuteYear = function(date_time){
  date = substring(date_time, 1, 10)
  if (str_length(date_time) == 10){
    time = "00:00:00"
  } else{
    time = substring(date_time, 12, 19) 
  }
  dateNr = yday(date)
  hour = as.numeric(substring(time, 1, 2))
  minute = as.numeric(substring(time, 4, 5))
  minuteNumber = (dateNr - 1) * 24 * 60 + hour * 60 + minute
  return(minuteNumber)
}

#add minute of year to broadcast matrix
exactMinute = matrix(NA, nrow(broadcast_netherlands_sorted))
for (i in 1:nrow(broadcast_netherlands_sorted)){
  exactMinute[i] = minuteYear(broadcast_netherlands_sorted$broadcast_date_time[i])
}
broadcast_netherlands_sorted_minute = cbind(broadcast_netherlands_sorted, exactMinute)

#add minute of year to traffic matrix
exactMinute = matrix(NA, nrow(traffic_netherlands_sorted))
for (i in 1:nrow(traffic_netherlands_sorted)){
  if (i %% 10000 == 0){
    print(i)
  }
  exactMinute[i] = minuteYear(traffic_netherlands_sorted$date_time[i])
}
traffic_netherlands_sorted_minute = cbind(traffic_netherlands_sorted, exactMinute)
split_1_min_traffic_minute = split(traffic_netherlands_sorted_minute, cut(strptime(paste(traffic_netherlands_sorted_minute$date_time), format = "%F %R"), "1 mins"))


totalMinutes = scopeDays * 24 * 60

interruptionDummy_min = matrix(0, totalMinutes)
for (i in 1:totalMinutes){
  if (i %in% broadcast_netherlands_sorted_minute$exactMinute){
    interruptionDummy_min[i] = 1
  }
}

start = as.POSIXct("2019-01-01")
end = start + as.difftime(60, units = "secs")
minutes = seq(from = start, by = 60, length.out = totalMinutes)

for (i in 1:NROW(split_1_min_traffic_minute)){
  if (i %% 10000 == 0){
    print(i)
  }
  names(split_1_min_traffic_minute)[i] = unique(split_1_min_traffic_minute[[i]]$exactMinute)
}

unique_exactMinute = unique(exactMinute)
amountMinute = matrix(0, totalMinutes)
j = 1
for (i in 0:totalMinutes - 1){
  if (i %% 10000 == 0){
    print(i)
  }
  if (i %in% unique_exactMinute){
      amountMinute[i + 1] = NROW(split_1_min_traffic_minute[[j]])
    j = j + 1
  }
}

x = as.data.frame(cbind(minutes, amountMinute, interruptionDummy_min_2))
itsa1 = itsa.model(data = x[(minuteYear("2019-02-24 19:00:00") + 1):(minuteYear("2019-02-24 22:00:00") + 1), ], time = "minutes", depvar = "V2", interrupt_var = "V3",
                   alpha=0.05, bootstrap = F)

#make dummy for interruption past 2 minutes
interruptionDummy_min_2 = interruptionDummy_min
for (i in 1:nrow(interruptionDummy_min)){
  if (interruptionDummy_min[i] == 1){
    interruptionDummy_min_2[i + 1] = 1
  }
}

#manually
Yt = amountMinute[(minuteYear("2019-01-01 00:00:00") + 1):(minuteYear("2019-01-31 23:59:00") + 1)]
Tt = c(1:NROW(Yt))
Xt = interruptionDummy_min_2[(minuteYear("2019-01-01 00:00:00") + 1):(minuteYear("2019-01-31 23:59:00") + 1)]
XtTt = Xt*Tt
itsaReg = lm(Yt ~ Tt + Xt + XtTt)
summary(itsaReg)
coeftest(itsaReg, vcov = vcovHC)

#bsts
unique_exactMinute = unique(exactMinute)
amountMinute = matrix(0, totalMinutes)
j = 1
for (i in 0:totalMinutes - 1){
  if (i %% 10000 == 0){
    print(i)
  }
  if (i %in% unique_exactMinute){
    amountMinute[i + 1] = NROW(split_1_min_traffic_minute[[j]])
    j = j + 1
  }
}

data = zoo(cbind(amountMinute, interruptionDummy_min), minutes)
matplot(data, type = "l")

commercialTime = paste(broadcast_netherlands_sorted_minute$date, broadcast_netherlands_sorted_minute$time)[1]
pre.period = c(as.POSIXlt.character(commercialTime) - 60 * 10, as.POSIXlt.character(commercialTime) - 60 * 1)
post.period = c(as.POSIXlt.character(commercialTime) - 60 * 0, as.POSIXlt.character(commercialTime) + 60 * 5)
beginDate = as.POSIXlt.character(commercialTime) - 60 * 40
beginDate_minute = minuteYear(beginDate)
endDate = as.POSIXlt.character(commercialTime) + 60 * 35
endDate_minute = minuteYear(endDate)
data = data[(beginDate_minute + 1):(endDate_minute + 1), ]

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
impact$report

#bsts for all broadcasts
data = zoo(cbind(amountMinute, interruptionDummy_min), minutes)

abs_effect_average_vector = matrix(NA, nrow(broadcast_netherlands))
abs_effect_cumulative_vector = matrix(NA, nrow(broadcast_netherlands))
significance_vector = matrix(0, nrow(broadcast_netherlands))

for (i in 1:nrow(broadcast_netherlands)){
  if (i %% 50 == 0){
    print(i)
  }
  commercialTime = paste(broadcast_netherlands_sorted_minute$date, broadcast_netherlands_sorted_minute$time)[i]
  pre.period = c(as.POSIXlt.character(commercialTime) - 60 * 10, as.POSIXlt.character(commercialTime) - 60 * 1)
  post.period = c(as.POSIXlt.character(commercialTime) - 60 * 0, as.POSIXlt.character(commercialTime) + 60 * 3)
  beginDate = as.POSIXlt.character(commercialTime) - 60 * 40
  beginDate_minute = minuteYear(beginDate)
  endDate = as.POSIXlt.character(commercialTime) + 60 * 35
  endDate_minute = minuteYear(endDate)
  
  #daylight saving time
  if (beginDate < "2019-03-31 02:00"){
    data_subset = data[(beginDate_minute + 1):(endDate_minute + 1), ]
  }
  if (beginDate >= "2019-03-31 03:00"){
    data_subset = data[(beginDate_minute + 1 - 60):(endDate_minute + 1 - 60), ]
  }
  
  impact = CausalImpact(data_subset, pre.period, post.period) 
  
  abs_effect_average_vector[i] = impact$summary[1,6]
  abs_effect_cumulative_vector[i] = impact$summary[2,6]
  
  if (impact$summary[1, 15] <= impact$summary[1,14]){
    significance_vector[i] = 1
  }
}

#bsts for entire time periods
#1-29 4-14

#how much traffic per day - Netherlands
dayNrs_Net = matrix(0, amountDays)
for (i in 1:nrow(traffic_netherlands)){
  if(i %% 10000 == 0){
    print(i)
  }
  dayNr = yday(traffic_netherlands[i,]$date_time) 
  dayNrs_Net[dayNr] = dayNrs_Net[dayNr] + 1
}

#how much traffic per day - Belgium
dayNrs_Bel = matrix(0, amountDays)
for (i in 1:nrow(traffic_belgium)){
  if(i %% 10000 == 0){
    print(i)
  }
  dayNr = yday(traffic_belgium[i,]$date_time) 
  dayNrs_Bel[dayNr] = dayNrs_Bel[dayNr] + 1
}

data = zoo(cbind(dayNrs_Net, dayNrs_Bel), c(1:amountDays))
commercialBegin = "2019-02-11"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-01-29", "2019-02-10")
yday_pre.period = yday(pre.period)
post.period = c("2019-02-11", "2019-04-08")
yday_post.period = yday(post.period)

entireImpact = CausalImpact(data, yday_pre.period, yday_post.period)
plot(entireImpact)
