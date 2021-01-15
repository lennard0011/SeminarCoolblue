#bsts
#make avg_gross_rating_point variable - Net
total_gross_rating_point = matrix(0, amountDays)
amount_broadcast = matrix(0, amountDays)
for (i in 1:NROW(broad_net)){
  dayNr = yday(broad_net$date[i])
  gross_rating_point = broad_net$gross_rating_point[i]
  total_gross_rating_point[dayNr] = total_gross_rating_point[dayNr] + gross_rating_point
  amount_broadcast[dayNr] = amount_broadcast[dayNr] + 1
}
avg_gross_rating_point_Net = total_gross_rating_point/amount_broadcast
for (i in 1:NROW(avg_gross_rating_point_Bel)){
  if (is.nan(avg_gross_rating_point_Net[i])){
    avg_gross_rating_point_Net[i] = 0
  }
}

#make avg_gross_rating_point variable - Bel
total_gross_rating_point = matrix(0, amountDays)
amount_broadcast = matrix(0, amountDays)
for (i in 1:NROW(broad_bel)){
  dayNr = yday(broad_bel$date[i])
  gross_rating_point = broad_bel$gross_rating_point[i]
  total_gross_rating_point[dayNr] = total_gross_rating_point[dayNr] + gross_rating_point
  amount_broadcast[dayNr] = amount_broadcast[dayNr] + 1
}
avg_gross_rating_point_Bel = total_gross_rating_point/amount_broadcast
for (i in 1:NROW(avg_gross_rating_point_Bel)){
  if (is.nan(avg_gross_rating_point_Bel[i])){
    avg_gross_rating_point_Bel[i] = 0
  }
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

abs_effect_average_vector = matrix(NA, nrow(broad_net))
abs_effect_cumulative_vector = matrix(NA, nrow(broad_net))
significance_vector = matrix(0, nrow(broad_net))

for (i in 1:nrow(broad_net)){
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
for (i in 1:nrow(traffic_net)){
  if(i %% 10000 == 0){
    print(i)
  }
  dayNr = yday(traffic_net[i,]$date_time) 
  dayNrs_Net[dayNr] = dayNrs_Net[dayNr] + 1
}

#how much traffic per day - Belgium
dayNrs_Bel = matrix(0, amountDays)
for (i in 1:nrow(traffic_bel)){
  if(i %% 10000 == 0){
    print(i)
  }
  dayNr = yday(traffic_bel[i,]$date_time) 
  dayNrs_Bel[dayNr] = dayNrs_Bel[dayNr] + 1
}

data = zoo(cbind(dayNrs_Net, dayNrs_Bel, avg_gross_rating_point_Net - avg_gross_rating_point_Bel), c(1:amountDays))
commercialBegin = "2019-05-20"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-04-21", "2019-05-19")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-20", "2019-06-03")
yday_post.period = yday(post.period)

entireImpact = CausalImpact(data, yday_pre.period, yday_post.period)
plot(entireImpact)
entireImpact$summary

#plot dayNrs
dayNrs = as.matrix(table(traffic$date))
dayNrs_names = row.names(dayNrs)
data_dayNrs = as.data.frame(cbind(dayNrs, dayNrs_names))
ggplot(data = data_dayNrs, aes("dayNrs_names", "V1")) + geom_line() + xlab("")