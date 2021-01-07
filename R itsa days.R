#itsa
traffic_netherlands_sorted = traffic_netherlands[base::order(traffic_netherlands$date_time), ]
split_1_day_traffic = split(traffic_netherlands_sorted, cut(strptime(paste(traffic_netherlands_sorted$date_time), format = "%F %R"), "1 days"))

broadcast_netherlands_sorted = broadcast_netherlands_sorted[base::order(broadcast_netherlands_sorted$broadcast_date_time), ]
split_1_day_broadcast = split(broadcast_netherlands_sorted, cut(strptime(paste(broadcast_netherlands_sorted$date), format = "%F"), "1 days"))

uniqueDatesNet = unique(broadcast_netherlands$date)

interruptionDummy = matrix(0, amountDays)
for (i in 1:amountDays){
  for (j in 1:NROW(uniqueDatesNet)){
    date = as.Date(i - 1, origin = "2019-01-01")
    if (date == uniqueDatesNet[j]){
      interruptionDummy[i] = 1
      break
    }
  }
}

start = as.POSIXct("2019-01-01 00:00:00", tz = "GMT")
end = start + as.difftime(amountDays - 1, units = "days")
days = seq(from = start, to = end, length.out = amountDays)

dayNrs = matrix(0, amountDays) #how much traffic per day
for (i in 1:nrow(traffic_netherlands)){
  if(i %% 10000 == 0){
    print(i)
  }
  dayNr = yday(traffic_netherlands[i,]$date_time) 
  dayNrs[dayNr] = dayNrs[dayNr] + 1
}

days = as.Date((days))

x = as.data.frame(cbind(days, dayNrs, interruptionDummy))
itsa1 = itsa.model(data = x, time = "days", depvar = "dayNrs", interrupt_var = "interruptionDummy",
                   alpha=0.05, bootstrap=TRUE, Reps = 250)
