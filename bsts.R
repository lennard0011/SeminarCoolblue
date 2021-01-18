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
for (i in 1:NROW(avg_gross_rating_point_Net)){
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

#bsts for entire time periods
#1-29 4-14
data = zoo(cbind(trafAmountNet, trafAmountBel, avg_gross_rating_point_Net - avg_gross_rating_point_Bel), c(1:amountDays))
commercialBegin = "2019-05-20"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-04-21", "2019-05-19")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-20", "2019-06-03")
yday_post.period = yday(post.period)

entireImpact = CausalImpact(data, yday_pre.period, yday_post.period)
plot(entireImpact)
entireImpact$summary