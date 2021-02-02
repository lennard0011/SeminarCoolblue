#bsts
#make avg_gross_rating_point variable - Net
total_gross_rating_point = matrix(0, amountDays)
amount_broadcast = matrix(0, amountDays)
for (i in 1:NROW(broadNet)){
  dayNr = yday(broadNet$date[i])
  gross_rating_point = broadNet$gross_rating_point[i]
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
for (i in 1:NROW(broadBel)){
  dayNr = yday(broadBel$date[i])
  gross_rating_point = broadBel$gross_rating_point[i]
  total_gross_rating_point[dayNr] = total_gross_rating_point[dayNr] + gross_rating_point
  amount_broadcast[dayNr] = amount_broadcast[dayNr] + 1
}
avg_gross_rating_point_Bel = total_gross_rating_point/amount_broadcast
for (i in 1:NROW(avg_gross_rating_point_Bel)){
  if (is.nan(avg_gross_rating_point_Bel[i])){
    avg_gross_rating_point_Bel[i] = 0
  }
}

# sum of visit_index -- Net
visWebNet$yday = yday(visWebNet$date)
sumVisitIndexNet = matrix(0, amountDays)
for (i in 1:amountDays){
  print(i)
  trafficSubset = subset(visWebNet, yday == i)
  sumVisitIndexNet[i] = sum(trafficSubset$visits_index)
}
visWebNet = subset(visWebNet, select = -yday)


# sum of visit_index -- Bel
visWebBel$yday = yday(visWebBel$date)
sumVisitIndexBel = matrix(0, amountDays)
for (i in 1:amountDays){
  print(i)
  trafficSubset = subset(visWebBel, yday == i)
  sumVisitIndexBel[i] = sum(trafficSubset$visits_index)
}
visWebBel = subset(visWebBel, select = -yday)

# weekdays
weekdays = matrix(NA, amountDays)
for (i in 1:amountDays){
  print(i)
  for (j in 1:nrow(visWebNet)){
    if (yday(visWebNet$date[j]) == i){
      weekdays[i] = weekdays(as.Date(visWebNet$date[j]))
      break
    }
  }
}
weekdayDummy = dummy_cols(weekdays, remove_most_frequent_dummy = TRUE)[, 2:7]

#bsts for entire time periods
#211-406
#website
data = zoo(cbind(sumVisitIndexNet, sumVisitIndexBel), c(1:amountDays))
commercialBegin = "2019-02-11"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-02-01", "2019-02-10") #1-29--2-10
yday_pre.period = yday(pre.period)
post.period = c("2019-02-11", "2019-04-06")
yday_post.period = yday(post.period)

entireImpact1 = CausalImpact(data, yday_pre.period, yday_post.period, model.args = list(niter = 5000))
plot(entireImpact1)
entireImpact1$summary
entireImpact1$report

#520-603
#website
commercialBegin = "2019-05-20"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-04-22", "2019-05-19")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-20", "2019-06-03")
yday_post.period = yday(post.period)

entireImpact2 = CausalImpact(data, yday_pre.period, yday_post.period, model.args = list(niter = 5000, nseasons = 7))
plot(entireImpact2)
entireImpact2$summary
entireImpact2$report

#617-630
#website
commercialBegin = "2019-06-17"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-06-04", "2019-06-16")
yday_pre.period = yday(pre.period)
post.period = c("2019-06-17", "2019-06-30")
yday_post.period = yday(post.period)

entireImpact3 = CausalImpact(data, yday_pre.period, yday_post.period, model.args = list(niter = 5000, nseasons = 7))
plot(entireImpact3)
entireImpact3$summary
entireImpact3$report

#test for imaginary period
#205-210
#website
commercialBegin = "2019-02-05"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-01-29", "2019-02-04")
yday_pre.period = yday(pre.period)
post.period = c("2019-02-05", "2019-02-10")
yday_post.period = yday(post.period)

entireImpact4 = CausalImpact(data, yday_pre.period, yday_post.period, model.args = list(niter = 5000, nseasons = 7))
plot(entireImpact4)
entireImpact4$summary
entireImpact4$report
