# Google Trends
Sys.setenv(TZ = "Europe/Amsterdam")
mediaMarktTrends = gtrends(keyword = "mediamarkt", geo = "NL", time = "2019-01-01 2019-06-30")
interestVectorMed = mediaMarktTrends$interest_over_time
plot(interestVectorMed$hits, type = 'l', xlab = "Day of the year", ylab = "MediaMarkt searches")

BCCTrends = gtrends(keyword = "bcc", geo = "NL", time = "2019-01-01 2019-06-30")
interestVectorBCC = BCCTrends$interest_over_time
plot(interestVectorBCC$hits, type = 'l', xlab = "Day of the year", ylab = "BCC searches")

# laptopTrends = gtrends(keyword = "laptop", geo = "NL", time = "2019-01-01 2019-06-30")
# interestVectorLap = laptopTrends$interest_over_time
# 
# washingMachineTrends = gtrends(keyword = "wasmachine", geo = "NL", time = "2019-01-01 2019-06-30")
# interestVectorWash= washingMachineTrends$interest_over_time
# 
# tvTrends = gtrends(keyword = "tv", geo = "NL", time = "2019-01-01 2019-06-30")
# interestVectorTV= tvTrends$interest_over_time
# 
# televisionTrends = gtrends(keyword = "televisie", geo = "NL", time = "2019-01-01 2019-06-30")
# interestVectorTelevision= televisionTrends$interest_over_time


#bsts
# #make avg_gross_rating_point variable - Net
# total_gross_rating_point = matrix(0, amountDays)
# amount_broadcast = matrix(0, amountDays)
# for (i in 1:NROW(broadNet)){
#   dayNr = yday(broadNet$date[i])
#   gross_rating_point = broadNet$gross_rating_point[i]
#   total_gross_rating_point[dayNr] = total_gross_rating_point[dayNr] + gross_rating_point
#   amount_broadcast[dayNr] = amount_broadcast[dayNr] + 1
# }
# avg_gross_rating_point_Net = total_gross_rating_point/amount_broadcast
# for (i in 1:NROW(avg_gross_rating_point_Net)){
#   if (is.nan(avg_gross_rating_point_Net[i])){
#     avg_gross_rating_point_Net[i] = 0
#   }
# }
# 
# #make avg_gross_rating_point variable - Bel
# total_gross_rating_point = matrix(0, amountDays)
# amount_broadcast = matrix(0, amountDays)
# for (i in 1:NROW(broadBel)){
#   dayNr = yday(broadBel$date[i])
#   gross_rating_point = broadBel$gross_rating_point[i]
#   total_gross_rating_point[dayNr] = total_gross_rating_point[dayNr] + gross_rating_point
#   amount_broadcast[dayNr] = amount_broadcast[dayNr] + 1
# }
# avg_gross_rating_point_Bel = total_gross_rating_point/amount_broadcast
# for (i in 1:NROW(avg_gross_rating_point_Bel)){
#   if (is.nan(avg_gross_rating_point_Bel[i])){
#     avg_gross_rating_point_Bel[i] = 0
#   }
# }

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


# some tests
# seasonality test
seasTest = wo(sumVisitIndexNet, freq = 7)
summary(seasTest) # weekly seasonality; does contain seasonality

# trend test
trendTest = trend.test(sumVisitIndexNet) # no trend
trendTest

# # what products were advertised
# broadNet$yday = yday(broadNet$date)
# productDay = matrix(NA, amountDays)
# for (i in 1:amountDays){
#   print(i)
#   for (j in 1:nrow(broadNet)){
#     if (broadNet$yday[j] == i){
#       productDay[i] = broadNet$product_category[j]
#       break
#     }
#   }
# }
# 
# maxFun = function(day){
#   ux = unique(broadNet$product_category[broadNet$yday == day])
#   maxim = ux[which.max(tabulate(match(broadNet$product_category[broadNet$yday == day], ux)))]
#   answer = paste0("On day ", day, ", mainly ", maxim, " are advertised")
#   return(answer)
# }
# maxFun(56)
# productDay[56] = "laptops"
# maxFun(57)
# maxFun(69)
# maxFun(70)


# 211-407
# website
# all
data = zoo(cbind(sumVisitIndexNet, sumVisitIndexBel, interestVectorMed[, 2], interestVectorBCC[, 2]), c(1:181))
set.seed(11)
commercialBegin = "2019-02-11"
commercialEnd = "2019-04-07"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-01-29", "2019-02-10")
yday_pre.period = yday(pre.period)
post.period = c(commercialBegin, commercialEnd)
yday_post.period = yday(post.period)

# seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # seasonality identified

# trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # trend not identified

entireImpact1 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact1)
entireImpact1$summary
entireImpact1$report
plot(entireImpact1$model$bsts.model, "coef", inc = .1)


# # 211-224
# # website
# # televisions
# data = zoo(cbind(sumVisitIndexNet, sumVisitIndexBel, interestVectorMed[, 2], interestVectorBCC[, 2]), c(1:181))
# set.seed(11)
# commercialBegin = as.Date(42 - 1, origin = "2019-01-01")
# commercialEnd = as.Date(55 - 1, origin = "2019-01-01")
# yday_commercialBegin = yday(commercialBegin)
# pre.period = c("2019-02-01", "2019-02-10") #1-29--2-10
# yday_pre.period = yday(pre.period)
# post.period = c(commercialBegin, commercialEnd)
# yday_post.period = yday(post.period)
# 
# # seasonality test
# seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
# summary(seasTest)  # period not long enough; choose seasonality
# 
# # trend test
# trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
# trendTest # trend not identified
# 
# entireImpact1.1 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000, nseasons = 7))
# plot(entireImpact1.1)
# entireImpact1.1$summary
# entireImpact1.1$report
# plot(entireImpact1.1$model$bsts.model, "coef", inc = .1)
# 
# # 225-310
# # website
# # laptops
# set.seed(11)
# commercialBegin = as.Date(56 - 1, origin = "2019-01-01")
# commercialEnd = as.Date(69 - 1, origin = "2019-01-01")
# yday_commercialBegin = yday(commercialBegin)
# pre.period = c("2019-02-01", "2019-02-10") #1-29--2-10
# yday_pre.period = yday(pre.period)
# post.period = c(commercialBegin, commercialEnd)
# yday_post.period = yday(post.period)
# 
# # seasonality test
# seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
# summary(seasTest)  # period not long enough; choose seasonality
# 
# # trend test
# trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
# trendTest # trend not identified
# 
# entireImpact1.2 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000, nseasons = 7))
# plot(entireImpact1.2)
# entireImpact1.2$summary
# entireImpact1.2$report
# 
# # 311-407
# # website
# # washing machines
# set.seed(11)
# commercialBegin = as.Date(70 - 1, origin = "2019-01-01")
# commercialEnd = as.Date(97 - 1, origin = "2019-01-01")
# yday_commercialBegin = yday(commercialBegin)
# pre.period = c("2019-02-01", "2019-02-10") #1-29--2-10
# yday_pre.period = yday(pre.period)
# post.period = c(commercialBegin, commercialEnd)
# yday_post.period = yday(post.period)
# 
# # seasonality test
# seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
# summary(seasTest)  # seasonality not identified
# 
# # trend test
# trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
# trendTest # trend not identified
# 
# entireImpact1.3 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000))
# plot(entireImpact1.3)
# entireImpact1.3$summary
# entireImpact1.3$report

# 520-603
# website
# televisions
set.seed(11)
commercialBegin = "2019-05-20"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-05-06", "2019-05-19")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-20", "2019-06-03")
yday_post.period = yday(post.period)

# seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # period not long enough; choose seasonality

# trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # trend not identified

entireImpact2 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact2)
entireImpact2$summary
entireImpact2$report
plot(entireImpact2$model$bsts.model, "coef", inc = .1)

# 617-630
# website
# televisions
set.seed(11)
commercialBegin = "2019-06-17"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-06-04", "2019-06-16")
yday_pre.period = yday(pre.period)
post.period = c("2019-06-17", "2019-06-30")
yday_post.period = yday(post.period)

# seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # period not long enough; choose seasonality

# trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # trend not identified

entireImpact3 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact3)
entireImpact3$summary
entireImpact3$report
plot(entireImpact3$model$bsts.model, "coef", inc = .1)

# test for imaginary period
# 513-519
# website
set.seed(11)
commercialBegin = "2019-05-13"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-05-06", "2019-05-12")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-13", "2019-05-19")
yday_post.period = yday(post.period)

# seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # period not long enough; choose seasonality

# trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # trend not identified

entireImpact4 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.01, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact4)
entireImpact4$summary
entireImpact4$report
plot(entireImpact4$model$bsts.model, "coef", inc = .1)
