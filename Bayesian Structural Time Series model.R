# Google Trends
Sys.setenv(TZ = "Europe/Amsterdam")

mediaMarktTrends = gtrends(keyword = "mediamarkt", geo = "NL", time = "2019-01-01 2019-06-30")
interestVectorMed = mediaMarktTrends$interest_over_time
plot(interestVectorMed$hits, type = 'l', xlab = "Day of the year", ylab = "MediaMarkt searches")

BCCTrends = gtrends(keyword = "bcc", geo = "NL", time = "2019-01-01 2019-06-30")
interestVectorBCC = BCCTrends$interest_over_time
plot(interestVectorBCC$hits, type = 'l', xlab = "Day of the year", ylab = "BCC searches")

# BSTS
# Sum of visit_index - Netherlands
visWebNet$yday = yday(visWebNet$date)
sumVisitIndexNet = matrix(0, amountDays)
for (i in 1:amountDays){
  print(i)
  trafficSubset = subset(visWebNet, yday == i)
  sumVisitIndexNet[i] = sum(trafficSubset$visits_index)
}
visWebNet = subset(visWebNet, select = -yday)

# Sum of visit_index - Belgium
visWebBel$yday = yday(visWebBel$date)
sumVisitIndexBel = matrix(0, amountDays)
for (i in 1:amountDays){
  print(i)
  trafficSubset = subset(visWebBel, yday == i)
  sumVisitIndexBel[i] = sum(trafficSubset$visits_index)
}
visWebBel = subset(visWebBel, select = -yday)

# Some tests
# Seasonality test
seasTest = wo(sumVisitIndexNet, freq = 7)
summary(seasTest) # Weekly seasonality; does contain seasonality

# Trend test
trendTest = trend.test(sumVisitIndexNet) # No trend
trendTest


# 211-407
# All products
data = zoo(cbind(sumVisitIndexNet, sumVisitIndexBel, interestVectorMed[, 2], interestVectorBCC[, 2]), c(1:181))
set.seed(11)
commercialBegin = "2019-02-11"
commercialEnd = "2019-04-07"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-01-29", "2019-02-10")
yday_pre.period = yday(pre.period)
post.period = c(commercialBegin, commercialEnd)
yday_post.period = yday(post.period)

# Seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # Seasonality identified

# Trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # Trend not identified

entireImpact1 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.05, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact1)
entireImpact1$summary
entireImpact1$report
plot(entireImpact1$model$bsts.model, "coef", inc = .1)


# 520-603
# Televisions
set.seed(11)
commercialBegin = "2019-05-20"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-05-06", "2019-05-19")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-20", "2019-06-03")
yday_post.period = yday(post.period)

# Seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # Period not long enough; choose seasonality

# Trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # Trend not identified

entireImpact2 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.05, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact2)
entireImpact2$summary
entireImpact2$report
plot(entireImpact2$model$bsts.model, "coef", inc = .1)

# 617-630
# Televisions
set.seed(11)
commercialBegin = "2019-06-17"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-06-04", "2019-06-16")
yday_pre.period = yday(pre.period)
post.period = c("2019-06-17", "2019-06-30")
yday_post.period = yday(post.period)

# Seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # Period not long enough; choose seasonality

# Trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # Trend not identified

entireImpact3 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.05, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact3)
entireImpact3$summary
entireImpact3$report
plot(entireImpact3$model$bsts.model, "coef", inc = .1)

# Test for imaginary period
# 513-519
set.seed(11)
commercialBegin = "2019-05-13"
yday_commercialBegin = yday(commercialBegin)
pre.period = c("2019-05-06", "2019-05-12")
yday_pre.period = yday(pre.period)
post.period = c("2019-05-13", "2019-05-19")
yday_post.period = yday(post.period)

# Seasonality test
seasTest = wo(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]], freq = 7)
summary(seasTest)  # Period not long enough; choose seasonality

# Trend test
trendTest = trend.test(sumVisitIndexNet[yday_post.period[1]:yday_post.period[2]]) 
trendTest # Trend not identified

entireImpact4 = CausalImpact(data, yday_pre.period, yday_post.period, alpha = 0.05, model.args = list(niter = 10000, nseasons = 7))
plot(entireImpact4)
entireImpact4$summary
entireImpact4$report
plot(entireImpact4$model$bsts.model, "coef", inc = .1)