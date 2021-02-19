## SEMINAR COOLBLUE BA&QM 2021 Team 21
## @author: Erik van der Heide

# ===========================================================
#                 Descriptive of TRAFFIC
# ===========================================================

rawTraffic = read.csv(file.choose(), header = T)

# Separate countries
rawTrafficNet = subset(rawTraffic, country=="Netherlands")
sum(rawTrafficNet$visits_index)
rawTrafficBel = subset(rawTraffic, country=="Belgium")
sum(rawTrafficBel$visits_index)

# Bounces Net
rawTrafficBounceNet = subset(rawTrafficNet, bounces == 1)
100 * sum(rawTrafficBounceNet$visits_index) / sum(rawTrafficNet$visits_index)

# Bounces Bel
rawTrafficBounceBel = subset(rawTrafficBel, bounces == 1)
100 * sum(rawTrafficBounceBel$visits_index) /sum(rawTrafficBel$visits_index)

# Continue with no-bounce data
rawTrafficNet = subset(rawTrafficNet, bounces != 1 | is.na(bounces))
sum(rawTrafficNet$visits_index)
rawTrafficBel = subset(rawTrafficBel, bounces != 1 | is.na(bounces))
sum(rawTrafficBel$visits_index)

# Visit source Net
rawTrafficPushNotificationNet = subset(rawTrafficNet, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationNet$visits_index) / sum(rawTrafficNet$visits_index)

# Visit source Bel
rawTrafficPushNotificationBel = subset(rawTrafficBel, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationBel$visits_index) / sum(rawTrafficBel$visits_index)

# Visit source Net (App only)
rawTrafficNetApp = subset(rawTrafficNet, medium == "app")
sum(rawTrafficNetApp$visits_index)
rawTrafficPushNotificationNetApp = subset(rawTrafficNetApp, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationNetAoo$visits_index) / sum(rawTrafficNetApp$visits_index)

# Visit source Bel (App only)
rawTrafficBelApp = subset(rawTrafficBel, medium == "app")
sum(rawTrafficBelApp$visits_index)
rawTrafficPushNotificationBelApp = subset(rawTrafficBelApp, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationBelApp$visits_index) / sum(rawTrafficBelApp$visits_index)

# Continue with no-push data
rawTrafficNet = subset(rawTrafficNet, visit_source != "push notification")
sum(rawTrafficNet$visits_index)
rawTrafficBel = subset(rawTrafficBel, visit_source != "push notification")
sum(rawTrafficBel$visits_index)

# Medium Net
rawTrafficWebNet = subset(rawTrafficNet, medium == "website")
100 * sum(rawTrafficWebNet$visits_index) / sum(rawTrafficNet$visits_index)

# Medium Bel
rawTrafficWebBel = subset(rawTrafficBel, medium == "website")
100 * sum(rawTrafficWebBel$visits_index) / sum(rawTrafficBel$visits_index)

# ============================================================
#                 Descriptive of BROAD
# ============================================================

head(broad)

sort(summary(as.factor(broadNet$channel)))
sort(summary(as.factor(broadNet$program_before)))
sort(summary(as.factor(broadNet$program_after)))

sort(summary(as.factor(broadBel$channel)))
sort(summary(as.factor(broadBel$program_before)))
sort(summary(as.factor(broadBel$program_after)))

sort(summary(as.factor(broad$position_in_break)))
sum(broad$position_in_break == "99")

sort(summary(as.factor(broadNet$length_of_spot)))
sort(summary(as.factor(broadBel$length_of_spot)))

hist(broadNet$gross_rating_point)
hist(broadBel$gross_rating_point)

summary(broadNet$gross_rating_point)
nrow(subset(broadNet, gross_rating_point < 1))
summary(broadBel$gross_rating_point)
nrow(subset(broadBel, gross_rating_point < 1))

sort(summary(as.factor(broadNet$product_category)))
sort(summary(as.factor(broadBel$product_category)))

# Bekijk nog eens de grootste 10 reclames
broadMostViewed <- broad[order(broad$gross_rating_point, decreasing = T),]
broadMostViewed <- subset(broadMostViewed, gross_rating_point > 1)
broadMostViewed <- broadMostViewed[1:10,]

# Useful operator: subset
#traffic_day <- subset(traffic, grepl("2019-05-01", traffic$date_time) == TRUE)
#traffic_day <- traffic_day[order(traffic_day$date_time),]

# Test plots, but not very useful anymore
#plot all broadcasts
plot(adAmount)
for (i in 1:length(uniqueDates)){
  abline(v = yday(uniqueDatesNet[i]), col = 'blue') # Blue=Net only
  abline(v = yday(uniqueDatesBel[i]), col = 'red') # Red=Bel only
  abline(v = yday(uniqueDatesBoth[i]), col = 'green') # Green=Both
}

#plot broadcasts Net + dummies
plot(adAmountNet)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = 'blue') # ads
}
for (i in 1:length(holidaysDates)) {
  abline(v = yday(holidaysDates[i]), col = 'orange') # holidays
}

#plot online traffic + holiday dummies 
# plot Netherlands -- Website
plot(daysVisitorsSum[,2], type = "l", xaxt='n',  yaxt='n', ann=FALSE)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = '#DCDCDC', lwd = 3) # ads
}
for (i in 1:length(holidaysDates)) {
  abline(v = yday(holidaysDates[i]), col = 'orange') # holidays
}
par(new=TRUE)
plot(daysVisitorsSum[,2], las=1, type = "l", xaxt='n', xlab = " ", 
     ylab = 'Visit density', main = "Website traffic Netherlands (2019)",
     sub = "Nieuwjaarsdag (01JAN), Goede Vrijdag (19APR), Eerste Paasdag (21APR), 
            Tweede Paasdag (22APR), Koningsdag (27APR), Bevrijdingsdag (05MAY), 
            Hemelvaartsdag (30MAY), 1ste Pinksterdag (09JUN), 2e Pinksterdag (10JUN)")
axis(side =1, at=c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side =1, at=c(ceiling(0+(31-0)/2), ceiling(31+(59-31)/2), ceiling(59+(90-59)/2), 
                   (90+(120-90)/2), ceiling(120+(151-120)/2), ceiling(151+(181-151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = FALSE)

## Simple regressions for Netherlands #####################################

#simple AR(1)
## NOTE: IF YOU RUN REGR WITH LAG(), THEN YOU NEED TO RELOAD DPLYR
library("dplyr")
timeSeriesWebNet = as.numeric(daysVisitorsSum[,2])
regrAR1WebNet = lm(timeSeriesWebNet ~ lag(timeSeriesWebNet, 1))
summary(regrAR1WebNet)
regrAR1AppNet = lm(as.numeric(daysVisitorsSum[,3]) ~ lag(as.numeric(daysVisitorsSum[,3]), 1))
summary(regrAR1AppNet)
regrAR1WebBel = lm(as.numeric(daysVisitorsSum[,4]) ~ lag(as.numeric(daysVisitorsSum[,4]), 1))
summary(regrAR1WebBel)
regrAR1AppBel = lm(as.numeric(daysVisitorsSum[,5]) ~ lag(as.numeric(daysVisitorsSum[,5]), 1))
summary(regrAR1AppBel)

# continue with WebNet only (timeSeriesWebNet)

#on a trend
regrTrend <- lm(timeSeriesWebNet ~ c(1:181))
summary(regrTrend)
timeSeriesWebNetDiff <- c(NA, diff(timeSeriesWebNet)) # first difference
regrTrend2 <- lm(timeSeriesWebNetDiff ~ 1) # should be the same
summary(regrTrend2)

#on weekdays
regrDays = lm(timeSeriesWebNetDiff ~ 0 + dummyWeekdays[,2] + dummyWeekdays[,3] +
                dummyWeekdays[,4] + dummyWeekdays[,5] + dummyWeekdays[,6] +
                dummyWeekdays[,7] + dummyWeekdays[,8])
names(regrDays$coefficients) <- c('monday','tuesday','wednesday','thursday',
                                  'friday','saturday','sunday')
summary(regrDays)
# TODO: F-test on whether weekends significantly differ from weekdays.

#on months
regrMonths = lm(timeSeriesWebNetDiff ~ 0 + dummyMonths[,2] + dummyMonths[,3] + 
                  dummyMonths[,4] + dummyMonths[,5] + dummyMonths[,6] + 
                  dummyMonths[,7])
names(regrMonths$coefficients) <- c('january','february','march','april','may',
                                    'june')
summary(regrMonths) # none of them are significant

#on holidays
regrHolidays <- lm(timeSeriesWebNet ~ lag(timeSeriesWebNet, 1) + dummyHolidays)
summary(regrHolidays)

#on Hemelvaartsdag (aberrant observation?)
regrHemelvaartsdag <- lm(timeSeriesWebNet ~ lag(timeSeriesWebNet, 1) + 
                           dummyHemelvaartsdag)
summary(regrHemelvaartsdag)

#on ads
regrAdsNet <- lm(timeSeriesWebNet ~ lag(timeSeriesWebNet, 1) + dummyAds[,2])
summary(regrAdsNet)

#on lagged ads
regrAdsNetLag <- lm(timeSeriesWebNet ~ lag(timeSeriesWebNet, 1) + dummyAds[,2] 
                 + lag(dummyAds[,2], 1))
summary(regrAdsNetLag)

# POSSIBLE TODO:
  #testing for a unit root (slide 28-30 TRA)
  #create best ARMA(p,q) model: lowest AIC&SIC, plot autocorrelations (AR1=to0)
  #inclusion of level and/or trend  
  #misspecification tests (slide 21-23 BasicConc2 TRA / Ectrie 2)
  #F-test on whether weekends significantly differ from weekdays.