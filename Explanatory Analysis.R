# Seminar Coolblue 2021 -- Explanatory Analysis
# @author: Lennard van der Plas, Erik van der Heide, Marjolein de With, Daniel Buijs

#install.packages('stringr')
library("stringr")
# ===========================================================
#                 Descriptive of TRAFFIC
# ===========================================================

rawTraffic = read.csv(file.choose(), header = T)

# Separate countries
rawTrafficNet = subset(rawTraffic, country == "Netherlands")
sum(rawTrafficNet$visits_index)
rawTrafficBel = subset(rawTraffic, country == "Belgium")
sum(rawTrafficBel$visits_index)

# Bounces Netherlands
rawTrafficBounceNet = subset(rawTrafficNet, bounces == 1)
100 * sum(rawTrafficBounceNet$visits_index) / sum(rawTrafficNet$visits_index)

# Bounces Belgium
rawTrafficBounceBel = subset(rawTrafficBel, bounces == 1)
100 * sum(rawTrafficBounceBel$visits_index) /sum(rawTrafficBel$visits_index)

# Continue with no-bounce data
rawTrafficNet = subset(rawTrafficNet, bounces != 1 | is.na(bounces))
sum(rawTrafficNet$visits_index)
rawTrafficBel = subset(rawTrafficBel, bounces != 1 | is.na(bounces))
sum(rawTrafficBel$visits_index)

# Visit source Netherlands
rawTrafficPushNotificationNet = subset(rawTrafficNet, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationNet$visits_index) / sum(rawTrafficNet$visits_index)

# Visit source Belgium
rawTrafficPushNotificationBel = subset(rawTrafficBel, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationBel$visits_index) / sum(rawTrafficBel$visits_index)

# Visit source Netherlands - app
rawTrafficNetApp = subset(rawTrafficNet, medium == "app")
sum(rawTrafficNetApp$visits_index)
rawTrafficPushNotificationNetApp = subset(rawTrafficNetApp, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationNetAoo$visits_index) / sum(rawTrafficNetApp$visits_index)

# Visit source Belgium - app
rawTrafficBelApp = subset(rawTrafficBel, medium == "app")
sum(rawTrafficBelApp$visits_index)
rawTrafficPushNotificationBelApp = subset(rawTrafficBelApp, visit_source == "push notification")
100 * sum(rawTrafficPushNotificationBelApp$visits_index) / sum(rawTrafficBelApp$visits_index)

# Continue with no push data
rawTrafficNet = subset(rawTrafficNet, visit_source != "push notification")
sum(rawTrafficNet$visits_index)
rawTrafficBel = subset(rawTrafficBel, visit_source != "push notification")
sum(rawTrafficBel$visits_index)

# Medium Netherlands
rawTrafficWebNet = subset(rawTrafficNet, medium == "website")
100 * sum(rawTrafficWebNet$visits_index) / sum(rawTrafficNet$visits_index)

# Medium Belgium
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
summary(as.factor(broadBel$position_in_break_3option))

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

# =============================================================
#                       PLOTTING
# =============================================================

# Look at 10 biggest commercials
broadMostViewed = broad[order(broad$gross_rating_point, decreasing = T), ]
broadMostViewed = subset(broadMostViewed, gross_rating_point > 1)
broadMostViewed = broadMostViewed[1:10, ]

## Plot of traffic (visit density) an arbitrary day (2019-05-01) 
visitorsSumDay = subset(visitorsSum, date == "2019-05-01")
par(mfrow = c(2, 2))
plot(visitorsSumDay$visitsWebNet, type = 'l', main = "Website-Netherlands visits on 2019-05-01",
     xaxt = 'n', yaxt = 'n', ann = F)
# Including "indicators" for commercials (65 only Dutch on 2019-05-01)
broadDay = subset(broad, date == "2019-05-01")
broadDay = broadDay[order(broadDay$time), ]
for (i in 1:nrow(broadDay)){
  abline(v = as.numeric(broadDay$time_min[i]), col = 'grey')
}
par(new = T)
plot(visitorsSumDay$visitsWebNet, type = 'l', main = "Website-Netherlands visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
plot(visitorsSumDay$visitsAppNet, type = 'l', main = "App-Netherlands visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
plot(visitorsSumDay$visitsWebBel, type = 'l', main = "Website-Belgium visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
plot(visitorsSumDay$visitsAppBel, type = 'l', main = "App-Belgium visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
rm(visitorsSumDay); rm(broadDay)


# Plots of visit density over 6 months (2x2) 
par(mfrow = c(2, 2))
# Plot Netherlands -- Website
plot(as.numeric(daysVisitorsSum[, 2])/as.numeric(max(daysVisitorsSum[,2])), type = "l", xaxt = 'n',  yaxt = 'n', ann = F)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = '#DCDCDC', lwd = 3) # Ads
}
par(new = T)
plot(as.numeric(daysVisitorsSum[, 2])/as.numeric(max(daysVisitorsSum[,2])), las = 1, type = "l", xaxt = 'n', xlab = "Time (months)", 
     ylab="", main = "Website traffic Netherlands (2019)")
axis(side = 1, at = c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side = 1, at = c(ceiling(0 + (31 - 0)/2), ceiling(31 + (59 - 31)/2), ceiling(59 + (90 - 59)/2), 
                      (90 + (120 - 90)/2), ceiling(120 + (151 - 120)/2), ceiling(151 + (181 - 151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = F)

# Plot Netherlands - app
plot(as.numeric(daysVisitorsSum[, 3])/as.numeric(max(daysVisitorsSum[, 2])), type = "l", xaxt = 'n',  yaxt = 'n', ann = F)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = '#DCDCDC', lwd = 3) # Ads
}
par(new = T)
plot(as.numeric(daysVisitorsSum[, 3])/as.numeric(max(daysVisitorsSum[,2])), las=1, type = "l", xaxt = 'n', xlab = "Time (months)", 
     ylab = '', main = "App traffic Netherlands (2019)")
axis(side = 1, at = c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side = 1, at = c(ceiling(0 + (31 - 0)/2), ceiling(31 + (59 - 31)/2), ceiling(59 + (90 - 59)/2), 
                      (90 + (120 - 90)/2), ceiling(120 + (151 - 120)/2), ceiling(151 + (181 - 151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = F)

# Plot Belgium - Website
plot(as.numeric(daysVisitorsSum[, 4])/as.numeric(max(daysVisitorsSum[,4])), type = "l", xaxt = 'n',  yaxt = 'n', ann = F)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesBel[i]), col = '#DCDCDC', lwd = 3) # Ads
}
par(new = T)
plot(as.numeric(daysVisitorsSum[, 4])/as.numeric(max(daysVisitorsSum[,4])), las = 1, type = "l", xaxt = 'n', xlab = "Time (months)", 
     ylab = '', main = "Website traffic Belgium (2019)")
axis(side = 1, at = c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side = 1, at = c(ceiling(0 + (31 -0)/2), ceiling(31 + (59 - 31)/2), ceiling(59 + (90 - 59)/2), 
                      (90 + (120 - 90)/2), ceiling(120 + (151 - 120)/2), ceiling(151 + (181 - 151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = F)

# Plot Belgium - app
plot(as.numeric(daysVisitorsSum[, 5])/as.numeric(max(daysVisitorsSum[,4])), type = "l", xaxt = 'n',  yaxt = 'n', ann = F)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesBel[i]), col = '#DCDCDC', lwd = 3) # Ads
}
par(new = T)
plot(as.numeric(daysVisitorsSum[, 5])/as.numeric(max(daysVisitorsSum[, 4])), las = 1, type = "l", xaxt = 'n', xlab = "Time (months)", 
     ylab = '', main = "App traffic Belgium (2019)")
axis(side = 1, at = c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side = 1, at = c(ceiling(0 + (31 - 0)/2), ceiling(31 + (59 - 31)/2), ceiling(59 + (90 - 59)/2), 
                      (90 + (120 - 90)/2), ceiling(120 + (151 - 120)/2), ceiling(151 + (181 - 151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = F)

# Barplot of total Broadcasts, per hour 
avBroadDayNet = matrix(0, 25)
for (i in 1:24){
  totBroadDayNetSubset = subset(broadNet, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayNet[i] = nrow(totBroadDayNetSubset)
}
avBroadDayBel = matrix(0, 25)
for (i in 1:24){
  totBroadDayBelSubset = subset(broadBel, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  avBroadDayBel[i] = nrow(totBroadDayBelSubset)
}

cat_net = rep("Netherlands", 25)
cat_bel = rep("Belgium", 25)
categories = c(cat_net, cat_bel)
hours = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "24:00")
value = c(avBroadDayNet, avBroadDayBel)
data = data.frame(categories, hours, value)
hourly_traffic = ggplot(data, aes(fill = categories, y = value, x = hours)) + scale_fill_grey(start = 0.7, end = 0.4)  +  geom_bar(position="dodge", stat="identity")
print(hourly_traffic + 
        labs(fill = "Countries", title = "Total amount of broadcasts per hour", y = "Total amount of broadcasts", x = "Hour of the day")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -0.5)) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text.x = element_text(color = c("black","transparent","transparent","transparent", "transparent","transparent", "black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent", "black"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Data for plot average of hour over the days
# Calculate average for different searches Netherlands - website
avTrafficDayNetWebsite = matrix(0, 25)
maxHourVisitors = 0
for (i in 1:24){
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  hourVisitors = matrix(NA, amountDays)
  for (j in 1:amountDays){
    dateJ = as.Date(j - 1, origin = "2019-01-01")
    hourVisitors[j] = sum(visitorsSumSubset[visitorsSumSubset$date == dateJ, ]$visitsWebNet)
    if (hourVisitors[j] > maxHourVisitors){
      maxHourVisitors = hourVisitors[j]
    }
  }
  avTrafficDayNetWebsite[i] = mean(hourVisitors)
}
avTrafficDayNetWebsite = avTrafficDayNetWebsite/maxHourVisitors

# Calculate average for different searches  Belgium - website
avTrafficDayBelWebsite = matrix(0, 25)
maxHourVisitors = 0
for (i in 1:24){
  print(i)
  visitorsSumSubset = subset(visitorsSum, (time_min >= (i - 1) * 60) & (time_min < i * 60))
  hourVisitors = matrix(NA, amountDays)
  for (j in 1:amountDays){
    dateJ = as.Date(j - 1, origin = "2019-01-01")
    hourVisitors[j] = sum(visitorsSumSubset[visitorsSumSubset$date == dateJ, ]$visitsWebBel)
    if (hourVisitors[j] > maxHourVisitors){
      maxHourVisitors = hourVisitors[j]
    }
  }
  avTrafficDayBelWebsite[i] = mean(hourVisitors)
}
avTrafficDayBelWebsite = avTrafficDayBelWebsite/maxHourVisitors

## Barplot of total Website traffic, per hour 
value = c(avTrafficDayNetWebsite, avTrafficDayBelWebsite)
data = data.frame(categories, hours, value)
hourlyTraffic = ggplot(data, aes(fill = categories, y = value, x = hours)) + scale_fill_grey(start = 0.7, end = 0.4)  +  geom_bar(position="dodge", stat="identity")
print(hourlyTraffic + 
        labs(fill = "Countries", title = "Average visit density per hour", y = "Average visit density", x = "Hour of the day")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -0.5)) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text.x = element_text(color = c("black","transparent","transparent","transparent", "transparent","transparent", "black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent", "black"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

## Plots of time before-after commercials with biggest GRP
## If you want to plot a specific plot, change plots to one,
## and set the value for j in the for loop.
broad = broad[order(broad$gross_rating_point, decreasing = T), ]
interval = 120
plots = 2
par(mfrow = c(1, 1))
rangeValues = matrix(rep(0, plots))
for(j in 1:plots){
  # j = 1 #only possible if plots is equal to 1
  datecommercial = broad[j, "date"]
  timecommercial = broad[j, "time"]
  
  traffic_datesub = subset(visitorsSum,grepl(datecommercial, visitorsSum$date) == T)
  
  timecommercial = str_split_fixed(timecommercial, ":", 3)
  colnames(timecommercial) = c("hour", "minute", "seconds")
  timecommercial = data.frame(timecommercial)
  timecommercial = 60 * as.numeric(timecommercial[1,"hour"]) + as.numeric(timecommercial[1, "minute"]) + 1
  
  timeStart = timecommercial - interval
  timeEinde = timecommercial + interval
  totalLength = 2 * interval + 1
  visitsVector = as.matrix(rep(0, totalLength))
  row.names(visitsVector) = c(seq(from = -120, to = 120))
  
  for(i in 1:totalLength){
    visitsVector[i] = traffic_datesub[(timeStart + i - 1), "visitsWebNet"]
  }
  
  visitsMean = rollmeanr(visitsVector, 10, align = 'center', fill = NA)
  firstMean = mean(visitsVector[1:(interval - 5)])
  secondMean =  mean(visitsVector[(interval + 40): (2 * interval -50)])
  row.names(visitsMean) = c(seq(from = timeStart, to = timeEinde))
  
  plot(visitsVector, type = "l", xlim = c(90, 150), xaxt = 'n', main = "Website visits", xlab = "Time (minutes)", ylab = "Visit Density")
  axis(side = 1, at = c(90, 121, 126), 
       labels= c('-40', '0', '5'))
  abline(v = interval + 1, col = "red")
  maxie = max(visitsVector[(121 + 1):(121 + 5)]) - visitsVector[121]
  maxie_per = 100 * (max(visitsVector[(121 + 1):(121 + 5)]) - visitsVector[121])/visitsVector[121]
  legend(x = "topleft", legend = c(paste0("VD increase in 5min: "), paste0(round(maxie,digits=2), " (", round(maxie_per,digits=2), "%)")), cex = 0.75)
  
  endMon = 0.5*interval
  if(firstMean > secondMean){
    for(i in 1:endMon){
      if(visitsVector[interval + i] > firstMean){
        rangeValues[j] = rangeValues[j] + 1
      }
    }
  }
  if(secondMean > firstMean){
    for(i in 1:40){
      if(visitsVector[interval + i] > secondMean){
        rangeValues[j] = rangeValues[j] + 1
      }
    }
  }
}
print(mean(rangeValues))
broad = broad[order(as.numeric(row.names(broad))), ]