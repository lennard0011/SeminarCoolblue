# File for PLOTS only -- Seminar Coolblue 2020-2021 -- Team 21
# @authors: MELD


## Plot of traffic (visit density) an arbitrary day (2019-05-01) [Erik]
visitorsSumDay = subset(visitorsSum, date == "2019-05-01")
par(mfrow=c(2,2))
plot(visitorsSumDay$visitsWebNet, type = 'l', main = "Website-Netherlands visits on 2019-05-01",
     xaxt='n', yaxt = 'n', ann=FALSE)
# Including "indicators" for commercials (65 only Dutch on 2019-05-01)
broadDay <- subset(broad, date == "2019-05-01")
broadDay <- broadDay[order(broadDay$time),]
for (i in 1:nrow(broadDay)){
  abline(v = as.numeric(broadDay$time_min[i]), col = 'grey')
}
par(new=TRUE)
plot(visitorsSumDay$visitsWebNet, type = 'l', main = "Website-Netherlands visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
plot(visitorsSumDay$visitsAppNet, type = 'l', main = "App-Netherlands visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
plot(visitorsSumDay$visitsWebBel, type = 'l', main = "Website-Belgium visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
plot(visitorsSumDay$visitsAppBel, type = 'l', main = "App-Belgium visits on 2019-05-01",
     xlab ='Time (seconds)', ylab = 'Visit density')
rm(visitorsSumDay); rm(broadDay)


## Plots of visit density over 6 months (2x2) [Erik]
par(mfrow=c(2,2))
# plot Netherlands -- Website
plot(days_visWebNetSum[,2], type = "l", xaxt='n',  yaxt='n', ann=FALSE)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = '#DCDCDC', lwd = 3) # ads
}
par(new=TRUE)
plot(days_visWebNetSum[,2], las=1, type = "l", xaxt='n', xlab = "Time (months)", 
     ylab = 'Visit density', main = "Website traffic Netherlands (2019)")
axis(side =1, at=c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side =1, at=c(ceiling(0+(31-0)/2), ceiling(31+(59-31)/2), ceiling(59+(90-59)/2), 
                   (90+(120-90)/2), ceiling(120+(151-120)/2), ceiling(151+(181-151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = FALSE)
# plot Netherlands -- App
plot(days_visAppNetSum[,2], type = "l", xaxt='n',  yaxt='n', ann=FALSE)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = '#DCDCDC', lwd = 3) # ads
}
par(new=TRUE)
plot(days_visAppNetSum[,2], las=1, type = "l", xaxt='n', xlab = "Time (months)", 
     ylab = 'Visit density', main = "App traffic Netherlands (2019)")
axis(side =1, at=c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side =1, at=c(ceiling(0+(31-0)/2), ceiling(31+(59-31)/2), ceiling(59+(90-59)/2), 
                   (90+(120-90)/2), ceiling(120+(151-120)/2), ceiling(151+(181-151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = FALSE)
# plot Belgium -- Website
plot(days_visWebBelSum[,2], type = "l", xaxt='n',  yaxt='n', ann=FALSE)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesBel[i]), col = '#DCDCDC', lwd = 3) # ads
}
par(new=TRUE)
plot(days_visWebBelSum[,2], las=1, type = "l", xaxt='n', xlab = "Time (months)", 
     ylab = 'Visit density', main = "Website traffic Belgium (2019)")
axis(side =1, at=c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side =1, at=c(ceiling(0+(31-0)/2), ceiling(31+(59-31)/2), ceiling(59+(90-59)/2), 
                   (90+(120-90)/2), ceiling(120+(151-120)/2), ceiling(151+(181-151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = FALSE)
# plot Belgium -- App
plot(days_visAppBelSum[,2], type = "l", xaxt='n',  yaxt='n', ann=FALSE)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesBel[i]), col = '#DCDCDC', lwd = 3) # ads
}
par(new=TRUE)
plot(days_visAppBelSum[,2], las=1, type = "l", xaxt='n', xlab = "Time (months)", 
     ylab = 'Visit density', main = "App traffic Belgium (2019)")
axis(side =1, at=c(0, 31, 59, 90, 120, 151, 181), labels = NA)
axis(side =1, at=c(ceiling(0+(31-0)/2), ceiling(31+(59-31)/2), ceiling(59+(90-59)/2), 
                   (90+(120-90)/2), ceiling(120+(151-120)/2), ceiling(151+(181-151)/2)), 
     labels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), tick = FALSE)


#we moeten nog steeds bepalen of het zin heeft om dit te laten zien -- interpretatie is vaag
## Barplot of total Website traffic, per hour [Marjolein]
hours = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00")
value = c(avTrafficDayNetWebsite, avTrafficDayBelWebsite)
data = data.frame(categories, hours, value)
hourlyTraffic = ggplot(data, aes(fill=categories, y=value, x=hours)) + scale_fill_grey(start = 0.7, end = 0.4)  +  geom_bar(position="dodge", stat="identity")
print(hourlyTraffic + 
        labs(fill = "Countries", title = "Average amount of website visitors per hour", y = "Average amount of website visitors", x = "Hour of the day")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -0.5)) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text.x = element_text(color=c("black","transparent","transparent","transparent", "transparent","transparent", "black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


## Barplot of total App traffic, per hour [Marjolein]
value = c(av_traffic_day_net_app, av_traffic_day_bel_app)
data = data.frame(categories, hours, value)
hourly_traffic = ggplot(data, aes(fill=categories, y=value, x=hours)) + scale_fill_grey(start = 0.7, end = 0.4)  +  geom_bar(position="dodge", stat="identity")
print(hourly_traffic + 
        labs(fill = "Countries", title = "Average amount of app visitors per hour", y = "Average amount of app visitors", x = "Hour of the day")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -0.5)) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text.x = element_text(color=c("black","transparent","transparent","transparent", "transparent","transparent", "black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


## Barplot of total Broadcasts, per hour [Marjolein]
cat_net = rep("Netherlands", 24)
cat_bel = rep("Belgium", 24)
categories = c(cat_net, cat_bel)
value = c(av_broad_day_net, av_broad_day_bel)
data = data.frame(categories, hours, value)
hourly_traffic = ggplot(data, aes(fill=categories, y=value, x=hours)) + scale_fill_grey(start = 0.7, end = 0.4)  +  geom_bar(position="dodge", stat="identity")
print(hourly_traffic + 
        labs(fill = "Countries", title = "Average amount of broadcasts per hour", y = "Average amount of broadcasts", x = "Hour of the day")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -0.5)) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text.x = element_text(color=c("black","transparent","transparent","transparent", "transparent","transparent", "black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","transparent"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Plots of time before-after commercials with biggest GRP [Daniel]

# Plots of time before-after commercials with biggest pre-post visitors
