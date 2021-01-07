#install packages
install.packages("ggplot2")
install.packages("lubridate")

#load packages
library("ggplot2")
library("lubridate")

#data
broadcasting = read.csv(file.choose())
traffic = read.csv(file.choose())

#subset data
traffic_netherlands = subset(traffic, country == 'Netherlands')
traffic_belgium = subset(traffic, country == 'Belgium')
broadcast_netherlands = subset(broadcasting, country == 'Netherlands')
broadcast_belgium = subset(broadcasting, country == 'Belgium')


#plot broadcasts nl-be
amountDays = 31 + 28 + 31 + 30 + 31 + 30
scopeDays = amountDays
uniqueDatesBel = unique(broadcast_belgium$date)
uniqueDatesNet = unique(broadcast_netherlands$date)
uniqueDatesBoth = base::intersect(uniqueDatesBel, uniqueDatesNet) #adverts in both on certain day
uniqueDatesBel = base::setdiff(uniqueDatesBel, uniqueDatesBoth) #adverts only in Belgium on certain day
uniqueDatesNet = base::setdiff(uniqueDatesNet, uniqueDatesBoth) #adverts only in Netherlands on certain day
adAmount = matrix(0, scopeDays)
for (i in 1:scopeDays){
  iDate = as.Date(i - 1, origin = "2019-01-01")
  adsIDate = sum(broadcasting$date == iDate)
  adAmount[i] = adsIDate
}
plot(adAmount)
for (i in 1:NROW(uniqueDates)){
  abline(v = yday(uniqueDatesBel[i]), col = 'blue')
  abline(v = yday(uniqueDatesNet[i]), col = 'red')
  abline(v = yday(uniqueDatesBoth[i]), col = 'green')
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
hitsPerDay = matrix(0, scopeDays)
for (i in 1:scopeDays){
  print(i)
  iDate = as.Date(i - 1, origin = "2019-01-01")
  hitsPerDay[i] = sum(traffic_netherlands$date == iDate)
}
plot(hitsPerDay)
for (i in 1:NROW(uniqueDates)){
  abline(v = yday(uniqueDates[i]), col = 'blue')
}