## SEMINAR COOLBLUE BA&QM 2021 Team 21
## @author: Erik van der Heide
## note to myself: use rm(var) to clean up workspace (environment)
## note to myself: dplyr packages takes care of lag()

# Packages
install.packages("dplyr")
library("dplyr")

# Load data
broad <- read.csv(file.choose(), header = T)
broad <- broad[order(broad$date, broad$time),]
traffic <- read.csv(file.choose(), header = T)

# Descriptive of TRAFFIC 
head(traffic)
min(traffic$date_time)
max(traffic$date_time)
summary(traffic$visits_index)
unique(traffic$medium)
sum(traffic$medium == "app")
sum(traffic$medium == "website")
unique(traffic$visit_source)
sum(traffic$visit_source =="push notification")
sum(traffic$visit_source =="direct")
sum(traffic$visit_source =="search")
sum(traffic$visit_source =="paid search")
sum(traffic$visit_source =="other")
unique(traffic$page_category)
sum(traffic$page_category == "home")
sum(traffic$page_category == "other")
sum(traffic$page_category == "product")
summary(traffic$avg_session_quality)
summary(traffic$bounces)
count_bounces <- traffic$bounces
sum(na.omit(count_bounces) == 0)
unique(traffic$country)
sum(traffic$country == "Netherlands")
sum(traffic$country == "Belgium")

# Descriptive of BROAD (I took most occuring from Jupyter)
head(broad)
unique(broad$operator)
unique(broad$channel)
unique(broad$date)
unique(broad$time)
unique(broad$position_in_break)
sum(broad$position_in_break == "99")
unique(broad$length_of_spot)
unique(broad$program_before)
unique(broad$program_after)
unique(broad$program_category_before)
unique(broad$program_category_after)
summary(broad$gross_rating_point)
unique(broad$product_category)
unique(broad$country)

# Select one day from data (Wednesday May 1, 2019):
traffic_day <- subset(traffic, grepl("2019-05-01", traffic$date_time) == TRUE)
traffic_day <- traffic_day[order(traffic_day$date_time),]

# Aggregate visits for every minute of the day 
# assumption: there is at least 1 visit every minute of the day
minute_counter = 1 # will go up to 1440
visit_density <- vector(mode="integer", length=1440)
# small neglection: the first obsv. is not counted
for (i in 2:nrow(traffic_day)) {
  if (traffic_day$date_time[i] == traffic_day$date_time[i-1]) {
    visit_density[minute_counter] = visit_density[minute_counter] + 1
  } else {
    minute_counter = minute_counter + 1
  }
}

plot(visit_density, main = "Number of Visitors on May 1, 2019", type = "l")

# Create "indicators" for commercials (given there are on this day)
broad_day <- subset(broad, date == "2019-05-01")
broad_day <- broad_day[order(broad_day$time),]

# Plotting
time_interval = 1:1400 # standard is 1:1400
plot(visit_density[time_interval], main = "Number of Visitors on May 1, 2019")
for (i in 1:nrow(broad_day)){
  #if (as.numeric(broad_day$time_min[i]) >= 1000 & as.numeric(broad_day$time_min[i]) <= 1100) {
    abline(v = as.numeric(broad_day$time_min[i]), col = 'blue')
  #}
}

# Plot data on daily basis

#plot all broadcasts (partly copied from Marjolein)
plot(adAmount)
for (i in 1:nrow(uniqueDates)){
  abline(v = yday(uniqueDatesNet[i]), col = 'blue') # Blue=Net only
  abline(v = yday(uniqueDatesBel[i]), col = 'red') # Red=Bel only
  abline(v = yday(uniqueDatesBoth[i]), col = 'green') # Green=Both
}

#plot broadcasts Net + dummies (partly copied from Marjolein)
plot(adAmountNet)
for (i in 1:length(uniqueDatesNet)){
  abline(v = yday(uniqueDatesNet[i]), col = 'blue') # ads
}
for (i in 1:length(holidaysDates)) {
  abline(v = yday(holidaysDates[i]), col = 'orange') # holidays
}

#plot daily traffic Net (partly copied from Marjolein)
plot(trafAmountNet)
for (i in 1:length(uniqueDatesNet)){ 
  abline(v = yday(uniqueDatesNet[i]), col = 'blue') # ads
}
for (i in 1:length(holidaysDates)) {
  abline(v = yday(holidaysDates[i]), col = 'orange') # holidays
} # global spike on Hemelvaartsdag in Net
for (i in 1:length(trafAmountNet)) {
  if (wday(unique(traffic$date)[i]) == 3) { # wednesdays
    abline(v = yday(unique(traffic$date)[i]), col = 'yellow') # plot all mondays
  }
}

#plot daily traffic Bel (partly copied from Marjolein)
plot(trafAmountBel)
for (i in 1:length(uniqueDatesBel)){
  abline(v = yday(uniqueDatesBel[i]), col = 'red') # ads
}
for (i in 1:length(holidaysDates)) {
  abline(v = yday(holidaysDates[i]), col = 'orange') # holidays
} # local spike on Hemvelvaartsdag in Bel

# Simple regression for Net
simpleAR1 <- lm(trafAmountNet ~ lag(trafAmountNet, 1))
summary(simpleAR1)
# TODO:
  #regress on day-dummies
  #regress on all holidays
  #regress on hemelvaartsdag
  #regress on weekdummies
  #regress on a linear trend
  #regress with first differences
  #unit root test
  #other tests from time series analyses / AMM W3
