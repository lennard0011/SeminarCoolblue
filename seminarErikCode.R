## SEMINAR COOLBLUE BA&QM 2021 Team 21
## @author: Erik van der Heide

# Load data
traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

# Descriptive of TRAFFIC 
head(traffic)
summary(traffic)

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
#sum(traffic$bounces == 1) # doesn't work

unique(traffic$country)
sum(traffic$country == "Netherlands")
sum(traffic$country == "Belgium")

# Descriptive of BROAD
summary(broad)
unique(broad$operator)
unique(broad$channel)
unique(broad$date)
unique(broad$time)
unique(broad$position_in_break)
unique(broad$length_of_spot)
unique(broad$program_before)
unique(broad$program_after)
unique(broad$program_category_before)
unique(broad$program_category_after)
unique(broad$gross_rating_point)
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
plot(visit_density, main = "Number of Visitors on May 1, 2019")

# Create "indicators" for commercials (given there are on this day)
broad_day <- subset(broad, date == "2019-05-01")
broad_day <- broad_day[order(broad_day$time),]
time_min <- vector(mode ="character", length = nrow(broad_day))
# convert times to numbers
for (i in 1:nrow(broad_day)) {
  time_min[i] <- gsub(":", "", broad_day$time[i])
  time_min[i] <- as.numeric(time_min[i]) / 10000
  time_min[i] <- floor(as.numeric(time_min[i]))*60 + 100*(as.numeric(time_min[i])-floor(as.numeric(time_min[i]))) + 1
}
broad_day <- cbind(broad_day, time_min)
