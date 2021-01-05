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
