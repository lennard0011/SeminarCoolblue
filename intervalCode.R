#change to intervals
library(chron)

traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

nBroad <- nrow(broad)
nTraffic <- nrow(traffic)

#add time_min to every broadcast
broad['time_min'] <- 0
for (index in 1:nBroad) { #nBroad
  time <- broad$time[[index]]
  timeMinute <- 60 * 24 * as.numeric(times(time))
  broad$time_min[[index]] <- timeSeconds
}
#add time_min to every travel
traffic['time_min'] <- 0
traffic$date_time <- as.character(traffic$date_time)

for (indexTravel in 1:nTraffic) {
  trafficDateSplit <- strsplit(trafficDate, "\\s+")[[1]]
  trafficDate <- trafficDateSplit[1]
  trafficTime <- trafficDateSplit[2]
  
  traffic$time_min[[indexTravel]] <- 60 * 24 * as.numeric(times(trafficTime))
  if( indexTravel %% 1000 == 0) { print(indexTravel)}
}


#count visits per minute
for (index in 1:3) { #nBroad
  preTrafficcount <- 0
  date <- as.character(broad$date[[index]])
  time <- broad$time[[index]]
  
  timeSeconds <- 60 * 60 * 24 * as.numeric(times(time))
  
  print(timeSeconds)
  
  for (indexTravel in 1:1) { #nTraffic
    trafficDate <- as.character(traffic$date_time[[indexTravel]])
    trafficDateSplit <- strsplit(trafficDate, "\\s+")[[1]]
    trafficDate <- trafficDateSplit[1]
    trafficTime <- trafficDateSplit[2]
    
    trafficTimeSeconds <- 60 * 60 * 24 * as.numeric(times(trafficTime))
    
    if(date == trafficDate) {
      print("found!")
      preTrafficcount = preTrafficcount + 1
    }
    
  }
   
  
  broad$travelBefore[[index]] <- preTrafficcount
}

