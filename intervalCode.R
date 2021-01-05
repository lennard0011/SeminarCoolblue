#change to intervals
library(chron)

traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)


broad['travelBefore'] <- 0
 
nBroad <- nrow(broad)
nTraffic <- nrow(traffic)

for (index in 1:nBroad) {
  preTrafficcount <- 0
  date <- as.character(broad$date[[index]])
  time <- broad$time[[index]]
  
  timeSeconds <- 60 * 60 * 24 * as.numeric(times(time))
  
  for (indexTravel in 1:1000) {
    trafficDate <- as.character(traffic$date_time[[indexTravel]])
    trafficDateSplit <- strsplit(testtraffic, "\\s+")[[1]]
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

