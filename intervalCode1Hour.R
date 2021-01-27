# Seminar Coolblue 2021 - Direct effects model (1-hour diff-in-diff interval)
# For the direct effects model we calculate the amount of traffic in an interval before the broadcast and after the broadcast
# Results are stored in the column preVisitors and postVisitors in the dataframe broadMostViewed

## ========================================================
##                  Collect interval data
## ========================================================

#get commercials only in NL, with BE control
# OPTIONAL: delete lowest GRP ratios
#broadMostViewed = broad[order(broad$gross_rating_point, decreasing = T),]
#broadMostViewed = subset(broadMostViewed, gross_rating_point > 1)
broadNet1 = subset(broad, country == "Netherlands")
broadNet1 = broadNet1[order(broadNet1$date),]
broadNet1 = subset(broadNet1, select = -c(preVisitorsWeb, preVisitorsApp, postVisitorsWeb, postVisitorsApp) ) # this is not always necessary

broadNet1['preVisitorsWebNet'] = 0
broadNet1['postVisitorsWebNet'] = 0
broadNet1['preVisitorsAppNet'] = 0
broadNet1['postVisitorsAppNet'] = 0

broadNet1['preVisitorsWebBel'] = 0
broadNet1['postVisitorsWebBel'] = 0
broadNet1['preVisitorsAppBel'] = 0
broadNet1['postVisitorsAppBel'] = 0

# Remove commercials in Belgium sales period (assumed whole January)
broadNet1 = subset(broadNet1, yday(date) > 31)

# Remove commercials where Bel has a commercial as well
belAdDates = sort(unique(subset(broad, country == "Belgium")$date))
for (i in 1:nrow(broadNet1)) {
  for (j in 1:length(belAdDates)) {
    if (broadNet1[i,]$date == belAdDates[j]) {
      broadNet1[i,]$channel = NA
    }
  }
}
broadNet1 = subset(broadNet1, !(is.na(channel)))
print(paste0("Number of commercials: ", nrow(broadNet1)))

# Calculate pre- and post visitors, Net and Bel, Web and App (1 hour)
intervalSize = 60
start = Sys.time()
for (i in 1:nrow(broadNet1)) {
  broadDate = broadNet1$date[[i]]
  broadTime = broadNet1$time_min[[i]]
  
  # TO-DO count extraViewers from day before or day after at midnight!
  
  broadNet1$preVisitorsWebNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebNet)
  broadNet1$preVisitorsAppNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppNet)
  
  broadNet1$preVisitorsWebBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsWebBel)
  broadNet1$preVisitorsAppBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min < broadTime & time_min >= broadTime - intervalSize)$visitsAppBel)
    
  broadNet1$postVisitorsWebNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebNet)
  broadNet1$postVisitorsAppNet[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppNet)
  
  broadNet1$postVisitorsWebBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsWebBel)
  broadNet1$postVisitorsAppBel[[i]] = sum(subset(visitorsSum, date == broadDate & time_min >= broadTime & time_min < broadTime + intervalSize)$visitsAppBel)
    
  
  if(i %% 100 == 0) {print(paste(i,Sys.time() - start))}
}
broadNet1 = broadNet1[order(broadNet1$gross_rating_point, decreasing=TRUE),]

## ========================================================
##                    First analysis
## ========================================================

par(mfrow=c(1,2))
plot(broadNet1$preVisitorsWebNet, broadNet1$postVisitorsWebNet, col = "blue")
lines(cbind(0,10000), cbind(0,10000))
plot(broadNet1$preVisitorsWebBel, broadNet1$postVisitorsWebBel, xlim = c(0,50), ylim = c(0,50), col = "red")
lines(cbind(0,10000), cbind(0,10000))
print(paste0("Num. WebNet post > pre: ", sum(broadNet1$postVisitorsWebNet>broadNet1$preVisitorsWebNet)))
print(paste0("Num. WebBel post > pre: ", sum(broadNet1$postVisitorsWebBel>broadNet1$preVisitorsWebBel)))

biggestAdsNet1 = subset(broadNet1, postVisitorsWebNet-preVisitorsWebNet > 10)

## ========================================================
##            REGRESSION MODELS 2-minute model
## ========================================================

# dummiesDirectModel contains the treatment variables
variablesDirectModelNet1 = c("product_category", "channel", "length_of_spot", "position_in_break_3option", "weekdays", "overlapBefore", "overlapAfter")
dummiesDirectModelPreNet1 = dummy_cols(.data = broadNet1, select_columns = variablesDirectModelNet1, remove_most_frequent_dummy = T)
dummiesDirectModelNet1 = dummiesDirectModelPreNet1[,((ncol(broadNet1)+1):ncol(dummiesDirectModelPreNet1))]
dummiesDirectModelNet1 = as.data.frame(dummiesDirectModelNet1)
rm(dummiesDirectModelPreNet1); rm(variablesDirectModelNet1)

# function for model summary
getModelSumm <- function(model, coef) {
  if(coef) {
    print(model)
    print(coeftest(model, vcov = vcovHC(model, type="HC1"))) # robust se
  }
  print(paste("R^2: ", summary(model)$r.squared))
  #hist(model$residuals, breaks = 50)
  print(paste("AIC: ",AIC(model)))
  print(paste("BIC: ", BIC(model)))
}

# Baseline models (is dit dan met of zonder belgie)
broadNet1['minusPreVisitorsWebBel'] = -1*broadNet1$preVisitorsWebBel
baselineModelNet1 = lm( (postVisitorsWebNet-postVisitorsWebBel) ~ preVisitorsWebNet +
                          minusPreVisitorsWebBel + factor(hours), data = broadNet1)
getModelSumm(baselineModelNet1, TRUE)

# Treatment effect only models ???
treatmentOnlyModelNet1 = lm((broadNet1$postVisitorsWebNet-broadNet1$postVisitorsWebBel) ~ ., data = dummiesDirectModelNet1)
getModelSumm(treatmentOnlyModelNet1, TRUE)

# Full model 
broadNet1['minusPreVisitorsWebBel'] = -1*broadNet1$preVisitorsWebBel
fullModelNet1 = lm( (broadNet1$postVisitorsWebNet-broadNet1$postVisitorsWebBel) ~ 
                      broadNet1$preVisitorsWebNet + broadNet1$minusPreVisitorsWebBel + 
                      factor(broadNet1$hours) + ., data = dummiesDirectModelNet1)
getModelSumm(fullModelNet1, T)
