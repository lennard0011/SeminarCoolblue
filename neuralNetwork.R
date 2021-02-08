install_keras()

library(keras)
library(tensorflow)

## Making appropiate data for network
# calculate the minute in the year of the commercial
# Input: latent vistors
visitNet = visitorsSum[, c("date", "time_min", "visitsWebNet")]

# Add minute_in_year to visitor data and broadcast data
visitNet = visitNet[order(visitNet$date, visitNet$time),]
dayCom = yday(visitNet$date)
dayTime = visitNet$time_min
visitNet$minute_in_year = ((dayCom-1)*1440)+dayTime+1
dayCom = yday(broadNet$date)
dayTime = broadNet$time_min
broadNet$minute_in_year = ((dayCom-1)*1440)+dayTime+1


# time specific feature adding
visitNet['visitsLag1Min'] = dplyr::lag(visitNet$visitsWebNet, 1)
visitNet['visitsLag1Hour'] = dplyr::lag(visitNet$visitsWebNet, 60)
visitNet['visitsLag1Week'] = dplyr::lag(visitNet$visitsWebNet, 60*24*7)
visitNetDumm = cbind(visitNet, dummy_cols(weekdays(as.Date(visitNet$date)), remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE))
#visitsNetDumm = cbind(visitsNetDumm, dummy_cols(months(as.Date(visitNet$date)), remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE))
visitNetDumm = cbind(visitNetDumm, dummy_cols(floor(visitNet$time_min / 60), remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE))

#adding broadcast specific data
broadInterval = 20
visitNetDumm['broadAmount'] = 0
for (i in broadInterval:nrow(broadNet)) {
  visitTime = broadNet$minute_in_year[i] 
  visitNetDumm$broadAmount[(visitTime-broadInterval):visitTime] = visitNetDumm$broadAmount[(visitTime-broadInterval):visitTime] + 1 
}

sapply(visitNetDumm$minute_in_year, )

broadCastCounter = 


visitNet['broadAmount'] = 
visitNet['broadGPR'] = 

#Take out rows with missing values.
visitNetDumm = na.omit(visitNetDumm)

# Make train and testset. Testset is last month.
sampleSplit = sample.split(visitNet$date, SplitRatio = 0.8)
visitTrain = visitNet[visitNet$minute_in_year <= 217440,]
visitTest = visitNet[visitNet$minute_in_year > 217440,]

# Quick prediction error for randomwalk estimator. 
randomWalkEstimate = mean(visitTrain$visitsWebNet)
mean((visitTest$visitsWebNet - randomWalkEstimate)^2)

# Neural network
model <- keras_model_sequential()
model %>%
  layer_dense(units = 3, activation = 'relu', input_shape = ncol(visitsNetDumm))%>%
  layer_dense(units = 1, activation = 'linear')
model %>% compile(
  optimizer = "adam",
  loss = 'mean_squared_error'
)

xTrain = cbind(dplyr::lag(visitTrain$visitsWebNet, 1), visitTrain$time_min)
yTrain = visitTrain$visitsWebNet

xVal = 
yVal = 

history <- model %>% fit(x, y, validation_split = 0.2, epochs = 4)
