install_keras()

library(keras)
library(tensorflow)

## Making appropiate data for network
# calculate the minute in the year of the commercial
# Input: latent vistors

visitNet = visitorsSum[, c("date", "time_min", "visitsWebNet")]

visitNet = visitNet[order(visitNet$date, visitNet$time),]
dayCom = yday(visitNet$date)
dayTime = visitNet$time_min
visitNet$minute_in_year = ((dayCom-1)*1440)+dayTime+1

sampleSplit = sample.split(visitNet$date, SplitRatio = 0.8)
broadTrain = visitNet[sampleSplit == TRUE,]
broadTest = visitNet[sampleSplit == FALSE,]


model <- keras_model_sequential()
model %>%
  layer_dense(units = 3, activation = 'relu', input_shape = 2)%>%
  layer_dense(units = 1, activation = 'linear')


model %>% compile(
  optimizer = "adam",
  loss = 'mean_squared_error'
)

history <- model %>% fit(cbind(dplyr::lag(visitNet$visitsWebNet, 1), visitNet$time_min ), visitNet$visitsWebNet, validation_split=0.2)
