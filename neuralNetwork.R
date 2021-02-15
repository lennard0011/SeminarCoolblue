#install_keras()

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
broadNet$date = NULL

# time specific feature adding
visitNet['visitsLag1Min'] = dplyr::lag(visitNet$visitsWebNet, 1)
visitNet['visitsLag1Hour'] = dplyr::lag(visitNet$visitsWebNet, 60)
visitNet['visitsLag1Week'] = dplyr::lag(visitNet$visitsWebNet, 60*24*7)
visitNetDumm = cbind(visitNet, dummy_cols(weekdays(as.Date(visitNet$date)), remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE))
visitNetDumm = cbind(visitNetDumm, dummy_cols(floor(visitNet$time_min / 60), remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE))

#adding broadcast specific data
broadInterval = 5
#aggregate grp and broadAmount over minute_in_year
broadGRP = aggregate(gross_rating_point ~ minute_in_year, data = broadNet, FUN=sum, simplify = TRUE, drop = TRUE)
broadAmount = as.data.frame(table(broadNet$minute_in_year))
names(broadAmount) = cbind("minute_in_year", "broadAmount")

#make channel dummies
channeldummys = dummy_cols(broadNet$channel, remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE)
#aggregate channels on minute_in_year
channeldummys2 = aggregate(. ~ broadNet$minute_in_year, data = channeldummys, FUN = sum, simplify = TRUE, drop = TRUE)
#namechange of minute_in_year
channeldummys2['minute_in_year'] = channeldummys2['broadNet$minute_in_year']; channeldummys2['broadNet$minute_in_year'] = NULL 
channeldummys3 = merge(visitNet['minute_in_year'], channeldummys2, all = TRUE)
channeldummys3[is.na(channeldummys3)] = 0
channeldummys3$minute_in_year = NULL
channeldummys4 = rollsum(channeldummys3, k = broadInterval, fill = NA, align = "right")
channeldummys5 = cbind(visitNet['minute_in_year'], channeldummys4)


#merge grp and broadAmount with visitNetDumm 
visitNetDummBroad = merge(merge(broadGRP, broadAmount, all = TRUE), visitNetDumm, all = TRUE)
#set missing grp and broadamount to zero
visitNetDummBroad$broadAmount[is.na(visitNetDummBroad$broadAmount)] = 0
visitNetDummBroad$gross_rating_point[is.na(visitNetDummBroad$gross_rating_point)] = 0
#make 20 minute average which is right alligned
visitNetDummBroad$broadAmount = rollmean(visitNetDummBroad$broadAmount, k = broadInterval, fill = NA, align = "right")
visitNetDummBroad$gross_rating_point = rollmean(visitNetDummBroad$gross_rating_point, broadInterval, fill = NA, align = "right")

visitNetDummBroad = merge(visitNetDummBroad, channeldummys5, all=TRUE)

visitNetDummBroad = na.omit(visitNetDummBroad)
visitNetDummBroad['time_min'] = NULL; visitNetDummBroad['date'] = NULL

names(visitNetDummBroad)

rm(broadGRP)
rm(broadAmount)

# Make train and testset. Testset is last month.
visitNetDummBroad = visitNetDumm
scaledvisitNetDummBroad = scale(visitNetDummBroad)

visitTrain = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year <= 217440,]
visitVal = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year > 217440,]

# Neural network
model <- keras_model_sequential() 
model %>%
  layer_dense(units = 1, activation = 'linear', input_shape = c(ncol(visitTrain)-1))%>%
  #layer_dense(units = 20, activation = 'relu', input_shape = c(63))%>%
  #layer_dropout(rate = 0.5) %>%
  #layer_dense(units = 20, activation = 'relu')%>%
  #layer_dropout(rate = 0.5) %>%
  #layer_dense(units = 1, activation = 'linear')

summary(model)

model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error'
)


xTrain = as.matrix(subset(visitTrain, select = -c(visitsWebNet)))
yTrain = as.matrix(subset(visitTrain, select = c(visitsWebNet)))

xVal = as.matrix(subset(visitVal, select = -c(visitsWebNet)))
yVal = as.matrix(subset(visitVal, select = c(visitsWebNet)))

history <- model %>% fit(
  xTrain, 
  yTrain, 
  epochs = 8
)

plot(history)

predictionsxVal = predict(model, xVal)
mean((yVal - predictionsxVal)^2)
plot(yVal, type = "l")
lines(predictionsxVal)


# Quick prediction error for randomwalk estimator. The neural network should be able to beat this... or there would be overfitting
randomWalkEstimate = mean(visitTrain$visitsWebNet)
mean((yVal - randomWalkEstimate)^2) 

linearModel = lm(yTrain ~ as.data.frame(xTrain))
summary(linearModel)
predictionsxVal = predict(linearModel, as.data.frame(xVal))
mean((yVal - predictionsxVal)^2)
plot(yVal, type = "l")
lines(predictionsxVal)

