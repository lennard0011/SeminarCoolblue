#install_keras()

library(keras)
library(tensorflow)

## Making appropiate data for network
# calculate the minute in the year of the commercial
# Input: latent vistors
visitNet = visitorsSum[, c("date", "time_min", "visitsWebNet")]
broadNet = subset(broad, country == "Netherlands")
broadNet = broadNet[broadNet$time_min < (1440 - 5) & broadNet$time_min > 5,]

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
#visitNet['visitsLag1Min'] = dplyr::lag(visitNet$visitsWebNet, 1)
#visitNet['visitsLag1Hour'] = dplyr::lag(visitNet$visitsWebNet, 60)
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
channeldummys = channeldummys[, colSums(channeldummys != 0) > 0]
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

#crosseffects with GRP
for (i in names(visitNetDummBroad)) {
  if (i != "visitsWebNet" & i != "visitsWebNet"){
    visitNetDummBroad = cbind(visitNetDummBroad, visitNetDummBroad[i] *  visitNetDummBroad$gross_rating_point)
  }
}


# Make train and testset. Testset is last month.
scaledvisitNetDummBroad = scale(visitNetDummBroad)
center = attr(scaledvisitNetDummBroad, 'scaled:center')
scale = attr(scaledvisitNetDummBroad, 'scaled:scale')

visitTrain = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year <= 172800,]
visitVal = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year > 172800 & visitNetDummBroad$minute_in_year <= 217440,]
visitTest = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year > 217440,]

visitTrain = na.omit(visitTrain)
visitVal = na.omit(visitVal)
visitTest = na.omit(visitTest)

# Neural network
makeModel = function (regParam, colAmount) {
  model <- keras_model_sequential() 
  model %>%
    layer_dense(units = 30, activation = 'relu', input_shape = colAmount, kernel_regularizer = regularizer_l2(l = regParam)) %>%
    layer_dense(units = 30, activation = 'relu', kernel_regularizer = regularizer_l2(l = regParam)) %>%
    layer_dense(units = 30, activation = 'relu', kernel_regularizer = regularizer_l2(l = regParam)) %>%
    layer_dense(units = 1, activation = 'linear')
  #summary(model)
  model %>% compile(
    optimizer = 'adam',
    loss = 'mean_squared_error'
  )
  return(model)
}

xTrain = as.matrix(subset(visitTrain, select = -c(visitsWebNet)))
yTrain = as.matrix(subset(visitTrain, select = c(visitsWebNet)))

xVal = as.matrix(subset(visitVal, select = -c(visitsWebNet)))
yVal = as.matrix(subset(visitVal, select = c(visitsWebNet)))

xTest = as.matrix(subset(visitTest, select = -c(visitsWebNet)))
yTest = as.matrix(subset(visitTest, select = c(visitsWebNet)))

valData = list(xVal, yVal)

seqParam = c(0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001)
#logarithmic scale
results = c()
for (regParam in seqParam) {
  modelVal = makeModel(regParam, (ncol(xTrain)))
  history <- modelVal %>% fit(
    xTrain, 
    yTrain, 
    epochs = 20,
    validation_data = valData,
    verbose = getOption("keras.fit_verbose", default = 2)
  )
  predNN = predict(modelVal, xVal)
  print(paste("Regularization param: ", regParam))
  print(paste("Prediction error: ", mean((yVal - predNN)^2)))
  results = append(results, mean((yVal - predNN)^2))
}

plot(seqParam, results)
# get parameter value with lowest error
optParam = seqParam[which(results == min(results))] #equals 0.1
optParam = 0.1

#train new network on train and validation data to compare with OLS
xTrainVal = rbind(xTrain, xVal)
yTrainVal = rbind(yTrain, yVal)

testData = list(xTest, yTest)

modelOpt = makeModel(optParam, (ncol(xTrainVal)))
history <- modelOpt %>% fit(
  xTrainVal, 
  yTrainVal, 
  epochs = 20,
  validation_data = testData
)

predNN = predict(modelOpt, xTest)
mean((yTest - predNN)^2) #mse
mean(abs(yTest - predNN)) #mae
mean(abs(yTest - predNN)/yTest) #mape
# = 0.135807

# Linear model to test
linearModel = lm(visitsWebNet ~ ., data = as.data.frame(rbind(visitTrain, visitVal)))
sm = summary(linearModel)
predOLS = predict(linearModel, as.data.frame(visitTest))
mean((yTest - predOLS)^2) #mse
mean(abs(yTest - predOLS)) #mae
mean(abs(yTest - predOLS)/yTest) #mape

# Sensitivity analysis
xWhole = rbind(xTrainVal, xTest)
yWhole = rbind(yTrainVal, yTest)

modelWholeOpt = makeModel(optParam, (ncol(xTrainVal)))
history <- modelWholeOpt %>% fit(
  xWhole, 
  yWhole, 
  epochs = 20
)

plot(history)

predNNWhole = predict(modelWholeOpt, xWhole)
hist((yWhole - predNNWhole))

sensAn = function(model, xTrain, depVar, center, scale) {
  stepSize = 1000
  xMean = t(as.matrix(colMeans(xTrain)))
  xMeanMatrix = xMean[rep(seq_len(nrow(xMean)), each = stepSize), ]
  maxValue = max(xTrain[,depVar])
  minValue = min(xTrain[,depVar])
  varTries = seq(from = minValue, to = maxValue, length.out = stepSize)
  xMeanMatrix[,depVar] = varTries
  predictions = predict(model, xMeanMatrix)
  visitors = scale["visitsWebNet"]*predictions + center["visitsWebNet"]
  plot(5*(varTries*scale[depVar] + center[depVar]), visitors, type = "l", xlab = depVar)
  lowest = visitors[1]
  highest = visitors[stepSize]
  
  return(highest - lowest)
}

length(colnames(xWhole))



xWhole[, 62:122] = xWhole[, 62:122] / mean(xWhole[,"gross_rating_point"])

sensAn = function(model, xTrain, depVar, center, scale) {
  stepSize = 1000
  xMean = t(as.matrix(colMeans(xTrain)))
  xMeanMatrix = xMean[rep(seq_len(nrow(xMean)), each = stepSize), ]
  maxValue = max(xTrain[,depVar])
  minValue = min(xTrain[,depVar])
  varTries = seq(from = minValue, to = maxValue, length.out = stepSize)
  xMeanMatrix[,depVar] = varTries
  xMeanMatrix[, 62:122] = xMeanMatrix[, 62:122] * varTries
  predictions = predict(model, xMeanMatrix)
  visitors = scale["visitsWebNet"]*predictions + center["visitsWebNet"]
  plot(5*(varTries*scale[depVar] + center[depVar]), visitors, type = "l", xlab = depVar)
  lowest = visitors[1]
  highest = visitors[stepSize]
  
  return(highest - lowest)
}

sensAn(modelWholeOpt, xWhole, "gross_rating_point", center, scale)
sensAn(modelWholeOpt, xWhole, "broadAmount", center, scale)
sensAn(modelWholeOpt, xWhole, "visitsLag1Week", center, scale)
#channeleffect
varNames = colnames(xTrain)
channelNames = varNames[34:61]
sensAn(modelWholeOpt, xTrain, channelNames[1], center, scale)
channelResults = c()
for (channelName in colnames) {
  channelResults = append(channelResults, sensAn(modelWholeOpt, xTrain,  channelName, center, scale))
}
channelResultsNames = cbind(channelNames, channelResults)
View(channelResultsNames)
