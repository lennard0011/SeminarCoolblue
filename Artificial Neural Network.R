# Seminar Coolblue 2021 -- Artificial Neural Network
# @author: Lennard van der Plas, Erik van der Heide, Marjolein de With, Daniel Buijs

#install_keras()
library(keras)
library(tensorflow)

## ===================================================================
##              Collect data and merge to full time series
## ===================================================================
# Begin with making a dataset for the whole time series with the amount of visitors and broadcast specific attributes


# Making appropriate data for network
# Calculate the minute in the year of the commercial
# Input: latent visitors
visitNet = visitorsSum[, c("date", "time_min", "visitsWebNet")]
broadNet = subset(broad, country == "Netherlands")
broadNet = broadNet[broadNet$time_min < (1440 - 5) & broadNet$time_min > 5, ]

# Add minute_in_year to visitor data and broadcast data
visitNet = visitNet[order(visitNet$date, visitNet$time), ]
dayCom = yday(visitNet$date)
dayTime = visitNet$time_min
visitNet$minute_in_year = ((dayCom - 1) * 1440) + dayTime + 1
dayCom = yday(broadNet$date)
dayTime = broadNet$time_min
broadNet$minute_in_year = ((dayCom - 1) * 1440) + dayTime + 1
broadNet$date = NULL

# Time specific feature adding
visitNet['visitsLag1Week'] = dplyr::lag(visitNet$visitsWebNet, 60 * 24 * 7)
visitNetDumm = cbind(visitNet, dummy_cols(weekdays(as.Date(visitNet$date)), remove_most_frequent_dummy = T, remove_selected_columns = T))
visitNetDumm = cbind(visitNetDumm, dummy_cols(floor(visitNet$time_min / 60), remove_most_frequent_dummy = T, remove_selected_columns = T))

# Adding broadcast specific data
broadInterval = 5
# Aggregate grp and broadAmount over minute_in_year
broadGRP = aggregate(gross_rating_point ~ minute_in_year, data = broadNet, FUN = sum, simplify = T, drop = T)
broadAmount = as.data.frame(table(broadNet$minute_in_year))
names(broadAmount) = cbind("minute_in_year", "broadAmount")

# Make channel dummies
channeldummys = dummy_cols(broadNet$channel, remove_most_frequent_dummy = T, remove_selected_columns = T)
channeldummys = channeldummys[, colSums(channeldummys != 0) > 0]
# Aggregate channels on minute_in_year
channeldummys2 = aggregate(. ~ broadNet$minute_in_year, data = channeldummys, FUN = sum, simplify = T, drop = T)
# Name change of minute_in_year
channeldummys2['minute_in_year'] = channeldummys2['broadNet$minute_in_year']; channeldummys2['broadNet$minute_in_year'] = NULL 
channeldummys3 = merge(visitNet['minute_in_year'], channeldummys2, all = T)
channeldummys3[is.na(channeldummys3)] = 0
channeldummys3$minute_in_year = NULL
channeldummys4 = rollsum(channeldummys3, k = broadInterval, fill = NA, align = "right")
channeldummys5 = cbind(visitNet['minute_in_year'], channeldummys4)


# Merge grp and broadAmount with visitNetDumm 
visitNetDummBroad = merge(merge(broadGRP, broadAmount, all = T), visitNetDumm, all = T)
# Set missing grp and broadamount to zero
visitNetDummBroad$broadAmount[is.na(visitNetDummBroad$broadAmount)] = 0
visitNetDummBroad$gross_rating_point[is.na(visitNetDummBroad$gross_rating_point)] = 0
# Make 20 minute average which is right alligned
visitNetDummBroad$broadAmount = rollmean(visitNetDummBroad$broadAmount, k = broadInterval, fill = NA, align = "right")
visitNetDummBroad$gross_rating_point = rollmean(visitNetDummBroad$gross_rating_point, broadInterval, fill = NA, align = "right")

visitNetDummBroad = merge(visitNetDummBroad, channeldummys5, all = T)

visitNetDummBroad = na.omit(visitNetDummBroad)
visitNetDummBroad['time_min'] = NULL; visitNetDummBroad['date'] = NULL

rm(broadGRP)
rm(broadAmount)

## ===================================================================
##              Divide data set in Train and Test
## ===================================================================
# Make train and test set. Test set is last month.
scaledvisitNetDummBroad = scale(visitNetDummBroad)
center = attr(scaledvisitNetDummBroad, 'scaled:center')
scale = attr(scaledvisitNetDummBroad, 'scaled:scale')

visitTrain = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year <= 172800, ]
visitVal = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year > 172800 & visitNetDummBroad$minute_in_year <= 217440, ]
visitTest = scaledvisitNetDummBroad[visitNetDummBroad$minute_in_year > 217440, ]

visitTrain = na.omit(visitTrain)
visitVal = na.omit(visitVal)
visitTest = na.omit(visitTest)

## ===================================================================
##                          Make Neural Network
## ===================================================================
# Function to make a neural network for certain regularization parameter and input size
makeModel = function (regParam, colAmount) {
  model = keras_model_sequential() 
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

# Put data in matrix
xTrain = as.matrix(subset(visitTrain, select = -c(visitsWebNet)))
yTrain = as.matrix(subset(visitTrain, select = c(visitsWebNet)))

xVal = as.matrix(subset(visitVal, select = -c(visitsWebNet)))
yVal = as.matrix(subset(visitVal, select = c(visitsWebNet)))

xTest = as.matrix(subset(visitTest, select = -c(visitsWebNet)))
yTest = as.matrix(subset(visitTest, select = c(visitsWebNet)))

valData = list(xVal, yVal)

## ===================================================================
##                    Find best regularisation parameter
## ===================================================================
# Logarithmic scale for regularization interval
seqParam = c(0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001)
results = c()
for (regParam in seqParam) {
  modelVal = makeModel(regParam, (ncol(xTrain)))
  history = modelVal %>% fit(
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
# Get parameter value with lowest error
optParam = seqParam[which(results == min(results))] # Equals 0.1
optParam = 0.1

## ===================================================================
##            Compare Neural Network with OLS
## ===================================================================
xTrainVal = rbind(xTrain, xVal)
yTrainVal = rbind(yTrain, yVal)

testData = list(xTest, yTest)

modelOpt = makeModel(optParam, (ncol(xTrainVal)))
history = modelOpt %>% fit(
  xTrainVal, 
  yTrainVal, 
  epochs = 20,
  validation_data = testData
)

predNN = predict(modelOpt, xTest)
mean((yTest - predNN)^2) #MSE
mean(abs(yTest - predNN)) #MAE
mean(abs(yTest - predNN)/yTest) #MAPE

# Linear model to test
linearModel = lm(visitsWebNet ~ ., data = as.data.frame(rbind(visitTrain, visitVal)))
sm = summary(linearModel)
predOLS = predict(linearModel, as.data.frame(visitTest))
mean((yTest - predOLS)^2) #MSE
mean(abs(yTest - predOLS)) #MAE
mean(abs(yTest - predOLS)/yTest) #MAPE

## ===================================================================
##              Variable effects analysis
## ===================================================================
xWhole = rbind(xTrainVal, xTest)
yWhole = rbind(yTrainVal, yTest)

modelWholeOpt = makeModel(optParam, (ncol(xTrainVal)))
history = modelWholeOpt %>% fit(
  xWhole, 
  yWhole, 
  epochs = 20
)

plot(history)

predNNWhole = predict(modelWholeOpt, xWhole)
hist((yWhole - predNNWhole))

# Function to determine the effects of a certain variable on the amount of visitors
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
  plot(5 * (varTries*scale[depVar] + center[depVar]), visitors, type = "l", xlab = depVar)
  lowest = visitors[1]
  highest = visitors[stepSize]
  
  return(highest - lowest)
}

# Effects of GRP, broadAmount and lagged visits
sensAn(modelWholeOpt, xWhole, "gross_rating_point", center, scale)
sensAn(modelWholeOpt, xWhole, "broadAmount", center, scale)
sensAn(modelWholeOpt, xWhole, "visitsLag1Week", center, scale)
# Channel effect
varNames = colnames(xTrain)
channelNames = varNames[34:61]
sensAn(modelWholeOpt, xTrain, channelNames[1], center, scale)
channelResults = c()
for (channelName in colnames) {
  channelResults = append(channelResults, sensAn(modelWholeOpt, xTrain,  channelName, center, scale))
}
channelResultsNames = cbind(channelNames, channelResults)
View(channelResultsNames)