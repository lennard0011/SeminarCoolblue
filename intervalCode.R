#change to intervals
library(chron)
library(caTools)

traffic <- read.csv(file.choose(), header = T)
broad <- read.csv(file.choose(), header = T)

nBroad <- nrow(broad)
nTraffic <- nrow(traffic)

mean(broad$postVisitors[1:500] - broad$preVisitors[1:500])
data = cbind(broad$postVisitors[1:500], broad$preVisitors[1:500])
#data = cbind(log(broad$postVisitors[1:500]), log(broad$preVisitors[1:500]))
data <- as.data.frame(data)
colnames(data) <- c("postVisitors", "preVisitors")

data_split = sample.split(data$postVisitors, SplitRatio = 0.8)
train <- subset(data, data_split == TRUE)
test <-subset(data, data_split == FALSE)
plot(data$preVisitors, data$postVisitors)
lines(cbind(0,10000), cbind(0,10000))

model <- lm(postVisitors[1:100] ~ 0 + preVisitors[1:100], data = broad) #DataFlair
summary(model)
coefficients(model)
hist(broad$postVisitors[1:500])
hist(broad$preVisitors[1:500])
