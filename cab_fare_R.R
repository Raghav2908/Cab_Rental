# Clening the environment
rm(list = ls(all = T))
# Setting working directory
setwd("/Users/raghavkotwal/Documents/Data Science/Cab Fare")
getwd()

#Load Libraries
x = c(
  "ggplot2",
  "corrgram",
  "DMwR",
  "caret",
  "randomForest",
  "unbalanced",
  "C50",
  "dummies",
  "e1071",
  "Information",
  "MASS",
  "rpart",
  "gbm",
  "ROSE",
  'sampling',
  'DataCombine',
  'inTrees'
)

#install.packages(x)
lapply(x, require, character.only = TRUE)

## Reading the data
Train = read.csv('train_cab.csv')
Test = read.csv('test.csv')

########################Exploratory Data Analysis#################################
# Structure of the data
str(Train)

#summary of the data set
summary(Train)

# Structure of the data
str(Test)

#summary of the data set
summary(Test)


####converting the fare_amount to to numerical ####
Train$fare_amount = as.character(Train$fare_amount)
Train$fare_amount = as.numeric(Train$fare_amount)


#######################Missing Values Analysis############################
sapply(Train, function(x) {
  sum(is.na(x))
})
missing_values = data.frame(sapply(Train, function(x) {
  sum(is.na(x))
}))

#Calculate missing percentage and arrage in order
missing_values$Var = row.names(missing_values)
row.names(missing_values) = NULL
names(missing_values)[1] = "Percentage"
missing_values$Percentage = ((missing_values$Percentage / nrow(Train)) *
                               100)
missing_values = missing_values[, c(2, 1)]
missing_values = missing_values[order(-missing_values$Percentage), ]

#Create missing value and impute using mean, median and knn
#Train[1,1]
#Train[1,1]=NA
#actual value = 4.5
##mean = 15.01
## median = 8.5


### Median Method for missing value Imputation ###
Train$fare_amount[is.na(Train$fare_amount)] = median(Train$fare_amount, na.rm = T)
Train$pickup_longitude[is.na(Train$pickup_longitude)] = median(Train$pickup_longitude, na.rm = T)
Train$pickup_latitude[is.na(Train$pickup_latitude)] = median(Train$pickup_latitude, na.rm = T)
Train$dropoff_longitude[is.na(Train$dropoff_longitude)] = median(Train$dropoff_longitude, na.rm = T)
Train$dropoff_latitude[is.na(Train$dropoff_latitude)] = median(Train$dropoff_latitude, na.rm = T)
Train$passenger_count[is.na(Train$passenger_count)] = median(Train$passenger_count, na.rm = T)

#Check if any missing values
sum(is.na(Train))


############### Outlier Analysis ######################

# Boxplot for continuous variables

boxplot(Train$fare_amount, xlab = "Fare", ylab = "count")
boxplot(Train$pickup_longitude, xlab = "Pickup_Longitude", ylab = "count")
boxplot(Train$pickup_latitude, xlab = "Pickup_Latitude", ylab = "count")
boxplot(Train$dropoff_latitude, xlab = "Dropoff_Latitude", ylab = "count")
boxplot(Train$dropoff_longitude, xlab = "Dropoff_Longitude", ylab = "count")
boxplot(Train$passenger_count, xlab = "Passenger_Count", ylab = "count")


##### Pickup time
#converting factor type pickup time to a character first
Train$pickup_datetime = as.character(Train$pickup_datetime)
#converting into datetime
Train$pickup_datetime <-
  as.POSIXct(Train$pickup_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "")

Train$Hours <-
  as.numeric(format(as.POSIXct(
    strptime(Train$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%H"))
Train$Minutes <-
  as.numeric(format(as.POSIXct(
    strptime(Train$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%M"))
Train$Date <-
  as.numeric(format(as.POSIXct(
    strptime(Train$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%d"))
Train$Month <-
  as.numeric(format(as.POSIXct(
    strptime(Train$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%m"))
Train$Year <-
  as.numeric(format(as.POSIXct(
    strptime(Train$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%Y"))

Train = na.omit(Train)

#converting Test data into datetime

Test$pickup_datetime <-
  as.POSIXct(Test$pickup_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "")

Test$Hours <-
  as.numeric(format(as.POSIXct(
    strptime(Test$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%H"))
Test$Minutes <-
  as.numeric(format(as.POSIXct(
    strptime(Test$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%M"))
Test$Date <-
  as.numeric(format(as.POSIXct(
    strptime(Test$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%d"))
Test$Month <-
  as.numeric(format(as.POSIXct(
    strptime(Test$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%m"))
Test$Year <-
  as.numeric(format(as.POSIXct(
    strptime(Test$pickup_datetime, "%Y-%m-%d %H:%M:%S", tz = "")
  ) , format = "%Y"))

Test = na.omit(Test)


#### Passenger Count should be less than 8 and greater than 0. Also remove 0.12 in train data

Train = subset(Train, passenger_count < 8 & passenger_count > 0)
Train = subset(Train, passenger_count != 0.12)

Test = subset(Test, passenger_count < 8 & passenger_count > 0)

#### Fare Amount should be greater than 0.

Train = subset(Train, fare_amount > 0)

#### Pickup Longitude and latitude

install.packages('geosphere')
library(geosphere)

Train = subset(Train, (pickup_longitude < 180 &
                         pickup_longitude > -180))
Train = subset(Train, (pickup_latitude < 90 &
                         pickup_latitude > -90))
Train = subset(Train, (dropoff_latitude < 90 &
                         dropoff_latitude > -90))
Train = subset(Train, (dropoff_longitude < 180 &
                         dropoff_longitude > -180))

Test = subset(Test, (pickup_longitude < 180 &
                       pickup_longitude > -180))
Test = subset(Test, (pickup_latitude < 90 & pickup_latitude > -90))
Test = subset(Test, (dropoff_latitude < 90 &
                       dropoff_latitude > -90))
Test = subset(Test, (dropoff_longitude < 180 &
                       dropoff_longitude > -180))


#Using Haversine Function of Geosphere package
Train$Distance <-
  distHaversine(
    cbind(Train$pickup_longitude, Train$pickup_latitude),
    cbind(Train$dropoff_longitude, Train$dropoff_latitude)
  )
Test$Distance  <-
  distHaversine(
    cbind(Test$pickup_longitude, Test$pickup_latitude),
    cbind(Test$dropoff_longitude, Test$dropoff_latitude)
  )

#Hence removing these values (0.00000,0.00000)

Train = subset(Train,
               (pickup_longitude != 0.00000 & pickup_longitude != 0.00000))
Train = subset(Train,
               (dropoff_longitude != 0.00000 & dropoff_longitude != 0.00000))

Train[order(-Train$Distance), ]

Test = subset(Test, (pickup_longitude != 0.00000 |
                       pickup_longitude != 0.00000))
Test = subset(Test,
              (dropoff_longitude != 0.00000 | dropoff_longitude != 0.00000))


Test[order(-Test$Distance), ]

Train = subset(Train, Distance < 4452067)



############### Visualizations ##############
df_columns = colnames(Train)

df_columns = c(
  'fare_amount',
  'passenger_count',
  'Hours',
  'Minutes',
  'Date',
  'Month',
  'Year',
  'Distance'
)

for (i in df_columns) {
  hist(
    Train[, i],
    col = "Blue",
    prob = TRUE,
    main = "Histogram",
    xlab = i,
    ylab = "Count"
  )
  lines(density(Train[, i]), lwd = 2)
}


############ Normalising data ##############


for (i in df_columns)
{
  print(i)
  Train[, i] = (Train[, i] - min(Train[, i])) / (max(Train[, i]) - min(Train[, i]))
}


##################### Model Development #########################


#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = sample(1:nrow(Train), 0.8 * nrow(Train))
train = Train[train.index, ]
test  = Train[-train.index, ]

train = train[, -2]
test = test[, -2]

##Decision tree for classification
#Develop Model on training data

library(rpart)
df_dtree = rpart(fare_amount ~ ., data = train, method = "anova")

dev.new()
plot(df_dtree)
text(df_dtree)
summary(df_dtree)
printcp(df_dtree)


#write rules into disk
write(capture.output(summary(df_dtree)), "Rules.txt")

#Lets predict for test data
pred_test_DT = predict(df_dtree, test[, -1])

#Lets predict for train data
pred_train_DT = predict(df_dtree, train[, -1])


install.packages("caret")
library(caret)


# For training data
print(postResample(pred = pred_train_DT, obs = train[, 1]))
#     RMSE  Rsquared       MAE
#6.779453e-04 1.627864e-01 8.867387e-05

# For testing data
print(postResample(pred = pred_test_DT, obs = test[, 1]))
#     RMSE  Rsquared       MAE
#1.786968e-02 4.505198e-07 4.064647e-04

##############Linear Regression
install.packages("caret")
library(caret)

#check multicollearity
install.packages("usdm")
library(usdm)
vif(Train[, -1:-2])

vifcor(Train[, -1:-2], th = 0.9)

#run regression model
LR_model = lm(fare_amount ~ ., data = train)

#Summary of the model
summary(LR_model)


#Lets predict for test data
pred_test_LR = predict(LR_model, test[, -1])

#Lets predict for train data
pred_train_LR = predict(LR_model, train[, -1])

#Predict
predictions_LR = predict(LR_model, test[, 2:12])

# For training data
print(postResample(pred = pred_train_LR, obs = train[, 1]))
#RMSE     Rsquared          MAE
#7.305286e-04 2.787657e-02 7.222486e-05

# For testing data
print(postResample(pred = pred_test_LR, obs = test[, 1]))
#RMSE     Rsquared          MAE
#0.0178638895 0.0003458237 0.0003830841



#------------------RANDOM FOREST------------------

install.packages("randomForest")
library(randomForest)
#RUN RANDOM FOREST
RF_model = randomForest(fare_amount ~ ., train, importance = TRUE, ntree = 300)

#Summary of the model
summary(RF_model)


#Lets predict for test data
pred_test_RF = predict(RF_model, test[, -1])

#Lets predict for train data
pred_train_RF = predict(RF_model, train[, -1])

#Predict
predictions_RF = predict(RF_model, test[, 2:12])

# For training data
print(postResample(pred = pred_train_RF, obs = train[, 1]))
#         RMSE  Rsquared       MAE
#3.368938e-04 9.494762e-01 2.335838e-05

# For testing data
print(postResample(pred = pred_test_RF, obs = test[, 1]))
#     RMSE  Rsquared       MAE
#0178634671 0.0002464168 0.0003647339






#---------------------------------------Prdicting using Random forest--------------

predictions_RF = predict(RF_model, Test[, 2:12])
