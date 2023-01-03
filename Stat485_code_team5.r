# Load required R packages
library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library(gplots)
library("TSA")
library('lubridate')
library('Hmisc')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('tidyquant')
library('quantmod')
library('TSstudio')
library('tseries')
library('MASS')
library("forecast")
library('TSA')
library("stats")
library("tseries")

library("ggplot2")
library("dplyr")
library("autoplotly")
source("BCA_functions_source_file.R")
source("Stat485-685_Rcode.R")

## Read data into R
steam <- read.csv("chart.csv", header = TRUE)
variable.summary(steam)

####################### Data Cleaning ##########################################


# Remove unneeded variables.
steam$Flags <- NULL
#steam$In.Game <- NULL
variable.summary(steam)

# Rename ï..DateTime
colnames(steam)[1] <- 'DateTime'

# Format Date.Time object in steam2
steam$DateTime <- as.Date(steam$DateTime)

View(steam)

# Trim all records before 2021, store the trimmed data into steam2
steam2 <- steam[6199:nrow(steam),]
rownames(steam2) <- NULL
variable.summary(steam2)

View(steam2)

# Remove Users, use In.Game as our target. 
steam2$Users <- NULL


variable.summary(steam2)


################################################################################


# Try 2021.
steam3 <- steam2[1:365,]
tail(steam3)

plot(steam3)
pacf(steam3$In.Game)
acf(steam3$In.Game)
eacf(steam3$In.Game)


############################ Functions #########################################




############################ Modeling ##########################################

## Plotting
plot(In.Game ~ DateTime, main="Number of Daily In Game Steam Users over Time", data=steam2)

plot(steam2,ylab='In game user count',type='o')
plot(steam2,ylab='In game user count',type='l')
points(y=steam2$In.Game,x=steam2$DateTime)


######## Examine the series using Box-Jenkins Method ###########################

## PACF
pacf(steam2$In.Game)


## ACF
acf(steam2$In.Game)


## EACF
eacf(steam2$In.Game)

######## Neither MA(q), nor AR(p), nor ARIMA(p,1,q) ############################


## Log transform the data.

In.Game.log <- log(steam2$In.Game) - lag(log(steam2$In.Game))
steam2.log <- steam2
steam2.log$In.Game <- In.Game.log
glimpse(steam2.log)
variable.summary(steam2.log)

steam2.log[is.na(steam2.log$In.Game),]

steam2.log <- steam2.log[2:nrow(steam2.log),]
glimpse(steam2.log)
variable.summary(steam2.log)

plot(steam2.log,ylab='In game user count',type='l')

acf(steam2.log$In.Game)
pacf(steam2.log$In.Game)
eacf(steam2.log$In.Game)


######## Examine the series using Dynamics Method ##############################

plot(steam2, ylab=("Daily In Game User Count"), type = "o")

order_list = c(c(1,0,0), c(2,0,1), c(3,0,2), c(4,0,3))

for (i in seq(1, length(order_list), by = 3)) {
  my_order = c(order_list[i],order_list[i+1],order_list[i+2])

  print(arima(steam2$In.Game, order = my_order))
    
  }


arima(steam2$In.Game, order = c(2,0,1))



########## Seasonal Model #####################################################



plot(steam2$In.Game, type = 'l',ylab='First Difference of User Count',xlab='Time')

plot(diff(steam2$In.Game), type = 'l',ylab='First Difference of User Count',xlab='Time')

acf(as.vector(diff(steam2$In.Game)),lag.max=100)

plot(diff(diff(steam2$In.Game),lag=7),type = 'l',xlab='Time',
     ylab='First and Seasonal Difference of User Count')


m1.steam <- arima(steam2$In.Game,order=c(2,1,2),seasonal=list(order=c(0,1,1),period=7))
m1.steam

plot(window(rstandard(m1.steam)),ylab='Standardized Residuals m1',type='l')

acf(as.vector(window(rstandard(m1.steam))),
    lag.max=200,ci.type='ma')

hist(window(rstandard(m1.steam)),
     xlab='Standardized Residuals')


qqnorm(window(rstandard(m1.steam)))
qqline(window(rstandard(m1.steam)))

?forecast.Arima

forecast_data <- forecast(m1.steam, 180)
print(forecast_data)
plot(forecast_data, main = "forecasting_data for stream in game user") 






















