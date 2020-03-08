#Analyzing the stock price of Amazon from 2014-2019 and using basic forecasting methods to try and predict the future price
library(fpp2)
library(fpp)
library(forecast)
#Dataset from kaggle.com

Amzn<-ts(AMZNtrain$Open)
autoplot(Amzn) # just to see how the data looks like
#Looking at the basic plot, +ve trend and there is a slight amnt of seasonality (although cant say for sure) hence, term it as cyclic data instead of seasonality 
#to check and plot seasonality:
ggseasonplot(Amzn) # data not seasonal
#to check for autocorr
ggAcf(Amzn)
#There does seem to be a good amount of autocorr
#trying to check the accuracy of this prediction
amznt<-ts(AMZNtest$Close)
autoplot(amznt)
#Trying some basic forecasting techniques:
autoplot(Amzn,main="predicting the closing price of amzn stock for the next 21 days",ylab="Amzn cl price")+
  autolayer(naive(Amzn,h=21),series="naive",PI=FALSE)+
  autolayer(meanf(Amzn,h=21),series="mean",PI=FALSE)+
  autolayer(rwf(Amzn,h=21,drift = TRUE),series="drift",PI=FALSE)+
  autolayer(rwf(amznt,h=2021),series="actual",PI=FALSE)+
  guides(colour=guide_legend(title="Forecast "))
#mean is useless to try and predict stock prices. Snaive in this case is useless as no seasonality. Use naive method.

#residuals from this series via naive method i.e. try and recognize the 4 conditions of a good model:
res<-residuals(rwf(Amzn))
autoplot(res) #to check for the var
#the spikes at the end make this forecast dubious and not that reliable
gghistogram(res) #although the histogram represents almost a normal dist -  a good sign
ggAcf(res) #mixed signs, can't determine if no autocorr or autocorr

#the forecast does not seem to satisfy the two key conditions for a good model: 1. have residuals that are uncorr 2. mean close to 0 
#not a good forecast although the prediction intervals might be good enough (so enable PI=TRUE to get atelast a good interval to predict stock value)

