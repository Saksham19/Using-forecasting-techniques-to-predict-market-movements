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
  xlab("day")+ylab("")+
  guides(colour=guide_legend(title="Forecast "))
#mean is useless to try and predict stock prices. Snaive in this case is useless as no seasonality. Use naive method.

#residuals from this series via naive method i.e. try and recognize the 4 conditions of a good model:
res<-residuals(rwf(Amzn))
autoplot(res) #to check for the var
#the spikes at the end make this forecast dubious and not that reliable
gghistogram(res) #although the histogram represents almost a normal dist -  a good sign
ggAcf(res) #mixed signs, can't determine if no autocorr or autocorr
Box.test(res,lag=21,fitdf = 0)
Box.test(res,lag=21,fitdf = 0,type = "Lj") # the p value is very small, null rejected. therefore, there is autocorr that is not whitenoise
checkresiduals(res)
#the forecast does not seem to satisfy the two key conditions for a good model: 1. have residuals that are uncorr 2. mean close to 0 
#not a good forecast although the prediction intervals might be good enough (so enable PI=TRUE to get atelast a good interval to predict stock value)


#checking the accuracy of the forecast
amzn<-window(Amzn,end=1237)
autoplot(Amzn)+
  autolayer(naive(amzn,h=21),series="naive",PI=FALSE)+
  autolayer(meanf(amzn,h=21),series="mean",PI=FALSE)+
  autolayer(rwf(amzn,h=21,drift = TRUE),series="drift",PI=FALSE)+
  xlab("day")+ylab("")+
  guides(colour=guide_legend(title="Forecast "))
Amznt<-window(Amzn,start=1238)
Amzn1<-naive(amzn,h=21)
Amzn2<-meanf(amzn,h=21)
Amzn3<-rwf(amzn,h=21,drift = TRUE)
a1<-accuracy(Amzn1,Amznt)
a2<-accuracy(Amzn2,Amznt)
a3<-accuracy(Amzn3,Amznt)
rbind(a1[2,c(2,3,5,6)],a2[2,c(2,3,5,6)],a3[2,c(2,3,5,6)])
#naive is the best method out of the three for forecasting amazon daily stock price (as shown by the graph and the min RMSE,MAE,MAPE,MASE)

