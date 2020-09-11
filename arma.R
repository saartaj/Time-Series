library(forecast)
library(quantmod)
library(dplyr)

getSymbols("GS",from="2014-01-01",to="2020-09-07")  ##loading data from yahoo finance


gsp <- GS$GS.Close   ##selecting closing price for Analysis

plot(gsp,main="GS Stock Price Fluctuation")  ## plotting data 

tseries::adf.test(GS$GS.Close)
#Comment: p-value>.05, we cannot reject null hypothesis that it has r=1, unit root problem.

dgsp <- diff(gsp) %>% na.omit() #diff function for lag differentiation


tseries::adf.test(dgsp) # we reject the null hypothesis that non-stationery.


plot(dgsp) #plot of differences in obs, 1 lag difference


plot(tail(dgsp,100)) #COVID, exceptional time period.
#giving it more thought to collect more information

acf(gsp) ##significant spikes are for MA, lags are so many.
##It might be because of error spikes.
pacf(gsp) #significant spikes are for AR lags

Box.test(gsp,type = "Ljung-Box") 

a <- arima(gsp,order = c(2,1,1))

aTSA::arch.test(a) #rejecting null hypothesis of homoskedastic


plot(forecast::forecast(arima(tail(gsp,140),order = c(2,1,1)),50))






