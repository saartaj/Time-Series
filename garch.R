
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(dplyr)


############################################################
############################################################
############################################################

#Extracting data from Yahoo-Finance from year 2014 to 2020 for Goldman-Sachs Inc

getSymbols("GS",from="2014-01-01",to="2020-09-09")

#Visual glimpse for the Goldman-Sachs
chartSeries(GS,subset = "last 2 months",theme = chartTheme(),
            TA = c(addVo(),addBBands()))


#Calculating return for the closing price
ret <- CalculateReturns(GS$GS.Close)
ret <- ret[-1]

#histogram for returns with density curve on it.
#tails of the returns are skewed
chart.Histogram(ret,methods = c("add.density"),
                colorset = c("blue","green"))

#Price returns graph, lower and upper bound.
#Extraordinary spikes can be seen for covid-19 situation.
chartSeries(ret,theme = "white",TA = c(addBBands()))



#  Volatility Annually
sd(ret) #standard deviation
sqrt(252)*sd(ret["2019"]) #annual valatility for Goldman-Sachs 2019.

#Rolling volatility for year 2019.
chart.RollingPerformance(R = ret["2016::2019"],
                         width = 22,
                         FUN = "sd.annualized",
                         scale=252,
                         main = "GS yearly rolling volatility")


# Model 1. standard GARCH with constant mean and Normality

s <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                variance.model = list(model="sGARCH"),#standard garch model
                distribution.model = "norm") #normally distributed return

m <- ugarchfit(spec = s,data = ret)                
plot(m,frame.plot = F,which="all") #diagonostic charts


f <- ugarchforecast(fitORspec=m, n.ahead=100)#predict


plot(fitted(f)) #since constant mean
plot(sigma(f)) #variability


v <- sqrt(252)*sigma(m)
w <- 0.05/v

plot(merge(v,w),multi.panel=T)


# Model 2. standard GARCH with constant mean and student-t
s <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
           variance.model = list(model="sGARCH"),
           distribution.model = "sstd")

m <- ugarchfit(spec=s,data = ret)
plot(m,which="all")

f <- ugarchforecast(fitORspec = m,n.ahead = 100)

plot(fitted(f))
plot(sigma(f))

v <- sqrt(252)*sigma(m)
w <- 0.1/v

plot(merge(v,w),multi.panel=T)


# Model 3. GJR-GARCH with asymmetry and student-t
s <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                variance.model = list(model="gjrGARCH"),
                distribution.model = "sstd")
m <- ugarchfit(spec = s,data = ret)
plot(m,which="all")

p <- ugarchforecast(data = return["/2021-12"],
                  fitORspec = m,n.ahead = 120)
plot(fitted(p))
plot(sigma(p))

v <- sqrt(252)*sigma(m)   #volatility
w <- 0.05/v               #weight assigned to them

plot(merge(v,w),multi.panel=T)
 
##These models are capable of capturing symmetric
##and asymmetric dynamics such as leptokurtosis, volatility clustering, 
##and leverage effects of the return series.


## Best model according to the analysis
s <- ugarchspec(mean.model = list(armaOrder=c(1,2)),
           variance.model = list("gjrGARCH"),
           distribution.model = "sstd"
           )
m <- ugarchfit(spec=s,data = ret)
plot(m,which='all')

ugarchforecast(m,n.ahead = 100) %>% plot()

