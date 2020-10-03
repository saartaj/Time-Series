library(quantmod)
library(prophet)
library(ggplot2)

#Converting xts into data frame for prophet Facebook model.

m <- data.frame(ds=index(gsp),y=gsp$GS.Close)

colnames(m) <- c("ds","y") #ds and y only work for prophet

##########################################################################
##########################################################################
##########################################################################
##########################################################################


#It is a simple prophet model for analysis
mm <- prophet(m,daily.seasonality = F,weekly.seasonality = F)

#Making data frame for future prediction
future <- make_future_dataframe(mm,periods = 365)
tail(future)

prophet_forecasting <- predict(mm,future)
d_frame <- cbind(future,prophet_forecasting$yhat) 
colnames(d_frame) <- c("yr","prc")


fake <- data.frame(start=as.POSIXct("2020-09-09"),
                   end=as.POSIXct("2022-09-09"))


ggplot(d_frame, aes(x=yr,y=prc))+
  geom_rect(data=fake,aes(NULL,NULL,xmin=start,xmax=end,fill="gray"),
            ymin=120,ymax=260)+
  geom_line(col="black")+
  xlab("")+ylab("")+labs(title = "Goldman-Sachs Inc",
                        subtitle = "Price for time interval 2014 to 2020",
                        caption = "Source:Data extraction from Yahoo-Finance")+
  theme_classic()+
  theme(legend.position = "none")




