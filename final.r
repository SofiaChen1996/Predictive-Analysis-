library(fpp3)
library(urca)
library(forecast)
#read the Taiwanese export custom basis data
twexport <- read.csv("d:/PA/final/twexport2003.csv")

#check the format of the data
head(twexport)

#change the data into time series 
twexport <-twexport[1:244,]
twexport<-data.frame(Date = as.Date(twexport$Date), Exports = twexport$Exports)
class(twexport$Exports)
head(twexport)

twexport<- mutate(twexport, Date = yearmonth(Date)) 
twexport_ts <-as_tsibble(twexport, index= Date) 

#check the data format 
head(twexport_ts)
class(twexport_ts)

#plot the time series 
autoplot(twexport_ts, Exports)+
  labs (title =  "Taiwanese Exports on Custom Basis", subtitle = " From 2003 to 2023", caption = "Data Resource: National Statistics Department, Taiwan (2023)")+
  xlab ('Month')+
  ylab('Exports (hundred million US dollars)')

#check the seasonality 
twexport_ts%>% gg_season(Exports)+
  labs(title= 'Seasonal Plot: Taiwanese Exports on Custom Basis (2003-2023)', xlab="Month", ylab='Exports (hundred million US dollars')
twexport_ts%>% gg_subseries(Exports)+
  labs(title= 'Seasonal Subseries Plot: Taiwanese Exports on Custom Basis (2003-2023)', xlab="Month", ylab='Exports (hundred million US dollars')

#check the auto correlation 
twexport_ts %>% ACF(Exports, lag_max = 48) %>% autoplot()+labs(title = "Taiwanese Exports on Custom Basis")

#data transformation -box cox transformation 
twexport_ts %>% features(Exports, features= guerrero )
bc_twexports <- twexport_ts
bc_twexports$Exports <-box_cox(twexport_ts$Exports, 0.762)
par(mfrow=c(2,1))
autoplot(bc_twexports)+
  labs (title =  "Box-Cox transformed Monthly Taiwanese Exports on Custom Basis (lambda=0.762)", subtitle = " From 2003 to 2023 ")+
  xlab ('Month')+
  ylab('Box-Cox Transformed Exports')
bc_twexports %>% ACF(Exports, lag_max = 48) %>% autoplot()+labs(title = "ACF: Box-Cox transformed Taiwanese Exports on Custom Basis")

#STL decomposition 
twexport_de <- twexport_ts%>% model(seat = X_13ARIMA_SEATS(Exports))
components((twexport_de))
components(twexport_de)%>% autoplot()+
  labs (title =  "SEATS Decomposition: Monthly Taiwanese Exports on Custom Basis")
#STL decomposition 2
twexport_de2 <- bc_twexports%>% model(seat = X_13ARIMA_SEATS(Exports))
components((twexport_de2))
components(twexport_de2)%>% autoplot()+
  labs (title =  "SEATS Decomposition: Box-Cox transformed Monthly Taiwanese Exports on Custom Basis (lambda=0.762)")
#stationary check - ADF
summary(ur.df(bc_twexports$Exports, type="trend", selectlags= "AIC", lags =24))
summary(ur.df(bc_twexports$Exports, type="drift", selectlags= "AIC", lags =24))
#stationary check - KPSS
summary (ur.kpss(bc_twexports$Exports, type = "tau"))
summary (ur.kpss(bc_twexports$Exports, type = "mu"))

#seasonal difference
bc_twexports<-bc_twexports%>% mutate(sdexport= Exports-lag(Exports, 12))
bc_twexports%>%autoplot(sdexport)
#check the kpss result 
summary(ur.kpss(bc_twexports$sdexport, type="mu"))
#check the adf result
summary(ur.df(bc_twexports%>%select(sdexport)%>%filter_index("2004-01"~.)%>%as.ts(), type = "none", selectlag="AIC", lags=24 ))

#split the training and test set 
trainset <- bc_twexports%>%filter_index("2003-01"~"2018-12")
testset <- bc_twexports%>% filter_index("2019-01"~.)
 #fit the ets model
etsmodel <-  trainset %>% model( guess= ETS(Exports~ error("A")+trend("A")+season("M")),
                                auto= ETS(Exports))
report(etsmodel%>% select(auto))
report(etsmodel%>%select(guess))
etsmodel%>%select(guess)%>% gg_tsresiduals(type="innovation")
etsmodel%>%select(auto)%>% gg_tsresiduals(type="innovation")
etsmodel %>%
  residuals() %>%
  features(.resid, features = ljung_box, lag =10)
#Model identification-ARIMA model 
#ACF and PACF plot
trainset%>%ACF(sdexport, lag_max = 48)%>%autoplot()+labs (title =  "")
trainset%>%PACF(sdexport, lag_max = 48)%>%autoplot()
#fit the guessed model and autoselected model
sarimamodel <- trainset %>% model(guess = ARIMA(Exports ~ pdq(3,0,0) + PDQ(3,1,0)), 
                                  auto = ARIMA(Exports, ic ="aic"))
#residuals check-auto selected model
sarimamodel %>%
  select(auto) %>%
  gg_tsresiduals(type = "innovation")
report(sarimamodel%>%select(auto))
#residuals check- guessed model 
sarimamodel %>%
  select(guess) %>%
  gg_tsresiduals(type = "innovation")
report(sarimamodel%>%select(guess))

#autocorrelation test 
sarimamodel %>%
  residuals() %>%
  features(.resid, features = ljung_box, lag=12, dof = 7)

#normality test
shapiro.test(sarimamodel %>%
               select(auto)%>%
               residuals() %>%
               select(.resid) %>%
               as.ts())
shapiro.test(sarimamodel %>%
               select(guess)%>%
               residuals() %>%
               select(.resid) %>%
               as.ts())
# define the final model 
trainset1<-trainset[,-3]
trainset1<-as.ts(trainset1)
trainset1
finalets<- ets(trainset1, model="ZZZ")
checkresiduals(finalets)
finalsarima<- Arima(trainset1, order = c(1,1,0), seasonal = list(order=c(2,0,0),period=12) )
checkresiduals(finalsarima)
#predict the data -SeasonalARIMA
fc1<-forecast::forecast(finalsarima, h=52)
#Inverse the BOX COX transformation
fc1$mean<-InvBoxCox(fc1$mean, 0.762)
fc1$lower<-InvBoxCox(fc1$lower, 0.762)
fc1$upper<-InvBoxCox(fc1$upper, 0.762)
fc1$x<-InvBoxCox(fc1$x, 0.762)
fc2<-forecast::forecast(finalets, h=52)
fc2$mean<-InvBoxCox(fc2$mean, 0.762)
fc2$lower<-InvBoxCox(fc2$lower, 0.762)
fc2$upper<-InvBoxCox(fc2$upper, 0.762)
fc2$x<-InvBoxCox(fc2$x, 0.762)
testset<-InvBoxCox(testset$Exports, 0.762)

twexport_test <- ts(twexport_test$Exports, start = c(2019, 1), frequency = 12)
twexport_ts2<-ts(twexport_ts$Exports, start = c(2003,1), end=c(2023,4), frequency = 12)
autoplot(twexport_ts2, series = "Actual", size=0.75) +
  autolayer(fc1, series = "Seasonal ARIMA", alpha=0.5, size=0.9) +
  autolayer(fc2, series = "ETS", alpha=0.3, size=0.75) +
  labs(title = "Seasonal ARIMA and ETS Model Forecast ", y = "Exports (hundred million US dollars)", x = "Year") +
  scale_color_manual(values = c("black", "red", "blue"),
                     labels = c("Actual", "ETS", "Seasonal ARIMA")) +
  guides(color = guide_legend(title = "Models"))
forecast::accuracy(fc1, twexport_test)
forecast::accuracy(fc2, twexport_test)

#fit the model with the whole data and do the 1 year prediction 
sarima_pr<-Arima(bc_twexports, order = c(1,1,0), seasonal = list(order=c(2,0,0),period=12) )
checkresiduals(sarima_pr)
fc3<-forecast::forecast(sarima_pr, h=12)
fc3$mean<-InvBoxCox(fc3$mean, 0.762)
fc3$lower<-InvBoxCox(fc3$lower, 0.762)
fc3$upper<-InvBoxCox(fc3$upper, 0.762)
fc3$x<-InvBoxCox(fc3$x, 0.762)
autoplot(fc3)+  
  labs (title =  "Taiwanese Monthly Exports One Year Forecasting", subtitle = "Seasonal ARIMA(1,1,0)(2,0,0)[12] ")+
  xlab ('Month')+
  ylab('Exports (hundred million US dollars)')
  