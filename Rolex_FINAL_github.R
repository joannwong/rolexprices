rolex <- read.csv("Rolex_Overall_Price_Index_5Y.csv")
View(rolex)      

library(tidyverse)
library(lubridate)
library(fpp2) 
 
# clean data 
rolex <- rolex %>%
  mutate(time = mdy(rolex$Date))       

rolex <- rolex %>% 
  mutate(month_year = format(rolex$time,"%Y-%m"))

rolex <- rolex %>%
  group_by(month_year) %>%
  summarize(avg_price = mean(as.integer(Price)))

# convert to ts  
rolexts <- ts(rolex$avg_price, start = c(2017,2) , end = c(2022,2), deltat = 1/12 ) 
autoplot(rolexts)  

# ts decomposition        
autoplot(mstl(rolexts)) 

# stationarity    
nsdiffs(rolexts) # 0          
ndiffs(rolexts) # 2          

rolexts_stat <- diff(diff(rolexts)) # length = 59 
  
# -------------------------------------------------------------------------------------------------  
# ARIMA  
arima_rolex <- stlf(rolexts, method = "arima")  
autoplot(arima_rolex) 
checkresiduals(arima_rolex)    

# VAR ---------------------------------------------------------------------------------------------     
library(lmtest)
library(vars)     

# Predictor Variables: 
    
# Steel -------------------------------------------------------------------------------------------
steel <- read.csv("steel_index.csv")
steel_ts <- ts(steel$WPU10170502, start = c(2017,2) , end = c(2022,2), deltat = 1/12)
autoplot(steel_ts)  

grangertest(steel_ts, rolexts) # p = 0.1407
grangertest(rolexts, steel_ts) # p = 0.00107 

ndiffs(steel_ts) # 1  
 
steel_diff <- diff(steel_ts) # length = 60 
steel_diff <- steel_diff[2:60] 
 
steel_rolex_ts <- cbind(steel_diff, rolexts_stat)     
VARselect(steel_rolex_ts) # 1    
  
var1_steel_rolex <- VAR(steel_rolex_ts, p =1, type = "const") 
serial.test(var1_steel_rolex) # p = 0.7259   
   
# Gold -------------------------------------------------------------------------------------------
gold <- read.csv("gold_price.csv") 

gold <- gold %>%
  mutate(time = mdy(gold$Date))   

gold <- gold %>%  
  mutate(month_year = format(gold$time,"%Y-%m")) 

gold <- gold %>%
  group_by(month_year) %>%
  summarize(avg_price = mean(as.integer(Price))) 

gold_ts <- ts(gold$avg_price, start = c(2017,2) , end = c(2022,2), deltat = 1/12)
autoplot(gold_ts)

grangertest(gold_ts, rolexts) # p = 0.0437  
grangertest(rolexts, gold_ts) # p = 0.8183 

# since it gold granger-causes rolex, we will try building the VAR model 
 
ndiffs(gold_ts) # 1  

gold_diff <- diff(gold_ts) # length = 60 
gold_diff <- gold_diff[2:60]
 
gold_rolex_ts <- cbind(gold_diff, rolexts_stat)  
VARselect(gold_rolex_ts) # 1 
 
var1_gold_rolex <- VAR(gold_rolex_ts, p =1, type = "const")
serial.test(var1_gold_rolex) # p = 0.5138                  

 
# Google Trends ----------------------------------------------------------------------------------
rolexsentiment <- read.csv("Rolex_Google_Trends.csv") 
rolexsentiment_ts <- ts(rolexsentiment$Rolex, start = c(2017,2) , end = c(2022,2), deltat = 1/12)
autoplot(rolexsentiment_ts) 
   
grangertest(rolexsentiment_ts, rolexts) # p = 0.1299
grangertest(rolexts, rolexsentiment_ts) # p 0.312 
 
ndiffs(rolexsentiment_ts) # 0  

rolexsentiment_rolex_ts <- cbind(rolexsentiment_ts[3:61], rolexts_stat) 
VARselect(rolexsentiment_rolex_ts) # 1

var1_rolexsentiment <- VAR(rolexsentiment_rolex_ts, p =1, type = "const")
serial.test(var1_rolexsentiment) # p = 0.4617    

# MSCI World --------------------------------------------------------------------------------------
msci_all <- read.csv("MSCI_all.csv") 
msci_all_ts <- ts(msci_all$ACWI.Standard..Large.Mid.Cap., start = c(2017,2) , end = c(2022,2), deltat = 1/12 ) 
autoplot(msci_all_ts)

grangertest(msci_all_ts, rolexts) # p = 0.1698
grangertest(rolexts, msci_all_ts) # p = 0.02387

ndiffs(msci_all_ts) # 1
msci_all_diff <- diff(msci_all_ts) # length = 60
msci_all_diff <- msci_all_diff[2:60] 
 
msci_rolex_ts <- cbind(msci_all_diff, rolexts_stat) 
VARselect(msci_rolex_ts) # 1 

var1_msci_rolex <- VAR(msci_rolex_ts, p =1, type = "const")
serial.test(var1_msci_rolex) # p = 0.873    

# ARIMA-X ---------------------------------------------------------------------------------------- 
# use time series regression to identify significant variables 

# use stationary variables
cbind_all_stat <- cbind(rolexts_stat, steel_diff, gold_diff, rolexsentiment_ts[3:61], msci_all_diff)   
tslm_rolex_stat <- tslm(formula = rolexts_stat ~ steel_diff + gold_diff + rolexsentiment_ts[3:61] + msci_all_diff, data = cbind_all_stat) 
summary(tslm_rolex_stat) # no significant vars!  
 
# use raw variables 
cbind_all_raw <- cbind(rolexts, steel_ts, gold_ts, rolexsentiment_ts, msci_all_ts)
tslm_rolex_raw <- tslm(formula = rolexts ~ steel_ts + gold_ts + rolexsentiment_ts + msci_all_ts, data = cbind_all_raw) 
summary(tslm_rolex_raw) # significant vars: steel_ts, msci_all_ts  
              
# build the ARIMA-X model   

rolexts_1 <- ts(rolexts, start = c(2017,4), end = c(2022,2), deltat= 1/12)

# xreg 
steel_ts_2 <- ts(steel_ts, start = c(2017,3), end = c(2022,2), deltat= 1/12)
steel_stat <- diff(steel_ts_2)
msci_ts_2 <-ts(msci_all_ts, start = c(2017,3), end = c(2022,2), deltat= 1/12)
msci_stat <- diff(msci_ts_2) 
xreg_2 <- cbind(steel_stat, msci_stat)   
 
# newxreg
steel_fc <- stlf(steel_ts, method = "arima", h = 12) 
msci_fc <- stlf(msci_all_ts, method = "arima", h = 12)  
cbind_raw_vars_fc <- cbind(steel_fc$mean, msci_fc$mean)  
   
# ARIMA-X model  
autoplot(stlf(rolexts_1, method = "arima", xreg = xreg_2, newxreg = cbind_raw_vars_fc, h = 12))   
arimax_rolex <- stlf(rolexts_1, method = "arima", xreg = xreg_2, newxreg = cbind_raw_vars_fc, h = 12)  
     
 
# Accuracy Testing -------------------------------------------------------------------------------

# in-sample testing
 
accuracy(arima_rolex)

accuracy(var1_steel_rolex$varresult$rolexts_stat)       
accuracy(var1_gold_rolex$varresult$rolexts_stat)
accuracy(var1_rolexsentiment$varresult$rolexts_stat)
accuracy(var1_msci_rolex$varresult$rolexts_stat)   
 
accuracy(arimax_rolex) # lowest RMSE                              
  
# out-of-sample testing for ARIMA, VAR(steel), VAR(gold), VAR(Google Trends), VAR(MSCI)   
library(TSstudio) 

# test set is last 12 months
out_of_sample = 12 
  
# split train and test data -- use stationary variables 
rolexts_train <- ts_split(rolexts, sample.out = out_of_sample) 
    
steel_rolex_train <- ts_split(steel_rolex_ts, sample.out = out_of_sample)
gold_rolex_train <- ts_split(gold_rolex_ts, sample.out = out_of_sample)
sentiment_rolex_train <- ts_split(rolexsentiment_rolex_ts, sample.out = out_of_sample)  
msci_rolex_train <- ts_split(msci_rolex_ts, sample.out = out_of_sample)
 
    
# use train set for models   
arima_rolex_train <- Arima(rolexts_train$train, order = c(0,2,2))

var_steel_train <- VAR(steel_rolex_train$train, p =1, type = "const")
var_gold_train <- VAR(gold_rolex_train$train, p =1, type = "const")
var_rolexsentiment_train <- VAR(sentiment_rolex_train$train, p =1, type = "const") 
var_msci_train <- VAR(msci_rolex_train$train, p =1, type = "const")
 
# forecasts  
arima_rolex_fc <- stlf(rolexts_train$train, method = "arima", h= out_of_sample) 
       
var_steel_fc <- forecast(var_steel_train, out_of_sample)           
var_gold_fc <- forecast(var_gold_train, out_of_sample)
var_rolexsentiment_fc <- forecast(var_rolexsentiment_train, out_of_sample)
var_msci_fc <- forecast(var_msci_train, out_of_sample)        
 
   
# back transform VAR forecasts      
h = 12   
   
# steel 
var_steel_fc_bt <- rolexts_train$train   

for (i in 1:h) {
  var_steel_fc_bt <- c(var_steel_fc_bt, var_steel_fc$forecast$rolexts_stat$mean[i] -
                         var_steel_fc_bt[length(var_steel_fc_bt)-1] +   
                         2*var_steel_fc_bt[length(var_steel_fc_bt)]) 
} 
 
var_steel_fc_bt <- tail(var_steel_fc_bt, 12)   
var_steel_fc_bt <- ts(var_steel_fc_bt, start = c(2021,3), deltat = 1/12) 

   
# gold 
var_gold_fc_bt <- rolexts_train$train     

for (i in 1:h) {
  var_gold_fc_bt <- c(var_gold_fc_bt, var_gold_fc$forecast$rolexts_stat$mean[i] -
                         var_gold_fc_bt[length(var_gold_fc_bt)-1] +   
                         2*var_gold_fc_bt[length(var_gold_fc_bt)])  
} 
 
var_gold_fc_bt <- tail(var_gold_fc_bt, 12)
var_gold_fc_bt <- ts(var_gold_fc_bt, start = c(2021,3), deltat = 1/12)  

# google trends/ sentiment 
var_rolexsentiment_fc_bt <- rolexts_train$train     

for (i in 1:h) {
  var_rolexsentiment_fc_bt <- c(var_rolexsentiment_fc_bt, var_rolexsentiment_fc$forecast$rolexts_stat$mean[i] -
                        var_rolexsentiment_fc_bt[length(var_rolexsentiment_fc_bt)-1] +   
                        2*var_rolexsentiment_fc_bt[length(var_rolexsentiment_fc_bt)])  
} 

var_rolexsentiment_fc_bt <- tail(var_rolexsentiment_fc_bt,12)
var_rolexsentiment_fc_bt <- ts(var_rolexsentiment_fc_bt, start = c(2021,3), deltat = 1/12)   

# msci 
var_msci_fc_bt <- rolexts_train$train   

for (i in 1:h) {
  var_msci_fc_bt <- c(var_msci_fc_bt, var_msci_fc$forecast$rolexts_stat$mean[i] -
                                  var_msci_fc_bt[length(var_msci_fc_bt)-1] +   
                                  2*var_msci_fc_bt[length(var_msci_fc_bt)])  
} 

var_msci_fc_bt <- tail(var_msci_fc_bt,12)    
var_msci_fc_bt <- ts(var_msci_fc_bt, start = c(2021,3), deltat = 1/12)     
     
         
# out-of-sample testing for ARIMA-X   
   
# xreg
steel_ts_outsample <- ts(steel_ts, start = c(2017,3), end = c(2021,2), deltat= 1/12)
steel_outsample_stat <- diff(steel_ts_outsample) 
msci_ts_outsample <-ts(msci_all_ts, start = c(2017,3), end = c(2021,2), deltat= 1/12)
msci_outsample_stat <- diff(msci_ts_outsample)
xreg <- cbind(steel_outsample_stat, msci_outsample_stat) 
   
# newxreg 
steel_train <- ts_split(steel_ts, sample.out = out_of_sample) 
steel_fc2 <- stlf(steel_train$train, method = "arima", h = 12)          

msci_train <- ts_split(msci_all_ts, sample.out = out_of_sample)
msci_fc2 <- stlf(msci_train$train, method = "arima", h = 12)      

cbind_raw_vars_fc2 <- cbind(steel_fc2$mean, msci_fc2$mean)  

rolexts_arimax <- ts(rolexts_train$train[2:length(rolexts_train$train)],start = c(2017,4), end=c(2021,2) , deltat = 1/12)  

arimax_fc <- stlf(rolexts_arimax, method = "arima", xreg = xreg, newxreg = cbind_raw_vars_fc2, h = 12)  

autoplot(arimax_fc)    
    
# --------------------------------------------------------------------------------------------- 
accuracy(rolexts_train$test, arima_rolex_fc$mean) # lowest RMSE 
 
accuracy(rolexts_train$test, var_steel_fc_bt)  
accuracy(rolexts_train$test, var_gold_fc_bt) 
accuracy(rolexts_train$test, var_rolexsentiment_fc_bt) 
accuracy(rolexts_train$test, var_msci_fc_bt)     
 
accuracy(rolexts_train$test, arimax_fc$mean)               
  
# Forecast Plot   
autoplot(window(rolexts, start = c(2017,2)), series ="Actual Price") + 
  autolayer(arima_rolex_fc, PI = FALSE, series = "ARIMA") + 
  autolayer(var_steel_fc_bt,series = "VAR w Steel") + 
  autolayer(var_gold_fc_bt, series = "VAR w Gold") + 
  autolayer(var_rolexsentiment_fc_bt, series = "VAR w Sentiment") + 
  autolayer(var_msci_fc_bt, series = "VAR w MSCI" ) +  
  autolayer(arimax_fc, PI = FALSE, series = "ARIMA-X")      

# ---------------------------------------------------------------------------------------------  
# Ex-Post Forecasting with ARIMA-X 
rolexts_2 <- ts(rolexts, start = c(2017,4), end = c(2021,2), deltat= 1/12)

# xreg 
steel_ts_3 <- ts(steel_ts, start = c(2017,3), end = c(2021,2), deltat= 1/12) 
steel_ts3_stat <- diff(steel_ts_3) 
msci_ts_3 <- ts(msci_all_ts, start = c(2017,3), end = c(2021,2), deltat= 1/12) 
msci_ts3_stat <- diff(msci_ts_3) 
xreg_3 <- cbind(steel_ts3_stat, msci_ts3_stat)  
   
# newxreg 
steel_ts_4 <- ts(steel_ts, start = c(2021,3), end = c(2022,2), deltat= 1/12)  
steel_ts4_stat <- diff(steel_ts_4)
msci_ts_4 <- ts(msci_all_ts, start = c(2021,3), end = c(2022,2), deltat= 1/12)   
msci_ts4_stat <- diff(msci_ts_4)
expost_newxreg <- cbind(steel_ts4_stat, msci_ts4_stat)      
 
# ARIMA-X model (ex-post)
autoplot(stlf(rolexts_2, method = "arima", xreg = xreg_3, newxreg = expost_newxreg, h = 11))  
arimax_expost <- stlf(rolexts_2, method = "arima", xreg = xreg_3, newxreg = expost_newxreg, h = 11) 

# ---------------------------------------------------------------------------------------------   
# out-of-sample testing for ARIMA-X (ex-post) 
  
# xreg
steel_ts_outsample2 <- ts(steel_ts, start = c(2017,3), end = c(2020,2), deltat= 1/12)
steel_outsample_stat2 <- diff(steel_ts_outsample2) 
msci_ts_outsample2 <-ts(msci_all_ts, start = c(2017,3), end = c(2020,2), deltat= 1/12)
msci_outsample_stat2 <- diff(msci_ts_outsample2)
xreg_expost <- cbind(steel_outsample_stat2, msci_outsample_stat2)   
 
# newxreg    
steel_train2 <- ts_split(steel_ts_3, sample.out = out_of_sample) 
steel_expost_fc <- stlf(steel_train2$train, method = "arima", h = 11)             

msci_train2 <- ts_split(msci_ts_3, sample.out = out_of_sample)
msci_expost_fc <- stlf(msci_train2$train, method = "arima", h = 11)      

newxreg_expost <- cbind(steel_expost_fc$mean, msci_expost_fc$mean)   

arimax_expost <- ts(rolexts_train$train[2:length(rolexts_train$train)],start = c(2017,4), end=c(2020,2) , deltat = 1/12)  

arimax_expost_fc <- stlf(arimax_expost, method = "arima", xreg = xreg_expost, newxreg = newxreg_expost, h = 11)   
 
autoplot(arimax_expost_fc) 
             
expost_rolex <- rolexts[38:48]
accuracy(expost_rolex, arimax_expost_fc$mean)     



  