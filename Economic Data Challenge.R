### Shreya Ganeshan ###
#### Economic Data Challenge ###

### data used: Thumbtack Economic Sentiment Survey (ESS) ###

# NEXT STEP: TRY VAR MODELS INSTEAD

# -------------- Questions -------------- #
# How does monthly economic sentiment for the United States correlate 
## with the monthly unemployment rate (U3) for the United States?
############# ANSWER: a 1 unit change in ESS yields a 0.6529 unit reduction in unemployment rate

# Is our headline economic sentiment data more of a leading, lagging, or 
## coincident indicator of the monthly unemployment rate?
############# ANSWER: ESS is likely a leading indicator of monthly unemployment

# Which of the following monthly time series can our headline economic sentiment 
## data best predict: the unemployment rate, real personal consumption expenditures, 
## change in nonfarm payrolls, or consumer sentiment?
############# ANSWER: ESS best predicts unemployment rate

# How well can responses to our survey’s “any recent hires” question (data here) predict
## month-over-month changes in “Total Nonfarm Private Payroll Employment”?
############# ANSWER: ----------

# -------------- Preparing the data -------------- #
# didn't use all these packages to answer assignment questions
# installing "httr" package to retrieve JSON data from API 
install.packages("httr")
library(httr) # functions to send request to ESS API
library(jsonlite) # to convert json data type to R object
library(zoo)
install.packages("forecast") # USED
library(forecast)
install.packages("stats")
library(stats)
install.packages("tseries")
library(tseries)
install.packages("lmtest")
library(lmtest)
install.packages("vars")
library(vars)

# getting a response object
ss_url = modify_url(paste0("https://data.thumbtack.com/v1/sentiments/states"))
ss_response = GET(ss_url)

# checking the status of the response
# since the status message is "Success: (200) OK" we can proceed
http_status(ss_response)
warn_for_status(ss_response)
# stop_for_status(ss_response)

# convert JSON string to R obejct
ss_text = content(ss_response, "text")
ss_data = fromJSON(ss_text, flatten = TRUE)$data
head(ss_data)

# export ss_response as csv file for offline analysis
write.csv(ss_data, file = "ss_response.csv")

# saving ss_response data 
ss_data1 = read.csv("ss_response.csv", sep = ",", header = T)
head(ss_data)
attach(ss_data1)
detach(ss_data1)

# creating variable of interest (only start date and score)
ss_data2 = (ss_data1[c("start_date", "score")])
head(ss_data2)
# converting start_date to a real date
ss_data2$start_date = as.Date(ss_data2$start_date)
head(ss_data2$start_date)

# ss_data ordered by start_date
ss_data2 = ss_data2[order(ss_data2$start_date),]
head(ss_data2)

# subset of data from 2012-10-1 to 2015-1-1 that is quarterly
# ss_quarterly = subset(ss_data2, start_date < "2015-07-01")
# head(ss_quarterly)
# tail(ss_quarterly)
# creating a monthly sequence for the all data range
ss_monthly = seq(ss_data2$start_date[1], tail(ss_data2$start_date,1), by = "month")
ss_monthly
# cubic interpolation using spline()
ss_data_monthly = data.frame(start_date=ss_monthly, score=spline(ss_data2, method="fmm", xout=ss_monthly)$y)
head(ss_data_monthly)
tail(ss_data_monthly)
ss_data_monthly

write.csv(ss_data_monthly, file = "ss_data_monthly.csv")
# ss_monthlyend = subset(ss_data2, start_date > "2015-01-01")
# ss_monthlyend
# head(ss_monthlyend)
# tail(ss_monthlyend)

# -------------- Preparing the Unemployment data -------------- #

# loading unemployment data
unemp_data = read.csv("UNRATE.csv", sep = ",", header = T)
head(unemp_data)
tail(unemp_data)
attach(unemp_data)
detach(unemp_data)
# converting DATE variable to a real date
unemp_data$DATE = as.Date(DATE)
head(unemp_data$DATE)
# ordering unemp data bby date
unemp_data = unemp_data[order(unemp_data$DATE),]
head(unemp_data)
tail(unemp_data)

date = unemp_data$DATE
unrate = unemp_data$UNRATE
unrate

# -------------- Time Series Analysis -------------- #
# model: relationship between ESS score (X) and U.S. unemployment rate (Y)
# sample period: 2012-10-01 to 2017-02-01

# saving the datasets as time series
ss_data_monthly_ts = as.ts(ss_data_monthly, frequency = 12, start = c(2012, 10))
head(ss_data_monthly_ts)
# ss_data_monthly_ts = data.frame(ss_data_monthly_ts)
#score_ts = ss_data_monthly_ts$score
#head(score_ts)
is.ts(ss_data_monthly_ts)

unemp_data_ts = as.ts(unemp_data, frequency = 12, start = c(2012, 10))
head(unemp_data_ts)
# unemp_data_ts = data.frame(unemp_data_ts)
# unrate_ts
# head(unrate_ts)
is.ts(unemp_data_ts)

# creating new data set with only score_ts (x) and unrate_ts (y) variables
ts = data.frame(ss_data_monthly_ts[,2], unemp_data_ts[,2])
ts
score_ts = as.vector(ts[,2])
score_ts
unrate_ts = as.vector(ts[,3])
unrate_ts
attach(ts)

# (1) plot the time series
par(mfrow=c(2,1))
plot.ts(score_ts)
plot.ts(unrate_ts)
# it's clear that the score_ts may be stationary
# byt unrate_ts is definitely not stationary

# (2) check for unit roots and number of differences required to make the data stationary 
# use KPSS test (Kwiatkowski-Phillips-Schmidt-Shin (KPSS))
# KPSS null -> process is trend stationary
ndiffs(score_ts, alpha = 0.05, test = c("kpss"))
ndiffs(unrate_ts, alpha = 0.05, test = c("kpss"))
# only 1 difference required to get rid of unit root
# can also use ADF test (Augmented Dickey Fuller Test)
# ADF null -> process has a unit root
adf.test(score_ts, alternative = "stationary") # stationary
adf.test(unrate_ts, alternative = "stationary") #not all stationary

# (3) create a differenced time series 
dscore_ts = diff(score_ts)
head(dscore_ts)
dunrate_ts = diff(unrate_ts)
head(dunrate_ts)

dlscore_ts = diff(log(score_ts))
dlunrate_ts = diff(log(unrate_ts))

# (4) plot differenced time series
par(mfrow=c(2,1))
plot.ts(dscore_ts)
plot.ts(dunrate_ts)
# looks more or less stationary

# -------------- Regression Analysis -------------- #
# model: relationship between ESS score (X) and U.S. unemployment rate (Y)
# simple model: Yt = a0 + a1Yt-1 + B0Xt + B1Xt-1 + Ut 
# where Ut is NOT originally a white noise proces 
# Ut = a1Et-1 + a2Et-2 + ...+ Zt where Zt ~ N(0, sigma^2)
# use lag operator: PHI(L) = 1 - PHI1(L) - PHI2(L^2) - ... 
# PHI(L)Et = Ut 
# Ut = Et*PHI^-1(L)
# Yt = a0 + a1Yt-1 + B0Xt + B1Xt-1 + PHI^-1(L)Et and Et is white noise 
# added lags until errors are serially uncorrelated

# (5) find the optimal lag 
auto.arima(dunrate_ts, xreg = dscore_ts)
# optimal lags: ARIMA(2,0,2) on first differenced series

# (6) ARIMA model
?arima
model1 = arima(dunrate_ts, order = c(2,0,2), xreg = dscore_ts)
summary(model1)
model1_resid = residuals(model1)
# AIC = -54
acf(model1_resid)
pacf(model1_resid)

# (7) check for model stationarity using Ljung-Box test and ADF test
Box.test(model1_resid, lag = 10, type= "Ljung-Box") # fail to reject Ho = stationary
adf.test(model1_resid, alternative = "stationary") # reject Ho = stationary

# -------------- Indicators Check -------------- #
# need to determine whether one series is likely to influence the other
# take different lags of one series and use it to model change in other
# create two models that predict Yt: 1) with only Yt lags and 2) with Yt and Xt lags
# test to see which model is better at predicting Yt+h
# aka does Xt provide more inforamtion about Yt+h than Yt on it's own
# also need to make sure that Yt doesn't provide information about Xt+h
# to make this easier, you can just use Granger-Causality Test 

grangertest(dlscore_ts ~ dlunrate_ts, order = 4)
grangertest(dlunrate_ts ~ dlscore_ts, order = 4)
# PROBLEM: failed to rejected Ho for BOTH -> can't determine whether leading or lagging

# another method: 
# Xt+h to predict Yt+h
# -h = X leads Y
# +h = X lags Y
# PRE-WHITENING
# (1) determine time series model for  x-variable and store the residuals
# creates white noise series as input
# (2) filter y-variable series using x-variable model (with the estimated coefficients from (1).  
# find differences between observed y-values and “estimated” y-values based on the x-variable model
# y-series = linear combination of x-series --> “transform” the x-series to white noise 
## (using residuals from ARIMA model), then can apply the transformation to both sides of the equation
# (3) look at CCF between  residuals (1) and  filtered y-values from (2)
# pre-whitening just helps identify which lags of x may predict y
# after identifying model from CCF, use original variables to estimate  lagged regression.
# CCF used to identify possible terms for lagged regression

# sample CCF
ccf(dscore_ts, dunrate_ts)
ccfvalues = ccf(dscore_ts, dunrate_ts)
ccfvalues # maximum (absolute value) values at h = -2 lags and (7 lags???)
# negative correlations = an above average score_ts = below average unrate_ts
# displays lag and correlation between Xt and Yt
# THUS: Xt (ESS) likely leads Yt (unemployment)

# all autocorrelations seem to fall within significance bounds and taper off!
acf(dscore_ts)
pacf(dscore_ts)
acf(dunrate_ts)
pacf(dunrate_ts)

###########################

# -------------- Prediction Using Other Series -------------- #
# other series: unemployment rate, real personal consumption expenditures, 
## change in nonfarm payrolls, or consumer sentiment

## 2: Regression with real personal conusmption expenditures
# loading pcec data
pcec_data = read.csv("PCEC96.csv", sep = ",", header = T)
head(pcec_data)
attach(pcec_data)
detach(pcec_data)
# converting DATE variable to a real date
pcec_data$DATE = as.Date(pcec_data$DATE)
head(pcec_data$DATE)
# ordering pcec data by date
pcec_data = pcec_data[order(pcec_data$DATE),]
head(pcec_data)

pcec = pcec_data$PCEC96
pcec
#saving pcec data as time series
pcec_data_ts = as.ts(pcec_data, frequency = 12, start = c(2012, 10))
head(pcec_data_ts)
is.ts(pcec_data_ts)

# creating new data set with only score_ts (x) and pcec_ts (y) variables
ss_data_monthly_ts_trunc = ss_data_monthly_ts[c(1:52),]
tail(ss_data_monthly_ts_trunc)
ts_pred_pecec = data.frame(ss_data_monthly_ts_trunc[,2], pcec_data_ts[,2])
ts_pred_pecec
score_ts_pcec = as.vector(ts_pred_pecec[,1])
score_ts_pcec
pcec_ts_pcec = as.vector(ts_pred_pecec[,2])
pcec_ts_pcec
attach(ts_pred_pecec)

# plot the time series
par(mfrow=c(2,1))
plot.ts(score_ts_pcec)
plot.ts(pcec_ts_pcec)
# it's clear that the score_ts may be stationary
# byt pcec_ts_pcec is definitely not stationary

# check for unit roots and number of differences required to make the data stationary 
# use KPSS test (Kwiatkowski-Phillips-Schmidt-Shin (KPSS))
ndiffs(score_ts_pcec, alpha = 0.05, test = c("kpss"))
ndiffs(pcec_ts_pcec, alpha = 0.05, test = c("kpss"))
# only 1 difference required 
# can also use ADF test (Augmented Dickey Fuller Test)

# create a differenced time series 
dscore_ts_pcec = diff(score_ts_pcec)
head(dscore_ts_pcec)
dpcec_ts_pcec = diff(pcec_ts_pcec)
head(dpcec_ts_pcec)

# plot differenced time series
par(mfrow=c(2,1))
plot.ts(dscore_ts_pcec)
plot.ts(dpcec_ts_pcec)
# looks more or less stationary

# find the optimal lag 
auto.arima(dpcec_ts_pcec, xreg = dscore_ts_pcec)
# optimal lags: ARIMA(1,0,0) on first differenced series

model2 = arima(dpcec_ts_pcec, order = c(1,0,0), xreg = dscore_ts_pcec)
summary(model2)
model2_resid = residuals(model1)
# AIC = 471.22
acf(model2_resid)
pacf(model2_resid)

# check for model stationarity using Ljung-Box test and ADF test
Box.test(model2_resid, lag = 10, type= "Ljung-Box") # fail to reject Ho = stationary
adf.test(model2_resid, alternative = "stationary") # reject Ho = stationary

###########################

## 3: Regression with change in nonfarm payrolls
# loading pcec data
payems_data = read.csv("PAYEMS.csv", sep = ",", header = T)
head(payems_data)
attach(payems_data)
detach(payems_data)
# converting DATE variable to a real date
payems_data$DATE = as.Date(payems_data$DATE)
head(payems_data$DATE)
# ordering payems data by date
payems_data = payems_data[order(payems_data$DATE),]
head(payems_data)

payems = payems_data$PAYEMS
payems
#saving payems data as time series
payems_data_ts = as.ts(payems_data, frequency = 12, start = c(2012, 10))
head(payems_data_ts)
tail(payems_data_ts)
is.ts(payems_data_ts)

# creating new data set with only score_ts (x) and payems_ts (y) variables
ts_pred_payems = data.frame(ss_data_monthly_ts[,2], payems_data_ts[,2])
ts_pred_payems
score_ts_payems = as.vector(ts_pred_payems[,1])
score_ts_payems
payems_ts_payems = as.vector(ts_pred_payems[,2])
payems_ts_payems
detach(ts_pred_payems)

# plot the time series
par(mfrow=c(2,1))
plot.ts(score_ts_payems)
plot.ts(log(payems_ts_payems))
# it's clear that the score_ts may be stationary
# but log(ayems_ts_payems) is definitely not stationary

# check for unit roots and number of differences required to make the data stationary 
# use KPSS test (Kwiatkowski-Phillips-Schmidt-Shin (KPSS))
ndiffs(score_ts_payems, alpha = 0.05, test = c("kpss"))
ndiffs(log(payems_ts_payems), alpha = 0.05, test = c("kpss"))
# only 1 difference required 
# can also use ADF test (Augmented Dickey Fuller Test)

# create a differenced time series 
dscore_ts_payems = diff(score_ts_payems)
head(dscore_ts_payems)
dlpayems_ts_payems = diff(log(payems_ts_payems))
head(dlpayems_ts_payems)

# plot differenced time series
par(mfrow=c(2,1))
plot.ts(dscore_ts_payems)
plot.ts(dlpayems_ts_payems)
# looks more or less stationary

# find the optimal lag 
auto.arima(dlpayems_ts_payems, xreg = dscore_ts_payems)
# optimal lags: ARIMA(0,0,0) on first differenced series

model3 = arima(dlpayems_ts_payems, order = c(0,0,0), xreg = dscore_ts_payems)
summary(model3)
model3_resid = residuals(model3)
# AIC: -637.89
acf(model3_resid)
pacf(model3_resid)

# check for model stationarity using Ljung-Box test and ADF test
Box.test(model3_resid, lag = 10, type= "Ljung-Box") # fail to reject Ho = stationary
adf.test(model3_resid, alternative = "stationary") # reject Ho = stationary

###########################

## 4: Regression with change in consumer sentiment
# loading consumer sentiment
umcsent_data = read.csv("UMCSENT.csv", sep = ",", header = T)
head(umcsent_data)

# converting DATE variable to a real date
umcsent_data$DATE = as.Date(umcsent_data$DATE)
head(umcsent_data$DATE)
# ordering umcsent data by date
umcsent_data = umcsent_data[order(umcsent_data$DATE),]
head(umcsent_data)

umcsent = umcsent_data$UMCSENT
umcsent

#saving umcsent data as time series
umcsent_data_ts = as.ts(umcsent_data, frequency = 12, start = c(2012, 10))
head(umcsent_data_ts)
tail(umcsent_data_ts)
is.ts(umcsent_data_ts)

# creating new data set with only score_ts (x) and umcsent_ts (y) variables
ts_pred_umcsent = data.frame(ss_data_monthly_ts[,2], umcsent_data_ts[,2])
ts_pred_umcsent
score_ts_umcsent = as.vector(ts_pred_umcsent[,1])
score_ts_umcsent
umcsent_ts_umcsent = as.vector(ts_pred_umcsent[,2])
umcsent_ts_umcsent

# plot the time series
par(mfrow=c(2,1))
plot.ts(score_ts_umcsent)
plot.ts(umcsent_ts_umcsent)
# it's clear that the score_ts may be stationary
# but umcsent_ts_umcsent is definitely not stationary

# check for unit roots and number of differences required to make the data stationary 
# use KPSS test (Kwiatkowski-Phillips-Schmidt-Shin (KPSS))
ndiffs(score_ts_umcsent, alpha = 0.05, test = c("kpss"))
ndiffs(umcsent_ts_umcsent, alpha = 0.05, test = c("kpss"))
# only 1 difference required 
# can also use ADF test (Augmented Dickey Fuller Test)

# create a differenced time series 
dscore_ts_umcsent = diff(score_ts_umcsent)
head(dscore_ts_umcsent)
dumcsent_ts_umcsent = diff(umcsent_ts_umcsent)
head(umcsent_ts_umcsent)

# plot differenced time series
par(mfrow=c(2,1))
plot.ts(dscore_ts_umcsent)
plot.ts(dumcsent_ts_umcsent)
# looks more or less stationary

# find the optimal lag 
auto.arima(dumcsent_ts_umcsent, xreg = dscore_ts_umcsent)
# optimal lags: ARIMA(0,0,0) on first differenced series

model4 = arima(dumcsent_ts_umcsent, order = c(0,0,0), xreg = dscore_ts_umcsent)
summary(model4)
model4_resid = residuals(model4)
# AIC: 282.45
acf(model4_resid)
pacf(model4_resid)

# check for model stationarity using Ljung-Box test and ADF test
Box.test(model4_resid, lag = 10, type= "Ljung-Box") # fail to reject Ho = stationary
adf.test(model4_resid, alternative = "stationary") # reject Ho = stationary

# Model 1 (ESS) is the best indicator of unemployment because it has an AIC of -54 (lowest absolute value)
# AIC 1: -54
# AIC 2: 471.22
# AIC 3: -637.89
# AIC 4: 282.45

