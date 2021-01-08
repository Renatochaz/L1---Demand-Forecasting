library("ggplot2")
library("forecast")
library("tseries")
library("tidyverse")
library("rio")
library(zoo)
library("normwhn.test")

df_alf_cres <- `alface crespa`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_TSObject2 = ts(df_alf_cres[,c('vendas')])
df_alf_cres$clean_count = tsclean(count_TSObject2)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = df_alf_cres, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = df_alf_cres, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
df_alf_cres$cnt_ma = ma(df_alf_cres$clean_count, order = 7)
df_alf_cres$cnt_ma30 = ma(df_alf_cres$clean_count, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(df_alf_cres)
df_alf_cres_2 <- replace(df_alf_cres, TRUE, lapply(df_alf_cres, na.aggregate))
summary(df_alf_cres_2)

# Plot original data
ggplot() +
  geom_line(data = df_alf_cres, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = df_alf_cres, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = df_alf_cres, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = df_alf_cres_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = df_alf_cres_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = df_alf_cres_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, 270 observation per unit of time (year)
count_ma = ts(na.omit(df_alf_cres_2$clean_count), frequency = 24)
decomp = stl(count_ma, "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)



# Is stationary? Yes
adf.test(count_ma, alternative = "stationary")

# ACF? Pretty bad
acf(count_ma, main = '')
pacf(count_ma, main = '')

#DIf 1
count_dl = diff(deseasonal_cnt, differences = 1)
plot(count_dl)
adf.test(count_dl, alternative = "stationary")

# ACF PACF
Acf(count_dl, main = 'ACF For Dif series')
Pacf(count_dl, main = 'PACF for Dif series')

#Autofit?
auto.arima(deseasonal_cnt, seasonal = FALSE)

fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 40, main = '(0,1,3) Model residuals')


fit2 <- arima(deseasonal_cnt, order=c(2,1,23))
tsdisplay(residuals(fit2), lag.max = 40, main = 'Seasonal Model residuals')
df_alf_cres_2$deseasonal_cnt = deseasonal_cnt
accuracy(fit2) # <- 83,7%

#Test holdout
hold <- window(ts(deseasonal_cnt), start = 749)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(749:872)]), order = c(2,1,23))
fcast_no_holdout <- forecast(fit_no_holdout, h=12)
plot(fcast_no_holdout, main = "")

lines(ts(count_TSObject2))
lines(ts(deseasonal_cnt))
lines(ts(count_ma))

#################test no deseason
##################################

clean_ts = ts(df_alf_cres_2$clean_count)

#Autofit?
auto.arima(clean_ts, seasonal = FALSE)

fit_clean <- auto.arima(clean_ts, seasonal = FALSE)
tsdisplay(residuals(fit_clean), lag.max = 40, main = '(1,1,1) Model residuals')

#custom fit?
fit_clean_custom <- arima(clean_ts, order = c(1,1,20))
tsdisplay(residuals(fit_clean_custom), lag.max = 40, main = 'custom model')
accuracy(fit_clean_custom) # <-  79,3% acc

hold_clean <- window(ts(clean_ts), start = 749)
fit_no_holdout_clean = arima(ts(clean_ts[-c(749:872)]), order = c(1,1,20))
fcast_no_holdout_clean <- forecast(fit_no_holdout_clean, h=12)
plot(fcast_no_holdout_clean, main = "")

lines(ts(count_TSObject2))


#################test no count ma weekly
##################################



#Autofit?
auto.arima(count_ma, seasonal = FALSE)

fit_ma <- auto.arima(count_ma, seasonal = FALSE)
tsdisplay(residuals(fit_ma), lag.max = 40, main = '(1,1,1) Model residuals')

#custom fit?
fit_ma_custom <- arima(count_ma, order = c(1,1,24))
tsdisplay(residuals(fit_ma_custom), lag.max = 40, main = 'custom model')
accuracy(fit_ma_custom) # <- 79,3%

hold_ma <- window(ts(count_ma), start = 500)
fit_no_holdout_ma = arima(ts(count_ma[-c(500:872)]), order = c(1,1,24))
fcast_no_holdout_ma <- forecast(fit_no_holdout_ma, h=12)
plot(fcast_no_holdout_ma, main = "")

lines(ts(count_ma))
checkresiduals(fit_no_holdout_ma)



# test with seasonality back
#fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal = TRUE)
#eas_fcast <- forecast(fit_w_seasonality, h = 15)
#plot(seas_fcast)

#lines(ts(count_ma))
#lines(ts(deseasonal_cnt))

#testind models
tsdisplay(residuals(fit_w_seasonality), lag.max=30, main = 'Seasonal model residual')

fit3 = auto.arima(deseasonal_cnt, seasonal = FALSE) #0,1,3
tsdisplay(residuals(fit3), lag.max = 30, main = 'Seasonal Model residuals')

# Evidence of lags, using our best model
fit4 = arima(deseasonal_cnt, order = c(1,1,25))
tsdisplay(residuals(fit4), lag.max = 30, main = 'Seasonal Model residuals')

#Default 1,1,1 arima
fit5 = arima(deseasonal_cnt, order = c(1,1,1))
tsdisplay(residuals(fit5), lag.max = 30, main = 'Seasonal model residuals')

#ETS orignal sales data
fit6 = ets(df_alf_cres_2$vendas)
plot(fit6)
tsdisplay(residuals(fit6), lag.max = 30)


#final fit
par(mfrow=c(2,3))


#autoarima (1,1,2) with seasonality
#fcast <- forecast(fit_w_seasonality, h=15)
#plot(fcast)
#lines(ts(deseasonal_cnt))

#autoarima (0,1,3) without seasonality
fcast2 <- forecast(fit3, h=15)
plot(fcast2)
lines(ts(deseasonal_cnt))

#custom arima (1,1,26)
fcast4 <- forecast(fit4, h=15)
plot(fcast4)
lines(ts(deseasonal_cnt))

#general arima
fcast5 <- forecast(fit5, h=15)
plot(fcast5)
lines(ts(deseasonal_cnt))

#ETS
fcast6 <- forecast(fit6, h=15)
plot(fcast6)
lines(ts(deseasonal_cnt))

# Accuracy test - 100 - MAPE = accuracy in %

#accuracy(fit_w_seasonality) 
accuracy(fit3)
accuracy(fit4) # <- 83,5% acc, 80$ with 24 frequency
accuracy(fit5)
accuracy(fit6)




### Test auto arima

tsdata <- ts(df_alf_cres$clean_count, frequency = 270, start = c(2017,1))
plot(tsdata)

autoarima1 <- auto.arima(tsdata)
forecast1 <- forecast(autoarima1, 26)
forecast1
plot(forecast1)
plot(forecast1$residuals)



# White noise?
whitenoise.test(clean_ts)

Box.test(clean_ts,type='Ljung',lag=log(length(clean_ts)))

plot(clean_ts)
mean(clean_ts)
var(clean_ts)
mean(clean_ts[1:100])
var(clean_ts[1:100])
mean(clean_ts[100:200])
var(clean_ts[100:200])
mean(clean_ts[200:300])
var(clean_ts[200:300])
mean(clean_ts[300:400])
var(clean_ts[300:400])
mean(clean_ts[400:600])
var(clean_ts[400:600])
mean(clean_ts[600:872])
var(clean_ts[600:872])

Box.test(count_ma,type='Ljung',lag=log(length(fit_ma_custom$residuals)))
checkresiduals(fit_ma_custom) # > 0,05 WN


whitenoise.test(fit_ma_custom$residuals)
plot(fit_ma_custom$residuals)
mean(fit_ma_custom$residuals)
var(fit_ma_custom$residuals)
mean(fit_ma_custom$residuals[1:100])
var(fit_ma_custom$residuals[1:100])
mean(fit_ma_custom$residuals[100:200])
var(fit_ma_custom$residuals[100:200])
mean(fit_ma_custom$residuals[200:500])
var(fit_ma_custom$residuals[200:500])
mean(fit_ma_custom$residuals[500:872])
var(fit_ma_custom$residuals[500:872])


#
checkresiduals(fit_ma) # No auto
checkresiduals(fit_ma_custom) # yes custom
checkresiduals(fit_no_holdout) # yes custom
checkresiduals(fit_clean) # yes auto clean
checkresiduals(fit_clean_custom) # no custom
accuracy(fit_ma_custom)




# Box jenkins

plot.ts(clean_ts[106:150])
