##### TS final para os dados ######
###################################
library("ggplot2")
library("forecast")
library("tseries")
library("tidyverse")
library("rio")
library(zoo)

#########################################
########## ALFACE CRESPA ################
#########################################

ts_alf_cres <- `alface crespa`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_alf_cres = ts(ts_alf_cres[,c('vendas')])
ts_alf_cres$clean_count = tsclean(count_ts_alf_cres)


### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_alf_cres, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_alf_cres, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_alf_cres$cnt_ma = ma(ts_alf_cres$clean_count, order = 6)
ts_alf_cres$cnt_ma30 = ma(ts_alf_cres$clean_count, order = 24)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_alf_cres)
ts_alf_cres_2 <- replace(ts_alf_cres, TRUE, lapply(ts_alf_cres, na.aggregate))
summary(ts_alf_cres_2)

# Plot original data
ggplot() +
  geom_line(data = ts_alf_cres, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_alf_cres, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_alf_cres, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_alf_cres_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_alf_cres_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_alf_cres_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ma_alf_cres = ts(na.omit(ts_alf_cres_2$clean_count), frequency = 290)

# Is stationary?
adf.test(count_ma_alf_cres, alternative = "stationary") # YES

# ACF / PACF
acf(count_ma_alf_cres, main = '')
pacf(count_ma_alf_cres, main = '')

# Autofit
auto.arima(count_ma_alf_cres, seasonal = FALSE)

fit_ma_alf_cres <- auto.arima(count_ma_alf_cres, seasonal = FALSE)
tsdisplay(residuals(fit_ma_alf_cres), lag.max = 40, main = '(1,1,1) Model residuals')

# Custom fit
fit_ma_custom_alf_cres <- arima(count_ma_alf_cres, order = c(1,1,24))
tsdisplay(residuals(fit_ma_custom_alf_cres), lag.max = 40, main = 'custom model')
accuracy(fit_ma_custom_alf_cres) # <- 79,3%

# Testing and prediction on set with best model
hold_ma_1 <- window(ts(count_ma_alf_cres), start = 500)
fit_no_holdout_ma_alf_cres = arima(ts(count_ma_alf_cres[-c(500:872)]), order = c(1,1,24))
fcast_no_holdout_ma_alf_cres <- forecast(fit_no_holdout_ma_alf_cres, h=12)
plot(fcast_no_holdout_ma_alf_cres, main = "")

lines(ts(count_ma_alf_cres))

# White noise test
checkresiduals(fit_no_holdout_ma_alf_cres)

## Prediction after set
fit_future_alf_cres <- arima(ts(count_ma_alf_cres), order = c(1,1,24))
fcast_future_alf_cres <- forecast(fit_future_alf_cres, h=10)
plot(fcast_future_alf_cres, main = "")
lines(ts(count_ma_alf_cres))
accuracy(fit_future_alf_cres)

# White noise test
checkresiduals(fit_future_alf_cres)

#### CREATING PREDICTED TABLE ####
pred_alf_cres <- rename(data.frame(as.numeric(fcast_no_holdout_ma_alf_cres$mean)), predicted = 1)
pred_alf_cres$dia <- ts_alf_cres_2$dia[500:511]
pred_alf_cres$vendas <- ts_alf_cres_2$vendas[500:511]
pred_alf_cres <- pred_alf_cres[c(2,3,1)]
pred_alf_cres$erro <- pred_alf_cres$vendas - pred_alf_cres$predicted
pred_alf_cres$precisao <- 100 - (abs(pred_alf_cres$erro / pred_alf_cres$vendas * 100))

### Computing table of future predictions ###
future_predictions <- data.frame(1:10)
future_predictions <- rename(future_predictions, Data = 1)
future_predictions$Data <- c("01/10/2019","02/10/2019","03/10/2019","04/10/2019","05/10/2019","07/10/2019","08/10/2019","09/10/2019","10/10/2019","11/10/2019")
future_predictions$Vendas_Alface_Crespa <- fcast_future_alf_cres$mean


#########################################
########## ALFACE LISA ################
#########################################

ts_alf_lisa <- `alface lisa`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_alf_lisa = ts(ts_alf_lisa[,c('vendas')])
ts_alf_lisa$clean_count = tsclean(count_ts_alf_lisa)


### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_alf_lisa, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_alf_lisa, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_alf_lisa$cnt_ma = ma(ts_alf_lisa$clean_count, order = 6)
ts_alf_lisa$cnt_ma30 = ma(ts_alf_lisa$clean_count, order = 24)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_alf_lisa)
ts_alf_lisa_2 <- replace(ts_alf_lisa, TRUE, lapply(ts_alf_lisa, na.aggregate))
summary(ts_alf_lisa_2)
count_ts_alf_lisa_2 = ts(ts_alf_lisa_2[,c('vendas')])

# Plot original data
ggplot() +
  geom_line(data = ts_alf_lisa, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_alf_lisa, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_alf_lisa, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_alf_lisa_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_alf_lisa_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_alf_lisa_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ma_alf_lisa = ts(na.omit(ts_alf_lisa_2$clean_count), frequency = 290)

# Is stationary?
adf.test(count_ma_alf_lisa, alternative = "stationary")# YES
adf.test(count_ts_alf_lisa_2, alternative = "stationary")

# ACF / PACF
acf(count_ma_alf_lisa, main = '')
pacf(count_ma_alf_lisa, main = '')

acf(count_ts_alf_lisa_2, main = '')
pacf(count_ts_alf_lisa_2, main = '')

# Autofit
auto.arima(count_ma_alf_lisa, seasonal = FALSE)
auto.arima(count_ts_alf_lisa_2, seasonal = FALSE)

fit_ma_alf_lisa <- auto.arima(count_ma_alf_lisa, seasonal = FALSE)
tsdisplay(residuals(fit_ma_alf_lisa), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_ma_alf_lisa)
checkresiduals(fit_ma_alf_lisa)

fit_ts_alf_lisa <- auto.arima(count_ts_alf_lisa_2, seasonal = FALSE)
tsdisplay(residuals(fit_ts_alf_lisa), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_ts_alf_lisa)
checkresiduals(fit_ts_alf_lisa)


# Custom fit
fit_ma_custom_alf_lisa <- arima(count_ma_alf_lisa, order = c(1,1,24))
tsdisplay(residuals(fit_ma_custom_alf_lisa), lag.max = 40, main = 'custom model')
accuracy(fit_ma_custom_alf_lisa) # <- 
checkresiduals(fit_ma_custom_alf_lisa)

fit_ts_custom_alf_lisa <- arima(count_ts_alf_lisa_2, order = c(1,1,24))
tsdisplay(residuals(fit_ts_custom_alf_lisa), lag.max = 40, main = 'custom model')
accuracy(fit_ts_custom_alf_lisa) # <- 
checkresiduals(fit_ts_custom_alf_lisa)

# Testing and prediction on set with best model
hold_ma_1 <- window(ts(count_ma_alf_lisa), start = 500)
fit_no_holdout_ma_alf_lisa = arima(ts(count_ma_alf_lisa[-c(500:872)]), order = c(1,1,24))
fcast_no_holdout_ma_alf_lisa <- forecast(fit_no_holdout_ma_alf_lisa, h=10)
plot(fcast_no_holdout_ma_alf_lisa, main = "")

lines(ts(count_ma_alf_lisa))

### UNPREDICTABLE IN THIS MODEL ###

#########################################
########## ALHO SOLTO KG ################
#########################################

ts_alho <- `alho solto kg`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_alho = ts(ts_alho[,c('vendas')])
ts_alho$clean_count = tsclean(count_ts_alho)
clean_ts_alho_clean = ts(ts_alho$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_alho, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_alho, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

recort <- ts_alho[106:376,1:30]

# Recort
ggplot() +
  geom_line(data = recort, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')

recort2 <- ts_alho[106:194,1:30]

# Recort
ggplot() +
  geom_line(data = recort2, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')

## Get monthly and weekly moving average  (MA)
ts_alho$cnt_ma = ma(ts_alho$clean_count, order = 6)
ts_alho$cnt_ma30 = ma(ts_alho$clean_count, order = 24)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_alho)
ts_alho_2 <- replace(ts_alho, TRUE, lapply(ts_alho, na.aggregate))
summary(ts_alho_2)

# Plot original data
ggplot() +
  geom_line(data = ts_alho, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_alho, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_alho, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_alho_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_alho_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_alho_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ma_alho = ts(na.omit(ts_alho_2$clean_count), frequency = 63)

# test decomp
decomp_alho = stl(count_ma_alho, "periodic")
deseasonal_cnt_alho <- seasadj(decomp_alho)
plot(decomp)

# Is stationary?
adf.test(count_ma_alho, alternative = "stationary") # YES
adf.test(deseasonal_cnt_alho, alternative = "stationary") # YES
adf.test(clean_ts_alho_clean, alternative = "stationary") # YES



# ACF / PACF
acf(count_ma_alho, main = '')
pacf(count_ma_alho, main = '')

acf(deseasonal_cnt_alho, main = '')
pacf(deseasonal_cnt_alho, main = '')

acf(clean_ts_alho_clean, main = '')
pacf(clean_ts_alho_clean, main = '')

# Autofit
auto.arima(count_ma_alho, seasonal = FALSE)

fit_ma_alho <- auto.arima(count_ma_alho, seasonal = FALSE)
tsdisplay(residuals(fit_ma_alho), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_ma_alho)
checkresiduals(fit_ma_alho)

# 2
fit_clean_alho <- auto.arima(clean_ts_alho_clean, seasonal = FALSE)
tsdisplay(residuals(fit_clean_alho), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_clean_alho)
checkresiduals(fit_clean_alho)

# 3
fit_desea_alho <- auto.arima(deseasonal_cnt_alho, seasonal = FALSE)
tsdisplay(residuals(fit_desea_alho), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_desea_alho)
checkresiduals(fit_desea_alho)


# Custom fit
fit_ma_custom_alho <- arima(count_ma_alho, order = c(4,0,35))
tsdisplay(residuals(fit_ma_custom_alho), lag.max = 40, main = 'custom model')
accuracy(fit_ma_custom_alho)
checkresiduals(fit_ma_custom_alho)

# Custom 2
fit_clean_custom_alho <- arima(clean_ts_alho_clean, order = c(4,0,5), seasonal = c(1,0,40))
tsdisplay(residuals(fit_clean_alho), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_clean_alho)
checkresiduals(fit_clean_alho)

# Testing and prediction on set with best model
hold_ma_alho_1 <- window(ts(count_ma_alho), start = 500)
fit_no_holdout_ma_alho = arima(ts(count_ma_alho[-c(500:872)]), order = c(4,0,35))
fcast_no_holdout_ma_alho <- forecast(fit_no_holdout_ma_alho, h=12)
plot(fcast_no_holdout_ma_alho, main = "")

lines(ts(count_ma_alho))

# White noise test
checkresiduals(fit_no_holdout_ma_alho)

## Prediction after set
fit_future_alho <- arima(ts(count_ma_alho), order = c(4,0,35))
fcast_future_alho <- forecast(fit_future_alho, h=10)
plot(fcast_future_alho, main = "")
lines(ts(count_ma_alho))
accuracy(fit_future_alho) # <- 66,9%

# White noise test
checkresiduals(fit_future_alho)

#### CREATING PREDICTED TABLE ####
pred_alho <- rename(data.frame(as.numeric(fcast_no_holdout_ma_alho$mean)), predicted = 1)
pred_alho$dia <- ts_alho_2$dia[500:511]
pred_alho$vendas <- ts_alho_2$vendas[500:511]
pred_alho <- pred_alho[c(2,3,1)]
pred_alho$erro <- pred_alho$vendas - pred_alho$predicted
pred_alho$precisao <- 100 - (abs(pred_alho$erro / pred_alho$vendas * 100))

### Computing table of future predictions ###
future_predictions$Vendas_Alho <- fcast_future_alho$mean

#########################################
########## ALFACE CRESPA WEEK DATE ################
#########################################

week_ts_alf_cres <- alf_cresp_week

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
week_count_ts_alf_cres = ts(week_ts_alf_cres[,c('vendas')])
week_ts_alf_cres$clean_count = tsclean(week_count_ts_alf_cres)


### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = week_ts_alf_cres, aes(x = week, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = week_ts_alf_cres, aes(x = week, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
week_ts_alf_cres$cnt_ma = ma(week_ts_alf_cres$clean_count, order = 7)
week_ts_alf_cres$cnt_ma30 = ma(week_ts_alf_cres$clean_count, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(week_ts_alf_cres)
week_ts_alf_cres_2 <- replace(week_ts_alf_cres, TRUE, lapply(week_ts_alf_cres, na.aggregate))
summary(week_ts_alf_cres_2)

# Plot original data
ggplot() +
  geom_line(data = week_ts_alf_cres, aes(x=week, y = vendas, colour = "Total sales")) +
  geom_line(data = week_ts_alf_cres, aes(x=week, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = week_ts_alf_cres, aes(x=week, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = week_ts_alf_cres_2, aes(x=week, y = clean_count, colour = "Total sales")) +
  geom_line(data = week_ts_alf_cres_2, aes(x=week, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = week_ts_alf_cres_2, aes(x=week, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
#count_ma_alf_cres = ts(na.omit(ts_alf_cres_2$clean_count), frequency = 290)

# Is stationary?
adf.test(week_count_ts_alf_cres, alternative = "stationary")


# ACF / PACF
acf(week_count_ts_alf_cres, main = '')
pacf(week_count_ts_alf_cres, main = '')

# Autofit
auto.arima(week_count_ts_alf_cres, seasonal = FALSE)
auto.arima(week_count_ts_alf_cres, seasonal = TRUE)

fit_week_alf_cres <- auto.arima(week_count_ts_alf_cres, seasonal = FALSE)
tsdisplay(residuals(fit_week_alf_cres), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_week_alf_cres)
checkresiduals(fit_week_alf_cres)


# Custom fit
fit_week_custom_alf_cres <- arima(week_count_ts_alf_cres, order = c(1,0,60))
tsdisplay(residuals(fit_week_custom_alf_cres), lag.max = 40, main = 'custom model')
accuracy(fit_week_custom_alf_cres) # <- 79,3%

fit_week_scustom_alf_cres <- arima(week_count_ts_alf_cres, order = c(1,0,20), season = c(1,0,20))
tsdisplay(residuals(fit_week_scustom_alf_cres), lag.max = 40, main = 'custom model')
accuracy(fit_week_scustom_alf_cres) # <- 79,3%

# Testing and prediction on set with best model
hold_sweek_alf_cres <- window(ts(week_count_ts_alf_cres), start = 110) # (10,0,18) (3,0,3)
fit_no_holdout_sweek_alf_cres = arima(ts(week_count_ts_alf_cres[-c(110:152)]), order = c(10,0,18), season = c(3,0,3))
accuracy(fit_no_holdout_sweek_alf_cres)
fcast_no_holdout_sweek_alf_cres <- forecast(fit_no_holdout_sweek_alf_cres, h=12)
plot(fcast_no_holdout_sweek_alf_cres, main = "")

lines(ts(week_count_ts_alf_cres))

# Testing and prediction on set with best model
hold_sweek_alf_cres <- window(ts(week_count_ts_alf_cres), start = 90) # (10,0,18) (3,0,3)
fit_no_holdout_sweek_alf_cres = arima(ts(week_count_ts_alf_cres[-c(90:152)]), order = c(10,0,18), season = c(3,0,3))
accuracy(fit_no_holdout_sweek_alf_cres)
fcast_no_holdout_sweek_alf_cres <- forecast(fit_no_holdout_sweek_alf_cres, h=12)
plot(fcast_no_holdout_sweek_alf_cres, main = "")

lines(ts(week_count_ts_alf_cres))

# White noise test
checkresiduals(fit_no_holdout_sweek_alf_cres)

##################################################
########## BATATA COMUM GRANEL KG ################
##################################################

ts_btt_comum <- `batata comum granel kg`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_btt_com = ts(ts_btt_comum[,c('vendas')])
ts_btt_comum$clean_count = tsclean(count_ts_btt_com)
ts_btt_com_clean = ts(ts_btt_comum$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_btt_comum, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_btt_comum, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

recort <- ts_alho[106:376,1:30]

# Recort
ggplot() +
  geom_line(data = recort, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')

recort2 <- ts_alho[106:194,1:30]

# Recort
ggplot() +
  geom_line(data = recort2, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')

## Get monthly and weekly moving average  (MA)
ts_btt_comum$cnt_ma = ma(ts_btt_comum$clean_count, order = 7)
ts_btt_comum$cnt_ma30 = ma(ts_btt_comum$clean_count, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_btt_comum)
ts_btt_comum_2 <- replace(ts_btt_comum, TRUE, lapply(ts_btt_comum, na.aggregate))
summary(ts_btt_comum_2)

# Plot original data
ggplot() +
  geom_line(data = ts_btt_comum, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_btt_comum, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_btt_comum, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_btt_comum_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_btt_comum_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_btt_comum_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ma_alho = ts(na.omit(ts_alho_2$clean_count), frequency = 63)

# test decomp
decomp_alho = stl(count_ma_alho, "periodic")
deseasonal_cnt_alho <- seasadj(decomp_alho)
plot(decomp)

# Is stationary?
adf.test(ts_btt_com_clean, alternative = "stationary") # YES




# ACF / PACF
acf(ts_btt_com_clean, main = '')
pacf(ts_btt_com_clean, main = '')

# Autofit
auto.arima(ts_btt_com_clean, seasonal = FALSE)

fit_btt_com <- auto.arima(ts_btt_com_clean, seasonal = FALSE)
tsdisplay(residuals(fit_btt_com), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_btt_com)
checkresiduals(fit_btt_com)


# Custom fit
fit_custom_btt_com <- arima(ts_btt_com_clean, order = c(3,1,24))
tsdisplay(residuals(fit_custom_btt_com), lag.max = 40, main = 'custom model')
accuracy(fit_custom_btt_com)
checkresiduals(fit_custom_btt_com)

# Custom fit
fit_custom_btt_com_2 <- arima(ts_btt_com_clean, order = c(3,1,24), seasonal = c(1,1,10))
tsdisplay(residuals(fit_custom_btt_com_2), lag.max = 40, main = 'custom model')
accuracy(fit_custom_btt_com_2)
checkresiduals(fit_custom_btt_com_2)

#########################################
########## BATATA DOCE ################
#########################################

ts_btt_doce <- `batata deoce roxa kg`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_btt_doce = ts(ts_btt_doce[,c('vendas')])
ts_btt_doce$clean_count = tsclean(count_ts_btt_doce)
ts_btt_doce_clean = ts(ts_btt_doce$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_btt_doce, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_btt_doce, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_btt_doce$cnt_ma = ma(ts_btt_doce$clean_count, order = 7)
ts_btt_doce$cnt_ma30 = ma(ts_btt_doce$clean_count, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_btt_doce)
ts_btt_doce_2 <- replace(ts_btt_doce, TRUE, lapply(ts_btt_doce, na.aggregate))
summary(ts_btt_doce_2)

# Plot original data
ggplot() +
  geom_line(data = ts_btt_doce, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_btt_doce, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_btt_doce, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_btt_doce_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_btt_doce_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_btt_doce_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ma_alho = ts(na.omit(ts_alho_2$clean_count), frequency = 63)
count_ts_btt_doce_clean = ts(na.omit(ts_btt_doce_2$clean_count))

# test decomp
decomp_alho = stl(count_ma_alho, "periodic")
deseasonal_cnt_alho <- seasadj(decomp_alho)
plot(decomp)

# Is stationary?
adf.test(count_ts_btt_doce_clean, alternative = "stationary") # YES




# ACF / PACF
acf(count_ts_btt_doce_clean, main = '')
pacf(count_ts_btt_doce_clean, main = '')


# Autofit
auto.arima(count_ts_btt_doce_clean, seasonal = FALSE)

fit_count_ts_btt_doce_clean <- auto.arima(count_ts_btt_doce_clean, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_btt_doce_clean), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_btt_doce_clean)
checkresiduals(fit_count_ts_btt_doce_clean)


# Custom fit
fit_ma_custom_count_ts_btt_doce_clean <- arima(count_ts_btt_doce_clean, order = c(4,0,35))
tsdisplay(residuals(fit_ma_custom_count_ts_btt_doce_clean), lag.max = 40, main = 'custom model')
accuracy(fit_ma_custom_count_ts_btt_doce_clean)
checkresiduals(fit_ma_custom_count_ts_btt_doce_clean)

#########################################
########## BROCOLI NINJA ################
#########################################

ts_brocolis <- `brocolis un`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_brocolis = ts(ts_brocolis[,c('vendas')])
ts_brocolis$clean_count = tsclean(count_ts_brocolis)
ts_brocolis_clean = ts(ts_brocolis$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_brocolis, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_brocolis, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_brocolis$cnt_ma = ma(ts_brocolis$vendas, order = 7)
ts_brocolis$cnt_ma30 = ma(ts_brocolis$vendas, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_brocolis)
ts_brocolis_2 <- replace(ts_brocolis, TRUE, lapply(ts_brocolis, na.aggregate))
summary(ts_broc_ninja_2)

# Plot original data
ggplot() +
  geom_line(data = ts_brocolis, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_brocolis, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_brocolis, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_brocolis_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_brocolis_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_brocolis_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ts_brocolis = ts(na.omit(ts_brocolis_2$clean_count), frequency = 26)


# Is stationary?
adf.test(count_ts_brocolis, alternative = "stationary") # YES



# ACF / PACF
acf(count_ts_brocolis, main = '')
pacf(count_ts_brocolis, main = '')


# Autofit
auto.arima(count_ts_brocolis, seasonal = FALSE)

fit_count_ts_brocolis <- auto.arima(count_ts_brocolis, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_brocolis), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_brocolis)
checkresiduals(fit_count_ts_brocolis)

# Autofit

fit_count_ts_brocolis_custom <- arima(count_ts_brocolis, order = c(4,1,22))
tsdisplay(residuals(fit_count_ts_brocolis_custom), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_brocolis_custom)
checkresiduals(fit_count_ts_brocolis_custom)

# Testing and prediction on set with best model
hold_ma_broc_ninja_1 <- window(ts(count_ts_broc_ninja), start = 500)
fit_no_holdout_ma_broc_ninja = arima(ts(count_ts_broc_ninja[-c(500:872)]), order = c(1,1,5))
fcast_no_holdout_ma_broc_ninja <- forecast(fit_no_holdout_ma_broc_ninja, h=12)
plot(fcast_no_holdout_ma_broc_ninja, main = "")

lines(ts(count_ts_broc_ninja))

#########################################
########## BROCOLI NINJA ################
#########################################

ts_broc_ninja <- `brocoli ninja in`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_broc_ninja = ts(ts_broc_ninja[,c('vendas')])
ts_broc_ninja$clean_count = tsclean(count_ts_broc_ninja)
ts_broc_ninja_clean = ts(ts_broc_ninja$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_broc_ninja, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_broc_ninja, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_broc_ninja$cnt_ma = ma(ts_broc_ninja$clean_count, order = 7)
ts_broc_ninja$cnt_ma30 = ma(ts_broc_ninja$clean_count, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_broc_ninja)
ts_broc_ninja_2 <- replace(ts_broc_ninja, TRUE, lapply(ts_broc_ninja, na.aggregate))
summary(ts_broc_ninja_2)

# Plot original data
ggplot() +
  geom_line(data = ts_broc_ninja, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_broc_ninja, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_broc_ninja, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_broc_ninja_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_broc_ninja_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_broc_ninja_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ts_broc_ninja = ts(na.omit(ts_broc_ninja_2$clean_count), frequency = 290)


# Is stationary?
adf.test(count_ts_broc_ninja, alternative = "stationary") # YES



# ACF / PACF
acf(count_ts_broc_ninja, main = '')
pacf(count_ts_broc_ninja, main = '')


# Autofit
auto.arima(count_ts_broc_ninja, seasonal = FALSE)

fit_count_ts_broc_ninja <- auto.arima(count_ts_broc_ninja, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_broc_ninja), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_broc_ninja)
checkresiduals(fit_count_ts_broc_ninja)

# Testing and prediction on set with best model
hold_ma_broc_ninja_1 <- window(ts(count_ts_broc_ninja), start = 500)
fit_no_holdout_ma_broc_ninja = arima(ts(count_ts_broc_ninja[-c(500:872)]), order = c(1,1,5))
fcast_no_holdout_ma_broc_ninja <- forecast(fit_no_holdout_ma_broc_ninja, h=12)
plot(fcast_no_holdout_ma_broc_ninja, main = "")

lines(ts(count_ts_broc_ninja))

#########################################
########## BROCOLI NINJA ################
#########################################

ts_cebola <- `cebola granel kg`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_cebola = ts(ts_cebola[,c('vendas')])
ts_cebola$clean_count = tsclean(count_ts_cebola)
ts_cebola_clean = ts(ts_cebola$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_cebola, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_cebola, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_cebola$cnt_ma = ma(ts_cebola$vendas, order = 7)
ts_cebola$cnt_ma30 = ma(ts_cebola$vendas, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_cebola)
ts_cebola_2 <- replace(ts_cebola, TRUE, lapply(ts_cebola, na.aggregate))
summary(ts_cebola_2)

# Plot original data
ggplot() +
  geom_line(data = ts_cebola, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_cebola, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_cebola, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_cebola_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_cebola_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_cebola_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ts_cebola = ts(na.omit(ts_cebola_2$clean_count), frequency = 26)


# Is stationary?
adf.test(count_ts_cebola, alternative = "stationary") # YES



# ACF / PACF
acf(count_ts_cebola, main = '')
pacf(count_ts_cebola, main = '')


# Autofit
auto.arima(count_ts_cebola, seasonal = FALSE)

fit_count_ts_cebola <- auto.arima(count_ts_cebola, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_cebola), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cebola)
checkresiduals(fit_count_ts_cebola)

# Autofit

fit_count_ts_cebola_custom <- arima(count_ts_cebola, order = c(2,1,40))
tsdisplay(residuals(fit_count_ts_cebola_custom), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cebola_custom)
checkresiduals(fit_count_ts_cebola_custom)

# Testing and prediction on set with best model
hold_ts_cebola <- window(ts(count_ts_cebola), start = 500)
fit_no_holdout_ts_cebola = arima(ts(count_ts_cebola[-c(500:872)]), order = c(2,1,40))
fcast_no_holdout_ts_cebola <- forecast(fit_no_holdout_ts_cebola, h=12)
plot(fcast_no_holdout_ts_cebola, main = "")

lines(ts(count_ts_cebola))

## Prediction after set
fit_future_cebola <- arima(ts(count_ts_cebola), order = c(2,1,35))
fcast_future_cebola <- forecast(fit_future_cebola, h=10)
plot(fcast_future_cebola, main = "")
lines(ts(count_ts_cebola))
accuracy(fit_future_cebola)

#### CREATING PREDICTED TABLE ####
pred_cebola <- rename(data.frame(as.numeric(fcast_no_holdout_ts_cebola$mean)), predicted = 1)
pred_cebola$dia <- ts_cebola_2$dia[500:511]
pred_cebola$vendas <- ts_cebola_2$vendas[500:511]
pred_cebola <- pred_cebola[c(2,3,1)]
pred_cebola$erro <- pred_cebola$vendas - pred_cebola$predicted
pred_cebola$precisao <- 100 - (abs(pred_cebola$erro / pred_cebola$vendas * 100))

### Computing table of future predictions ###
future_predictions$Vendas_cebola <- fcast_future_cebola$mean

#########################################
########## BROCOLI NINJA ################
#########################################

ts_cenoura <- `cenoura kg`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_cenoura = ts(ts_cenoura[,c('vendas')])
ts_cenoura$clean_count = tsclean(count_ts_cenoura)
ts_cenoura_clean = ts(ts_cenoura$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_cenoura, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_cenoura, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_cenoura$cnt_ma = ma(ts_cenoura$vendas, order = 7)
ts_cenoura$cnt_ma30 = ma(ts_cenoura$vendas, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_cenoura)
ts_cenoura_2 <- replace(ts_cenoura, TRUE, lapply(ts_cenoura, na.aggregate))
summary(ts_cenoura_2)

# Plot original data
ggplot() +
  geom_line(data = ts_cenoura, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_cenoura, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_cenoura, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_cenoura_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_cenoura_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_cenoura_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ts_cenoura = ts(na.omit(ts_cenoura_2$clean_count), frequency = 26)


# Is stationary?
adf.test(count_ts_cenoura, alternative = "stationary") # YES



# ACF / PACF
acf(count_ts_cenoura, main = '')
pacf(count_ts_cenoura, main = '')


# Autofit
auto.arima(count_ts_cenoura, seasonal = FALSE)

fit_count_ts_cenoura <- auto.arima(count_ts_cenoura, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_cenoura), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cenoura)
checkresiduals(fit_count_ts_cenoura)

# Autofit

fit_count_ts_cenoura_custom <- arima(count_ts_cenoura, order = c(2,1,40))
tsdisplay(residuals(fit_count_ts_cenoura_custom), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cenoura_custom)
checkresiduals(fit_count_ts_cenoura_custom)

#########################################
########## CHEIRO VERDE ################
#########################################

ts_cverde <- `cheiro verde maco`

## Creating ts object based on vendas to pass tsclean() 
#[ clean outliers and missing data]
count_ts_cverde = ts(ts_cverde[,c('vendas')])
ts_cverde$clean_count = tsclean(count_ts_cverde)
ts_cverde_clean = ts(ts_cverde$clean_count)

### Graph cleaned data
# By day cleaned
ggplot() +
  geom_line(data = ts_cverde, aes(x = poxicct_dia, y = clean_count)) + ylab('cleaned count')

# By day real
ggplot() +
  geom_line(data = ts_cverde, aes(x = poxicct_dia, y = vendas)) + ylab('cleaned count')
# Notice presence of outliers

## Get monthly and weekly moving average  (MA)
ts_cverde$cnt_ma = ma(ts_cverde$vendas, order = 7)
ts_cverde$cnt_ma30 = ma(ts_cverde$vendas, order = 30)

## Check and replace NAS - Ignore NA warning
## In this step quebras and entradas become useless because of too much replacement
summary(ts_cverde)
ts_cverde_2 <- replace(ts_cverde, TRUE, lapply(ts_cverde, na.aggregate))
summary(ts_cverde_2)

# Plot original data
ggplot() +
  geom_line(data = ts_cverde, aes(x=poxicct_dia, y = vendas, colour = "Total sales")) +
  geom_line(data = ts_cverde, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_cverde, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = ts_cverde_2, aes(x=poxicct_dia, y = clean_count, colour = "Total sales")) +
  geom_line(data = ts_cverde_2, aes(x=poxicct_dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = ts_cverde_2, aes(x=poxicct_dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, daily - monthly effect adjust
count_ts_cverde = ts(na.omit(ts_cverde_2$clean_count), frequency = 26)


# Is stationary?
adf.test(count_ts_cverde, alternative = "stationary") # YES



# ACF / PACF
acf(count_ts_cverde, main = '')
pacf(count_ts_cverde, main = '')


# Autofit
auto.arima(count_ts_cverde, seasonal = FALSE)

fit_count_ts_cverde <- auto.arima(count_ts_cverde, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_cverde), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cverde)
checkresiduals(fit_count_ts_cverde)

# Autofit
auto.arima(count_ts_cverde, seasonal = TRUE)

fit_count_ts_cverde <- auto.arima(count_ts_cverde, seasonal = FALSE)
tsdisplay(residuals(fit_count_ts_cverde), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cverde)
checkresiduals(fit_count_ts_cverde)

# CUstom

fit_count_ts_cverde_custom <- arima(count_ts_cverde, order = c(5,1,30))
tsdisplay(residuals(fit_count_ts_cverde_custom), lag.max = 40, main = '(1,1,1) Model residuals')
accuracy(fit_count_ts_cverde_custom)
checkresiduals(fit_count_ts_cverde_custom)

# Testing and prediction on set with best model
hold_ts_cverde <- window(ts(count_ts_cverde), start = 500)
fit_no_holdout_ts_cverde = arima(ts(count_ts_cverde[-c(500:872)]), order = c(5,1,25))
fcast_no_holdout_ts_cverde <- forecast(fit_no_holdout_ts_cverde, h=12)
plot(fcast_no_holdout_ts_cverde, main = "")

lines(ts(count_ts_cverde))

## Prediction after set
fit_future_cverde <- arima(ts(count_ts_cverde), order = c(5,1,25))
fcast_future_cverde <- forecast(fit_future_cverde, h=10)
plot(fcast_future_cverde, main = "")
lines(ts(count_ts_cverde))
accuracy(fit_future_cverde)

#### CREATING PREDICTED TABLE ####
pred_cverde <- rename(data.frame(as.numeric(fcast_no_holdout_ts_cverde$mean)), predicted = 1)
pred_cverde$dia <- ts_cverde_2$dia[500:511]
pred_cverde$vendas <- ts_cverde_2$vendas[500:511]
pred_cverde <- pred_cverde[c(2,3,1)]
pred_cverde$erro <- pred_cverde$vendas - pred_cverde$predicted
pred_cverde$precisao <- 100 - (abs(pred_cverde$erro / pred_cverde$vendas * 100))

### Computing table of future predictions ###
future_predictions$Vendas_cverde <- fcast_future_cverde$mean



write.csv(future_predictions,"G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Resultados/predicoes_futuras.csv", row.names = FALSE)
write.csv(pred_alf_cres,"G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Resultados/predicoes_alf_cres.csv", row.names = FALSE)
write.csv(pred_cebola,"G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Resultados/predicoes_pred_cebola.csv", row.names = FALSE)
write.csv(pred_cverde,"G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Resultados/predicoes_pred_cverde.csv", row.names = FALSE)
write.csv(pred_alho,"G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Resultados/predicoes_pred_alho.csv", row.names = FALSE)

pred_cebola
pred_cverde
pred_alho