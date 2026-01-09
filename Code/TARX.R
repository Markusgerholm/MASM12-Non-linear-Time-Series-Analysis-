## Loading data and packages

library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)
source("Code/DenmarkFunctions.r") # function for Danish data
source("Code/read_data2.r") # data
source("Code/Functions.r") # functions

## Fitting base line model (SARIMA)
h <- 500 # length of test set
freq <- 24
# full series from your dataframe
y_all <- ts(helsingborg_2024$Lufttemperatur, frequency = freq)
# convert row indices to ts "time" values
t_start <- 1 + (start - 1) / freq
t_end   <- 1 + (end   - 1) / freq
# train as ts
y_train <- window(y_all, start = t_start, end = t_end)
# additional test set roughly 6 months forward in time
gap <- 2000  
t_test1_start <- t_test_start + gap / freq
t_test1_end   <- t_test1_start + (h - 1) / freq
y_test1 <- window(y_all, start = t_test1_start, end = t_test1_end)
p <- rep(0, 18)
p[1] <- NA
p[2] <- NA
p[11] <- 0
p[18] <- NA
q <- rep(0, 23)
q[2] <- NA
q[20] <- NA
q[23] <- NA
fit <- Arima(y_train,
             order = c(18,0,23),
             fixed = c(p,q,NA),
             seasonal = c(0,1,1))
## Prediction residual from test set far away from training set
y_train_test1 <- window(y_all, start = t_start, end = t_test1_end) # training data + test data
fit1 <- Arima(y_train_test1, model = fit)   # parameters held fixed
e_all <- fit1$residuals # 1 step forecast errors from training + test set
e_test1 <- window(e_all, start = t_test1_start, end = t_test1_end) # 1 step forecast errors for actual testing period
r <-  acf(e_test1, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
axis(1, at = 0:100, labels = 0:100)   # label every lag
pacf(e_test1, lag.max = 50, na.action = na.omit, xaxt = "n", main = "PACF")
axis(1, at = 0:100, labels = 0:100)   # label every lag

## Modeling weather stations
## Helsingborg - compare to base line model
y_he <- ts(df_he$Lufttemperatur, frequency = freq)
fit_he_auto <- auto.arima(y_he)

## Ullared
# Imputing missing values with auto.arima and then fitting new auto.arima on complete time series
y_obs <- ts(df_ul$Lufttemperatur, frequency = freq)
impute_ul <- auto.arima(y_obs)
kr <- KalmanRun(y_obs, impute_ul$model)
id_na <- which(is.na(y_obs))
y_hat <- as.numeric(impute_ul$model$Z %*% t(kr$states)) 
y_filled <- y_obs
y_filled[id_na] <- y_hat[id_na]
df_ul$Lufttemperatur[id_na] <- y_hat[id_na]
y_ul <- ts(df_ul$Lufttemperatur, frequency = freq)
fit_ul_auto <- auto.arima(y_ul)

## Falsterbo
y_fa <- ts(df_fa$Lufttemperatur, frequency = freq)
fit_fa_auto <- auto.arima(y_fa)

## Hörby
y_hö <- ts(df_hö$Lufttemperatur, frequency = freq)
fit_hö_auto <- auto.arima(y_hö)

## Köbenhavn lufthavn
y_kb <- ts(df_kb$Lufttemperatur, frequency = freq)
fit_kb_auto <- auto.arima(y_kb)

## Roskilde lufthavn
y_ro <- ts(df_ro$Lufttemperatur, frequency = freq)
y_ro_filled <- na.interp(y_ro)
df_ro$Lufttemperatur <- as.numeric(y_ro_filled)
fit_ro_auto <- auto.arima(y_ro_filled)

## Sletterhage fyr
y_sl <- ts(df_sl$Lufttemperatur, frequency = freq)
y_sl_filled <- na.interp(y_sl)
df_sl$Lufttemperatur <- as.numeric(y_sl_filled)
fit_sl_auto <- auto.arima(y_sl_filled)

## Gniben
y_gn <- ts(df_gn$Lufttemperatur, frequency = freq)
y_gn_filled <- na.interp(y_gn)
df_gn$Lufttemperatur <- as.numeric(y_gn_filled)
fit_gn_auto <- auto.arima(y_gn_filled)

## Anholt
# Imputing missing values with auto.arima and then fitting new auto.arima on complete time series
y_obs <- ts(df_an$Lufttemperatur, frequency = freq)
impute_an <- auto.arima(y_obs)
kr <- KalmanRun(y_obs, impute_an$model)
id_na <- which(is.na(y_obs))
y_hat <- as.numeric(impute_an$model$Z %*% t(kr$states)) 
y_filled <- y_obs
y_filled[id_na] <- y_hat[id_na]
df_an$Lufttemperatur[id_na] <- y_hat[id_na]
y_an <- ts(df_an$Lufttemperatur, frequency = freq)
fit_an_auto <- auto.arima(y_an)
