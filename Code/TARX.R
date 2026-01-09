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
t_test_start <- 1 + (end) / freq          # row end + 1
t_test_end   <- 1 + (end + h - 1) / freq  # row end + h
# train as ts
y_train <- window(y_all, start = t_start, end = t_end)
y_test  <- window(y_all, start = t_test_start, end = t_test_end)
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

## Calling function to get training data and data up to test set
out_he <- train_test(ts(helsingborg_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_he <- out_he$y_train_total

out_ul <- train_test(ts(ullared_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_ul <- out_ul$y_train_total

out_fa <- train_test(ts(falsterbo_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_fa <- out_fa$y_train_total

out_hö <- train_test(ts(hörby_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_hö <- out_hö$y_train_total

out_kb <- train_test(ts(köbenhavn_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_kb <- out_kb$y_train_total

out_ro <- train_test(ts(roskilde_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_ro <- out_ro$y_train_total

out_sl <- train_test(ts(slatterhage_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_sl <- out_sl$y_train_total

out_gn <- train_test(ts(gniben_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_gn <- out_gn$y_train_total

out_an <- train_test(ts(anholt_2024$Lufttemperatur, frequency = freq), frequency = freq, h_total=2500,h2=500)
y_train_test_an <- out_an$y_train_total

## 1 step forecast error for test set
#Ullared, Falsterbo, Hörby, Köbenhavn, Roskilde, Sletterhage, Gniben, Anholt
res_he <- train_test_res(fit, y_train_test_he, out_he$t_test_start, out_he$t_test_end)
e_test_he <- res_he$e_test

res_ul <- train_test_res(fit_ul_auto, y_train_test_ul, out_ul$t_test_start, out_ul$t_test_end)
e_test_ul <- res_ul$e_test

res_fa <- train_test_res(fit_fa_auto, y_train_test_fa, out_fa$t_test_start, out_fa$t_test_end)
e_test_fa <- res_fa$e_test

res_hö <- train_test_res(fit_hö_auto, y_train_test_hö, out_hö$t_test_start, out_hö$t_test_end)
e_test_hö <- res_hö$e_test

res_kb <- train_test_res(fit_kb_auto, y_train_test_kb, out_kb$t_test_start, out_kb$t_test_end)
e_test_kb <- res_kb$e_test

res_ro <- train_test_res(fit_ro_auto, y_train_test_ro, out_ro$t_test_start, out_ro$t_test_end)
e_test_ro <- res_ro$e_test

res_sl <- train_test_res(fit_sl_auto, y_train_test_sl, out_sl$t_test_start, out_sl$t_test_end)
e_test_sl <- res_sl$e_test

res_gn <- train_test_res(fit_gn_auto, y_train_test_gn, out_gn$t_test_start, out_gn$t_test_end)
e_test_gn <- res_gn$e_test
e_test_gn <- na.interp(e_test_gn)

res_an <- train_test_res(fit_an_auto, y_train_test_an, out_an$t_test_start, out_an$t_test_end)
e_test_an <- res_an$e_test

## ACF/PACF for Helsingborg residual, up to 12 lags according to ACF
r <-  acf(e_test_he, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
axis(1, at = 0:100, labels = 0:100)   # label every lag
pacf(e_test_he, lag.max = 50, na.action = na.omit, xaxt = "n", main = "PACF")
axis(1, at = 0:100, labels = 0:100)   # label every lag

## CCF between Helsingborg and station X
plot_ccf_hourly(e_test_he, e_test_gn, max_lag_hours = 50)

## Extracting residuals from training sequence
t_train_start <- t_start
t_train_end <- t_end
e_train_he <- window(res_he$e_all, start = t_train_start, end = t_train_end)
e_train_ul <- window(res_ul$e_all, start = t_train_start, end = t_train_end)
e_train_fa <- window(res_fa$e_all, start = t_train_start, end = t_train_end)
e_train_hö <- window(res_hö$e_all, start = t_train_start, end = t_train_end)
e_train_kb <- window(res_kb$e_all, start = t_train_start, end = t_train_end)
e_train_ro <- window(res_ro$e_all, start = t_train_start, end = t_train_end)
e_train_sl <- window(res_sl$e_all, start = t_train_start, end = t_train_end)
e_train_gn <- window(res_gn$e_all, start = t_train_start, end = t_train_end)
e_train_an <- window(res_an$e_all, start = t_train_start, end = t_train_end)

wd_all <- helsingborg_2024$Vindriktning   # <-- change if needed
ws_all <- helsingborg_2024$Vindhastighet  # <-- change if needed

reg_all <- wind_regime4(wd_all, ws_all, rot = 45)

# Put regimes on the same ts index as y_all so window() works
reg_ts <- ts(reg_all, frequency = freq)

reg_train <- window(reg_ts, start = t_train_start, end = t_train_end)

ex_list <- list(
  ul = e_train_ul,
  fa = e_train_fa,
  hö = e_train_hö,
  kb = e_train_kb,
  ro = e_train_ro,
  sl = e_train_sl,
  gn = e_train_gn,
  an = e_train_an
)

maxLag <- 50
topK <- 5

ccf_peaks <- list()

for (R in levels(reg_train)) {
  idx <- which(reg_train == R)
  
  # Skip empty/small regimes
  if (length(idx) < 200) next
  
  # Build regime-specific vectors (keep alignment)
  yR <- e_train_he[idx]
  
  for (nm in names(ex_list)) {
    xR <- ex_list[[nm]][idx]
    
    # Drop NA pairs
    ok <- complete.cases(as.numeric(yR), as.numeric(xR))
    yRR <- yR[ok]
    xRR <- xR[ok]
    if (length(yRR) < 200) next
    
    # Plot CCF positive lags: ccf(y, x)
    # NOTE: Positive lags in ccf(y, x) means x leads y (since x is the 2nd series).
    df_cc <- plot_ccf_poslags(yRR, xRR, max_lag = maxLag,
                              main = paste0("CCF (train) ", R, ": HE vs ", nm))
    
    # Save top peaks (by absolute magnitude)
    ord <- order(abs(df_cc$ccf), decreasing = TRUE)
    peaks <- head(df_cc[ord, ], topK)
    peaks$regime <- R
    peaks$station <- nm
    
    ccf_peaks[[paste(R, nm, sep = "_")]] <- peaks
  }
}

ccf_peaks_df <- do.call(rbind, ccf_peaks)
ccf_peaks_df <- ccf_peaks_df[order(ccf_peaks_df$regime, ccf_peaks_df$station, -abs(ccf_peaks_df$ccf)), ]
print(ccf_peaks_df)
