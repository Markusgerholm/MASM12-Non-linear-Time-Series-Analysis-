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
df_test <- helsingborg_2024[(end+2001):(end+2500), ]
plot(df_test$ts_utc, y_test1, type = "l", col = "blue", main = "Temperature of Helsingborg", ylab = "Temperature (C°)", xlab = "Time")
## ACF/PACF on training data
r <-  acf(y_train, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
axis(1, at = 0:100, labels = 0:100)   # label every lag
## ACF/PACF on seasonal difference
fit_plot <- Arima(y_train, order = c(0,0,0), seasonal = c(0,1,1))
e_plot <- fit_plot$residuals
r <-  acf(e_plot, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
axis(1, at = 0:100, labels = 0:100)   # label every lag
pacf(e_plot, lag.max = 50, na.action = na.omit, xaxt = "n", main = "PACF")
axis(1, at = 0:100, labels = 0:100)   # label every lag
## Final model
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
## ACF/PACF on final model residual
par(mfrow=c(1,2))
e_final <- fit$residuals
r <-  acf(e_final, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
axis(1, at = 0:100, labels = 0:100)   # label every lag
pacf(e_final, lag.max = 50, na.action = na.omit, xaxt = "n", main = "PACF")
axis(1, at = 0:100, labels = 0:100)   # label every lag

## Plot of fitted vs obs
train_df <- helsingborg_2024[start:end, ]

plot(train_df$ts_utc, y_train, type = "l", col = "blue", main = "Temperature vs predicted temperatue", ylab = "Temperature (C°)", xlab = "Time")
lines(train_df$ts_utc, fit$fitted, col = "red")
legend("topleft", legend = c("observed", "fitted"), col = c("blue", "red"), lty = 1, bty = "n")

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

## ACF/PACF for Helsingborg residual, up to 12 lags according to ACF
r <-  acf(e_train_he, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
axis(1, at = 0:100, labels = 0:100)   # label every lag
pacf(e_test_he, lag.max = 50, na.action = na.omit, xaxt = "n", main = "PACF")
axis(1, at = 0:100, labels = 0:100)   # label every lag

wd_all <- helsingborg_2024$Vindriktning   # <-- change if needed
ws_all <- helsingborg_2024$Vindhastighet  # <-- change if needed

reg_all <- wind_regime4(wd_all, ws_all, rot = 45)

reg_train <- reg_all[start:end]

# Ullared, Falsterbo, Hörby, Köbenhavn - switch string to get plots for another regime
string <- "R4"
par(mfrow=c(2,2))
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_ul,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Ullared vs Helsingborg"
)
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_fa,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Falsterbo vs Helsingborg"
)
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_hö,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Hörby vs Helsingborg"
)
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_kb,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "København vs Helsingborg"
)

# Roskilde, Sletterhage, Gniben, Anholt

par(mfrow=c(2,2))
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_ro,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Roskilde vs Helsingborg"
)
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_sl,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Sletterhage fyr vs Helsingborg"
)
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_gn,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Gniben vs Helsingborg"
)
plot_ccf_region(
  res_he = e_train_he,
  res_x  = e_train_an,
  reg_vec = reg_train,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Anholt vs Helsingborg"
)
## Screening based on CCF and lags yields:
#R1: falsterbo, hörby, roskilde 
#R2: ullared, köbenhavn, hörby, anholt 
#R3: falsterbo, köbenhavn, roskilde, hörby 
#R4: ullared, hörby, falsterbo

## Finding optimal lags/stations from subset
stations_by_regime <- list(
  R1 = c("fa","hö","ro"),
  R2 = c("ul","kb","hö","an"),
  R3 = c("fa","kb","ro","hö"),
  R4 = c("ul","hö", "fa")
)

P_max <- 12 # max lags for helsingborg residual
L_max <- 12 # max lags for exogenous lags

# Master list of all training residuals
ex_all <- list(
  ul = e_train_ul,
  fa = e_train_fa,
  hö = e_train_hö,
  kb = e_train_kb,
  ro = e_train_ro,
  gn = e_train_gn,
  an = e_train_an
)

fits_by_regime <- list()

for (R in names(stations_by_regime)) {
  exR <- ex_all[ stations_by_regime[[R]] ]
  fits_by_regime[[R]] <- fit_tarx_regime(
    e_he = e_train_he,
    ex_list = exR,
    reg_vec = reg_train,
    reg_name = R,
    P_max = 12,
    L_max = 12
  )
}

# quick summary
lapply(fits_by_regime, function(x) if (is.null(x)) NULL else c(n=x$n, aic=x$aic, adj_r2=x$adj_r2, n_coeff = length(x$terms)))

fit_R1 <- fits_by_regime$R1$fit

mf <- model.frame(fit_R1)     # data actually used after NA drops
y_obs <- mf$y                 # observed within R1 (training rows used)
y_hat <- fitted(fit_R1)       # fitted values aligned to y_obs

c(
  mean_obs = mean(y_obs),
  mean_hat = mean(y_hat),
  bias = mean(y_obs - y_hat),
  sd_obs = sd(y_obs),
  sd_hat = sd(y_hat),
  sd_ratio = sd(y_hat) / sd(y_obs),
  cor = cor(y_obs, y_hat)
)
acf(residuals(fits_by_regime$R4$fit), lag.max = 50)

plot(y_obs, type = "l", ylab = "e_HE", xlab = "Row (R1 training used)", main = "R1: fitted vs observed (training)", col = "blue")
lines(y_hat, col = "red")
legend("topright", legend = c("observed", "fitted"), col = c("blue", "red"), lty = 1, bty = "n")

## Testing if predictions are improved
i_test1_start <- end + gap
i_test1_end   <- end + gap + h - 1
reg_test <- reg_all[i_test1_start:i_test1_end]

rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
mae  <- function(a, b) mean(abs(a - b), na.rm = TRUE)

## Test "data"
ex_test_all <- list(
  ul = e_test_ul,
  fa = e_test_fa,
  hö = e_test_hö,
  kb = e_test_kb,
  ro = e_test_ro,
  gn = e_test_gn,
  an = e_test_an
)
## Function for predicting regimes based on wind direction
pred_out <- predict_tarx_on_test(
  fits_by_regime = fits_by_regime,
  e_test_he = e_test_he,
  ex_test_all = ex_test_all,
  reg_test = reg_test,
  stations_by_regime = stations_by_regime,
  P_max = 12,
  L_max = 12
)
## predicted forecast error
e_hat_test <- pred_out$yhat
## Plot for each regime
par(mfrow = c(2,2))
for (R in c("R1","R2","R3","R4")) {
  if (is.null(pred_out$eval_rows[[R]])) next
  y_obs <- pred_out$eval_rows[[R]]$y_obs
  y_hat <- pred_out$eval_rows[[R]]$y_hat
  
  plot(y_obs, type="l", main=paste0(R, ": observed vs predicted (test)"),
       xlab="Row (within regime, test)", ylab="e_HE")
  lines(y_hat, col=2)
  legend("topright", c("observed","predicted"), lty=1, col=c(1,2), bty="n")
}
par(mfrow = c(1,1))

ok <- complete.cases(as.numeric(e_test_he), e_hat_test)
## RMSE/MAE based on original residual and corrected residual
baseline_rmse <- rmse(as.numeric(e_test_he)[ok], 0)
baseline_mae  <- mae (as.numeric(e_test_he)[ok], 0)

corrected_rmse <- rmse(as.numeric(e_test_he)[ok] - e_hat_test[ok], 0)
corrected_mae  <- mae (as.numeric(e_test_he)[ok] - e_hat_test[ok], 0)

cat("Test points used:", sum(ok), "out of", length(e_test_he), "\n")
cat("Baseline (SARIMA errors):   RMSE =", baseline_rmse, " MAE =", baseline_mae, "\n")
cat("After TARX correction:      RMSE =", corrected_rmse, " MAE =", corrected_mae, "\n")
cat("RMSE improvement (%):", 100*(baseline_rmse - corrected_rmse)/baseline_rmse, "\n")
cat("MAE  improvement (%):", 100*(baseline_mae  - corrected_mae )/baseline_mae,  "\n")

## Adding to SARIMA forecast
fit1 <- Arima(y_train_test_he, model=fit)
yhat_sarima_all  <- fitted(fit1)  # 1-step predictions aligned to y_train_test1
yhat_sarima_test <- window(yhat_sarima_all, start = out_he$t_test_start, end = out_he$t_test_end)
y_obs_test <- y_test1

## TARX corrected errors
yhat_corr <- yhat_sarima_test + e_hat_test

ok <- complete.cases(as.numeric(y_obs_test), as.numeric(yhat_sarima_test), yhat_corr)

cat("SARIMA only: RMSE =", rmse(as.numeric(y_obs_test)[ok], as.numeric(yhat_sarima_test)[ok]),
    " MAE =", mae(as.numeric(y_obs_test)[ok], as.numeric(yhat_sarima_test)[ok]), "\n")

cat("SARIMA + TARX: RMSE =", rmse(as.numeric(y_obs_test)[ok], yhat_corr[ok]),
    " MAE =", mae(as.numeric(y_obs_test)[ok], yhat_corr[ok]), "\n")

# per-regime RMSE improvement
reg_levels <- sort(unique(as.character(reg_test[ok])))

per_regime <- do.call(rbind, lapply(reg_levels, function(R) {
  idx <- which(ok & as.character(reg_test) == R)
  if (length(idx) < 20) return(NULL)
  
  data.frame(
    regime = R,
    n = length(idx),
    RMSE_SARIMA = rmse(as.numeric(y_obs_test)[idx], as.numeric(yhat_sarima_test)[idx]),
    RMSE_CORR   = rmse(as.numeric(y_obs_test)[idx], yhat_corr[idx]),
    MAE_SARIMA  = mae(as.numeric(y_obs_test)[idx], as.numeric(yhat_sarima_test)[idx]),
    MAE_CORR    = mae(as.numeric(y_obs_test)[idx], yhat_corr[idx])
  )
}))

# add % improvements (positive = better)
if (!is.null(per_regime) && nrow(per_regime) > 0) {
  per_regime$RMSE_impr_pct <- 100*(per_regime$RMSE_SARIMA - per_regime$RMSE_CORR)/per_regime$RMSE_SARIMA
  per_regime$MAE_impr_pct  <- 100*(per_regime$MAE_SARIMA  - per_regime$MAE_CORR )/per_regime$MAE_SARIMA
}

print(per_regime)


