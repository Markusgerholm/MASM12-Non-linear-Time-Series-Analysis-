## Loading data and packages

library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)
source("Code/DenmarkFunctions.r") # function for Danish data
source("Code/read_data2.r") # data
source("Code/Functions.r") # functions
set.seed(132)
## Indices for training, validation and testing
start <- 3200
end   <- 5900
N <- 8783
freq <- 24

i_train_start <- start
i_train_end   <- end

i_test_start  <- N - 500 + 1   # 8285
i_test_end    <- N             # 8784

i_val_start   <- end + 1       # 5901
i_val_end     <- i_test_start - 1  # 8284

# ts "time" values
t_train_start <- 1 + (i_train_start - 1) / freq
t_train_end   <- 1 + (i_train_end   - 1) / freq
t_val_start   <- 1 + (i_val_start   - 1) / freq
t_val_end     <- 1 + (i_val_end     - 1) / freq
t_test_start  <- 1 + (i_test_start  - 1) / freq
t_test_end    <- 1 + (i_test_end    - 1) / freq

# --- Helsingborg
y_all_he <- ts(helsingborg_2024$Lufttemperatur, frequency = freq)
split_he <- train_val_test_split(y_all_he, frequency = freq, start = start, end = end, test_len = 500)

y_train_he <- split_he$y_train
y_val_he   <- split_he$y_val
y_test_he  <- split_he$y_test
## Fitting base ARIMA model
p <- rep(0, 18)
p[1] <- NA
p[2] <- NA
p[11] <- 0
p[18] <- NA
q <- rep(0, 23)
q[2] <- NA
q[20] <- NA
q[23] <- NA
fit <- Arima(y_train_he,
             order = c(18,0,23),
             fixed = c(p,q,NA),
             seasonal = c(0,1,1))
## Extracting validation residuals and testing residuals
res_he <- get_val_test_errors(y_all_he, fit, split_he)
e_val_he  <- res_he$e_val
e_test_he <- res_he$e_test

## Modeling exogenous inputs
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

## Extracting validation and testing residuals for all exogenous inputs

# --- Ullared
y_all_ul <- ts(ullared_2024$Lufttemperatur, frequency = freq)
split_ul <- train_val_test_split(y_all_ul, frequency = freq, start = start, end = end, test_len = 500)
res_ul   <- get_val_test_errors(y_all_ul, fit_ul_auto, split_ul)
e_val_ul  <- res_ul$e_val
e_test_ul <- res_ul$e_test

# --- Falsterbo
y_all_fa <- ts(falsterbo_2024$Lufttemperatur, frequency = freq)
split_fa <- train_val_test_split(y_all_fa, frequency = freq, start = start, end = end, test_len = 500)
res_fa   <- get_val_test_errors(y_all_fa, fit_fa_auto, split_fa)
e_val_fa  <- res_fa$e_val
e_test_fa <- res_fa$e_test

# --- Hörby
y_all_hö <- ts(hörby_2024$Lufttemperatur, frequency = freq)
split_hö <- train_val_test_split(y_all_hö, frequency = freq, start = start, end = end, test_len = 500)
res_hö   <- get_val_test_errors(y_all_hö, fit_hö_auto, split_hö)
e_val_hö  <- res_hö$e_val
e_test_hö <- res_hö$e_test

# --- Köbenhavn
y_all_kb <- ts(köbenhavn_2024$Lufttemperatur, frequency = freq)
split_kb <- train_val_test_split(y_all_kb, frequency = freq, start = start, end = end, test_len = 500)
res_kb   <- get_val_test_errors(y_all_kb, fit_kb_auto, split_kb)
e_val_kb  <- res_kb$e_val
e_test_kb <- res_kb$e_test

# --- Roskilde
y_all_ro <- ts(roskilde_2024$Lufttemperatur, frequency = freq)
split_ro <- train_val_test_split(y_all_ro, frequency = freq, start = start, end = end, test_len = 500)
res_ro   <- get_val_test_errors(y_all_ro, fit_ro_auto, split_ro)
e_val_ro  <- res_ro$e_val
e_test_ro <- res_ro$e_test

# --- Slatterhage
y_all_sl <- ts(slatterhage_2024$Lufttemperatur, frequency = freq)
split_sl <- train_val_test_split(y_all_sl, frequency = freq, start = start, end = end, test_len = 500)
res_sl   <- get_val_test_errors(y_all_sl, fit_sl_auto, split_sl)
e_val_sl  <- res_sl$e_val
e_test_sl <- res_sl$e_test

# --- Gniben
y_all_gn <- ts(gniben_2024$Lufttemperatur, frequency = freq)
split_gn <- train_val_test_split(y_all_gn, frequency = freq, start = start, end = end, test_len = 500)
res_gn   <- get_val_test_errors(y_all_gn, fit_gn_auto, split_gn)
e_val_gn  <- res_gn$e_val
e_test_gn <- res_gn$e_test

# --- Anholt
y_all_an <- ts(anholt_2024$Lufttemperatur, frequency = freq)
split_an <- train_val_test_split(y_all_an, frequency = freq, start = start, end = end, test_len = 500)
res_an   <- get_val_test_errors(y_all_an, fit_an_auto, split_an)
e_val_an  <- res_an$e_val
e_test_an <- res_an$e_test

# Helsingborg ACF/PACF on validation residual
r <-  acf(e_val_he, lag.max = 50, na.action = na.omit, xaxt = "n", plot = FALSE)
r$acf[1] <- NA
plot(r, main = "ACF", xaxt = "n")
#axis(1, at = (0:50)/freq, labels = 0:50)  # 0..50 hours
axis(1, at = 0:100, labels = 0:100)   # label every lag
pacf(e_val_he, lag.max = 50, na.action = na.omit, xaxt = "n", main = "PACF")
axis(1, at = 0:100, labels = 0:100)   # label every lag

wd_all <- helsingborg_2024$Vindriktning  
ws_all <- helsingborg_2024$Vindhastighet  

reg_all <- wind_regime4(wd_all, ws_all, rot = 45)

reg_train <- reg_all[start:end]
N <- length(helsingborg_2024$Lufttemperatur)  # 8783
i_val_start <- end + 1
i_test_start <- N - 500 + 1
i_val_end <- i_test_start - 1
reg_val <- reg_all[i_val_start:i_val_end]

# Ullared, Falsterbo, Hörby, Köbenhavn - switch string to get plots for another regime
string <- "R4"
par(mfrow=c(2,2))
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_ul,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Ullared vs Helsingborg"
)
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_fa,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Falsterbo vs Helsingborg"
)
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_hö,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Hörby vs Helsingborg"
)
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_kb,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "København vs Helsingborg"
)

# Roskilde, Sletterhage, Gniben, Anholt

par(mfrow=c(2,2))
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_ro,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Roskilde vs Helsingborg"
)
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_sl,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Sletterhage fyr vs Helsingborg"
)
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_gn,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Gniben vs Helsingborg"
)
plot_ccf_region(
  res_he = e_val_he,
  res_x  = e_val_an,
  reg_vec = reg_val,
  regimes = c(string),     # choose one or several, e.g. c("R1","R2","R3","R4")
  lag_max = 50,
  main_prefix = "Anholt vs Helsingborg"
)

## Best candidates for each regime
# R1: Falsterbo, Anholt, Köbenhavn
# R2: Ullared, Falsterbo, Hörby
# R3: Hörby, Falsterbo, Ullared
# R4: Ullared, Falsterbo, Köbenhavn

## Finding optimal lags/stations from subset
stations_by_regime <- list(
  R1 = c("fa","an","kb"),
  R2 = c("ul","fa", "hö"),
  R3 = c("hö", "fa", "ul"),
  R4 = c("ul","ro", "kb")
)

P_max <- 12 # max lags for helsingborg residual
L_max <- 12 # max lags for exogenous lags

# Master list of all training residuals
ex_all <- list(
  ul = e_val_ul,
  fa = e_val_fa,
  hö = e_val_hö,
  kb = e_val_kb,
  ro = e_val_ro,
  gn = e_val_gn,
  an = e_val_an
)

fits_by_regime <- list()

for (R in names(stations_by_regime)) {
  exR <- ex_all[ stations_by_regime[[R]] ]
  fits_by_regime[[R]] <- fit_tarx_regime(
    e_he = e_val_he,
    ex_list = exR,
    reg_vec = reg_val,
    reg_name = R,
    P_max = P_max,
    L_max = L_max
  )
}

# quick summary
lapply(fits_by_regime, function(x) if (is.null(x)) NULL else c(n=x$n, aic=x$aic, adj_r2=x$adj_r2, n_coeff = length(x$terms)))
# Summary of each fit
summary(fits_by_regime$R1$fit)
summary(fits_by_regime$R2$fit)
summary(fits_by_regime$R3$fit)
summary(fits_by_regime$R4$fit)

# rmse and mae functions
rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
mae  <- function(a, b) mean(abs(a - b), na.rm = TRUE)

fit_R1 <- fits_by_regime$R1$fit

mf <- model.frame(fit_R1)     # data actually used after NA drops
y_obs <- mf$y                 # observed within R1 (training rows used)
y_hat <- fitted(fit_R1)       # fitted values aligned to y_obs
rmse(y_obs, 0)
rmse(y_obs - y_hat, 0)
# Bias? Proportion of variance?
c(
  mean_obs = mean(y_obs),
  mean_hat = mean(y_hat),
  bias = mean(y_obs - y_hat),
  sd_obs = sd(y_obs),
  sd_hat = sd(y_hat),
  sd_ratio = sd(y_hat) / sd(y_obs),
  cor = cor(y_obs, y_hat)
)
# Plot of observed vs fitted
par(mfrow = c(1,1))
plot(y_obs, type = "l", ylab = "e_HE", xlab = "Row (R1 training used)", main = "R1: fitted vs observed (training)", col = "blue")
lines(y_hat, col = "red")
legend("topright", legend = c("observed", "fitted"), col = c("blue", "red"), lty = 1, bty = "n")

# Evaluating on test forecast errors
test_len <- 500
i_test_start <- N - test_len + 1
i_test_end   <- N
reg_test <- reg_all[i_test_start:i_test_end]

stopifnot(length(reg_test) == length(e_test_he))
table(reg_test, useNA = "ifany")

ex_test_all <- list(
  ul = e_test_ul,
  fa = e_test_fa,
  hö = e_test_hö,
  kb = e_test_kb,
  ro = e_test_ro,
  gn = e_test_gn,
  an = e_test_an
)

ex_val_all <- list(ul=e_val_ul, fa=e_val_fa, hö=e_val_hö, kb=e_val_kb,
                   ro=e_val_ro, gn=e_val_gn, an=e_val_an)

ex_test_all <- list(ul=e_test_ul, fa=e_test_fa, hö=e_test_hö, kb=e_test_kb,
                    ro=e_test_ro, gn=e_test_gn, an=e_test_an)

pred_out <- predict_tarx_on_test(
  fits_by_regime = fits_by_regime,
  e_val_he = e_val_he,
  e_test_he = e_test_he,
  ex_val_all = ex_val_all,
  ex_test_all = ex_test_all,
  reg_test = reg_test,
  stations_by_regime = stations_by_regime,
  P_max = P_max,
  L_max = L_max
)

e_hat_test <- pred_out$yhat

## Per regime RMSE
# Per-regime RMSE/MAE on the TEST forecast errors
reg_levels <- c("R1","R2","R3","R4")

per_regime_metrics <- do.call(rbind, lapply(reg_levels, function(R) {
  
  idx <- which(as.character(reg_test) == R)
  
  # only evaluate where TARX prediction exists (and e_test exists)
  okR <- idx[complete.cases(as.numeric(e_test_he)[idx], e_hat_test[idx])]
  
  if (length(okR) < 5) return(NULL)
  
  e_obs <- as.numeric(e_test_he)[okR]
  e_cor <- e_obs - e_hat_test[okR]
  
  RMSE_base <- rmse(e_obs, 0)          # baseline: predict error = 0
  RMSE_corr <- rmse(e_cor, 0)          # corrected: error after TARX
  MAE_base  <- mae(e_obs, 0)
  MAE_corr  <- mae(e_cor, 0)
  
  data.frame(
    regime = R,
    n = length(okR),
    RMSE_base = RMSE_base,
    RMSE_corr = RMSE_corr,
    RMSE_impr_pct = 100*(RMSE_base - RMSE_corr)/RMSE_base,
    MAE_base = MAE_base,
    MAE_corr = MAE_corr,
    MAE_impr_pct = 100*(MAE_base - MAE_corr)/MAE_base
  )
}))

print(per_regime_metrics)


par(mfrow = c(2,2))
for (R in c("R1","R2","R3","R4")) {
  if (is.null(pred_out$eval_rows[[R]])) next
  y_obs <- pred_out$eval_rows[[R]]$y_obs
  y_hat <- pred_out$eval_rows[[R]]$y_hat
  
  plot(y_obs, type="l",
       main=paste0(R, ": observed vs predicted"),
       xlab="Regime index", ylab="Test forecast error")
  lines(y_hat, col=2)
  legend("bottomright", c("observed","predicted"), lty=1, col=c(1,2), bty="n", cex = 0.5)
}
par(mfrow = c(1,1))

ok <- complete.cases(as.numeric(e_test_he), e_hat_test)

baseline_rmse <- rmse(as.numeric(e_test_he)[ok], 0)
baseline_mae  <- mae (as.numeric(e_test_he)[ok], 0)

corrected_rmse <- rmse(as.numeric(e_test_he)[ok] - e_hat_test[ok], 0)
corrected_mae  <- mae (as.numeric(e_test_he)[ok] - e_hat_test[ok], 0)

cat("Test points used:", sum(ok), "out of", length(e_test_he), "\n")
cat("Baseline (SARIMA errors):   RMSE =", baseline_rmse, " MAE =", baseline_mae, "\n")
cat("After TARX correction:      RMSE =", corrected_rmse, " MAE =", corrected_mae, "\n")
cat("RMSE improvement (%):", 100*(baseline_rmse - corrected_rmse)/baseline_rmse, "\n")
cat("MAE  improvement (%):", 100*(baseline_mae  - corrected_mae )/baseline_mae,  "\n")

## SARIMA predictions with TARX correction
## --- SARIMA 1-step forecasts on the FINAL test set (last 500) ---

# Run SARIMA forward over train+val+test with parameters fixed
fit1 <- Arima(split_he$y_train_val_test, model = fit)

# 1-step forecasts (fitted values) for the whole train+val+test series
yhat_sarima_all <- fitted(fit1)

# Slice out the last-500 test window using split boundaries
yhat_sarima_test <- window(yhat_sarima_all,
                           start = split_he$t_test_start,
                           end   = split_he$t_test_end)

# Observed test values (last 500)
y_obs_test <- split_he$y_test

stopifnot(length(y_obs_test) == length(yhat_sarima_test))
stopifnot(length(y_obs_test) == length(e_hat_test))
stopifnot(length(y_obs_test) == length(reg_test))

## --- TARX-corrected SARIMA forecast ---
yhat_corr <- as.numeric(yhat_sarima_test) + as.numeric(e_hat_test)

ok <- complete.cases(as.numeric(y_obs_test),
                     as.numeric(yhat_sarima_test),
                     yhat_corr,
                     reg_test)

cat("Test points used:", sum(ok), "out of", length(y_obs_test), "\n\n")

cat("SARIMA only: RMSE =",
    rmse(as.numeric(y_obs_test)[ok], as.numeric(yhat_sarima_test)[ok]),
    " MAE =",
    mae(as.numeric(y_obs_test)[ok], as.numeric(yhat_sarima_test)[ok]),
    "\n")

cat("SARIMA + TARX: RMSE =",
    rmse(as.numeric(y_obs_test)[ok], yhat_corr[ok]),
    " MAE =",
    mae(as.numeric(y_obs_test)[ok], yhat_corr[ok]),
    "\n\n")

## --- Per-regime RMSE/MAE and improvements ---
reg_levels <- sort(unique(as.character(reg_test[ok])))

per_regime <- do.call(rbind, lapply(reg_levels, function(R) {
  idx <- which(ok & as.character(reg_test) == R)
  if (length(idx) < 20) return(NULL)
  
  RMSE_SARIMA <- rmse(as.numeric(y_obs_test)[idx], as.numeric(yhat_sarima_test)[idx])
  RMSE_CORR   <- rmse(as.numeric(y_obs_test)[idx], yhat_corr[idx])
  MAE_SARIMA  <- mae (as.numeric(y_obs_test)[idx], as.numeric(yhat_sarima_test)[idx])
  MAE_CORR    <- mae (as.numeric(y_obs_test)[idx], yhat_corr[idx])
  
  data.frame(
    regime = R,
    n = length(idx),
    RMSE_SARIMA = RMSE_SARIMA,
    RMSE_CORR   = RMSE_CORR,
    MAE_SARIMA  = MAE_SARIMA,
    MAE_CORR    = MAE_CORR,
    RMSE_impr_pct = 100*(RMSE_SARIMA - RMSE_CORR)/RMSE_SARIMA,
    MAE_impr_pct  = 100*(MAE_SARIMA  - MAE_CORR )/MAE_SARIMA
  )
}))

print(per_regime)

## Plot of predictions
ts_test <- helsingborg_2024$ts_utc[(N - test_len + 1):N]

plot(ts_test[ok], as.numeric(y_obs_test)[ok], type="l",
     xlab="Time", ylab="Temperature (C°)",
     main="Observed vs SARIMA vs SARIMA+TARX")
lines(ts_test[ok], as.numeric(yhat_sarima_test)[ok], col="blue")
lines(ts_test[ok], yhat_corr[ok], col="red")
legend("bottomright", c("Observed","SARIMA","SARIMA + TARX"),
       lty=1, col=c(1,"blue","red"), bty="n", cex=0.75)
