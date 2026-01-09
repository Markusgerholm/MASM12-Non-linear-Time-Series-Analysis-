## Function for regimes
wind_regime4 <- function(wd, ws, rot = 45) {
  wd <- (wd - rot) %% 360
  out <- rep(NA_character_, length(wd))
  # Only assign R1–R4 when wind speed is not zero, when zero, set regime to R0
  idx <- !is.na(ws) & ws != 0 & !is.na(wd)
  out[!is.na(ws) & ws == 0] <- "R0"
  out[idx & wd >   0 & wd <=  90] <- "R1"
  out[idx & wd >  90 & wd <= 180] <- "R2"
  out[idx & wd > 180 & wd <= 270] <- "R3"
  out[idx & wd > 270 & wd <  360] <- "R4"
  out[idx & wd == 0]              <- "R4"
  factor(out, levels = c("R0","R1","R2","R3","R4"))
}

train_test <- function(y_all, frequency, h_total, h2) {
  start <- 3200
  end   <- 5900
  t_start <- 1 + (start - 1) / frequency
  # End of (intervening + test2)
  t_end_total <- 1 + (end + h_total - 1) / frequency
  # Far test2 start = end + (h_total - h2)
  t_test_start <- 1 + (end + (h_total - h2)) / frequency
  t_test_end   <- 1 + (end + h_total - 1) / frequency
  list(
    y_train_total = window(y_all, start = t_start, end = t_end_total),
    t_test_start = t_test_start,
    t_test_end   = t_test_end
  )
}

train_test_res <- function(fit, y_train_test, t_test_start, t_test_end){
  fit1 <- Arima(y_train_test, model = fit)   # parameters held fixed
  e_all <- fit1$residuals # 1 step forecast errors from training + test set
  e_test <- window(e_all, start = t_test_start, end = t_test_end) # 1 step forecast errors for actual testing period
  list(fit1 = fit1, e_all = e_all, e_test = e_test)
}

plot_ccf_hourly <- function(x, y, max_lag_hours = 72, main = NULL) {
  # compute ccf without plotting
  cc <- ccf(x, y, lag.max = max_lag_hours, plot = FALSE, na.action = na.omit)
  
  lags <- as.numeric(cc$lag)   # in "time steps" (hours for hourly ts)
  vals <- as.numeric(cc$acf)
  
  # keep only lag >= 0
  keep <- lags >= 0
  lags_pos <- lags[keep]
  vals_pos <- vals[keep]
  
  # approximate 95% CI used by ccf (same idea as acf/ccf default)
  n <- sum(complete.cases(as.numeric(x), as.numeric(y)))
  ci <- 1.96 / sqrt(n)
  
  # plot
  plot(lags_pos, vals_pos, type = "h", xlab = "Lag (hours, >= 0)", ylab = "CCF",
       main = main)
  abline(h = 0)
  abline(h = c(-ci, ci), lty = 2)
}

plot_ccf_poslags <- function(x, y, max_lag = 50, main = "") {
  cc <- ccf(x, y, lag.max = max_lag, plot = FALSE, na.action = na.omit)
  lags <- as.numeric(cc$lag)
  vals <- as.numeric(cc$acf)
  
  keep <- lags >= 0
  lags_pos <- lags[keep]
  vals_pos <- vals[keep]
  
  n <- sum(complete.cases(as.numeric(x), as.numeric(y)))
  ci <- 1.96 / sqrt(n)
  
  plot(lags_pos, vals_pos, type = "h",
       xlab = "Lag (hours, >= 0)", ylab = "CCF",
       main = main)
  abline(h = 0)
  abline(h = c(-ci, ci), lty = 2)
  
  invisible(data.frame(lag = lags_pos, ccf = vals_pos))
}
