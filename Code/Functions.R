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

plot_ccf_region <- function(res_he,
                                              res_x,
                                              reg_vec,
                                              regimes = c("R1","R2","R3","R4"),
                                              lag_max = 50,
                                              positive_lags_only = TRUE,
                                              main_prefix = "X vs Helsingborg CCF") {
  # res_he: Helsingborg residuals (ts or numeric)
  # res_x : other-station residuals (ts or numeric)
  # reg_vec: vector/factor of regimes, same length/order as residuals
  # regimes: which regime(s) to plot
  
  # Basic checks
  if (length(res_he) != length(res_x)) stop("res_he and res_x must have same length.")
  if (length(reg_vec) != length(res_he)) stop("reg_vec must have same length as residuals.")
  
  # Make sure reg_vec is character/factor usable with %in%
  reg_vec <- as.character(reg_vec)
  
  # Loop over regimes → separate plots
  for (reg in regimes) {
    
    idx <- which(reg_vec == reg)
    
    # Skip empty/small regimes
    if (length(idx) < 5) {
      message("Skipping ", reg, ": too few points (", length(idx), ").")
      next
    }
    
    xR  <- res_x[idx]
    heR <- res_he[idx]
    
    # Drop NA pairs
    ok <- complete.cases(as.numeric(xR), as.numeric(heR))
    xRR  <- xR[ok]
    heRR <- heR[ok]
    
    if (length(xRR) < 5) {
      message("Skipping ", reg, ": too few complete pairs after NA removal.")
      next
    }
    
    r <- ccf(xRR, heRR, lag.max = lag_max, na.action = na.omit, plot = FALSE)
    # Positive lags only if requested
    keep <- r$lag > 0
    #keep <- if (positive_lags_only) r$lag >= 0 else rep(TRUE, length(r$lag))
    
    # CI lines (same idea as default acf/ccf)
    n <- length(xRR)
    ci <- 1.96 / sqrt(n)
    
    plot(r$lag[keep], r$acf[keep], type = "h",
         xlab = "Lag",
         ylab = "CCF",
         main = paste0(main_prefix, " – ", reg),
         cex.main = 0.9,
         ylim = c(-0.1, 0.3))
    
    abline(h = 0)
    abline(h = c(-ci, ci), lty = 2)
  }
}

make_lags <- function(x, max_lag, prefix) {
  out <- as.data.frame(
    sapply(1:max_lag, function(L) dplyr::lag(as.numeric(x), L))
  )
  names(out) <- paste0(prefix, "_L", 1:max_lag)
  out
}

fit_tarx_regime <- function(e_he, ex_list, reg_vec, reg_name,
                            P_max = 12, L_max = 12) {
  
  idx <- which(as.character(reg_vec) == reg_name)
  
  # Response = HE error at time t, within regime rows
  y <- as.numeric(e_he)[idx]
  
  # Build candidate lag columns (using the full series first, then subset rows)
  df <- data.frame(y = as.numeric(e_he))
  df <- cbind(df, make_lags(e_he, P_max, "he"))
  
  for (nm in names(ex_list)) {
    df <- cbind(df, make_lags(ex_list[[nm]], L_max, nm))
  }
  
  # Subset to regime rows and drop rows with any NA (from lagging)
  dfR <- df[idx, , drop = FALSE]
  dfR <- dfR[complete.cases(dfR), , drop = FALSE]
  
  # If too few rows, skip
  if (nrow(dfR) < 200) {
    warning("Too few rows in ", reg_name, " after lagging/NA drop: ", nrow(dfR))
    return(NULL)
  }
  
  # Full model, then AIC stepwise
  fit_full <- lm(y ~ ., data = dfR)
  fit_aic  <- step(fit_full, direction = "both", trace = 0)
  
  list(
    fit = fit_aic,
    n = nrow(dfR),
    aic = AIC(fit_aic),
    adj_r2 = summary(fit_aic)$adj.r.squared,
    terms = attr(terms(fit_aic), "term.labels")
  )
}

predict_tarx_on_test <- function(fits_by_regime,
                                 e_test_he,
                                 ex_test_all,
                                 reg_test,
                                 stations_by_regime,
                                 P_max = 12,
                                 L_max = 12) {
  
  y <- as.numeric(e_test_he)
  reg_test <- as.character(reg_test)
  
  # base df with HE lags
  df <- data.frame(y = y)
  df <- cbind(df, make_lags(e_test_he, P_max, "he"))
  
  # add all possible exog lags (we'll subset columns per regime)
  for (nm in names(ex_test_all)) {
    df <- cbind(df, make_lags(ex_test_all[[nm]], L_max, nm))
  }
  
  # prediction vector aligned to test series
  yhat_all <- rep(NA_real_, length(y))
  
  # store per-regime evaluation rows for plotting/scoring
  eval_rows <- list()
  
  for (R in names(stations_by_regime)) {
    fitR_obj <- fits_by_regime[[R]]
    if (is.null(fitR_obj) || is.null(fitR_obj$fit)) next
    
    idxR <- which(reg_test == R)
    if (length(idxR) == 0) next
    
    # pick the exact columns needed by the fitted model
    # model.frame(fit) contains the variables used
    needed <- names(model.frame(fitR_obj$fit))
    # needed includes "y" and selected regressors like "he_L1", "fa_L3", etc.
    
    dfR <- df[idxR, , drop = FALSE]
    dfR <- dfR[, intersect(names(dfR), needed), drop = FALSE]
    
    # must contain all needed columns
    miss <- setdiff(needed, names(dfR))
    if (length(miss) > 0) {
      warning("Regime ", R, ": missing columns in test design matrix: ", paste(miss, collapse=", "))
      next
    }
    
    # drop NA rows (lagging creates NAs)
    keep <- complete.cases(dfR)
    if (sum(keep) < 20) next
    
    dfR2 <- dfR[keep, , drop = FALSE]
    idxR2 <- idxR[keep]
    
    # predict
    pred <- as.numeric(predict(fitR_obj$fit, newdata = dfR2))
    yhat_all[idxR2] <- pred
    
    eval_rows[[R]] <- list(
      idx = idxR2,
      y_obs = y[idxR2],
      y_hat = pred
    )
  }
  
  list(yhat = yhat_all, eval_rows = eval_rows)
}


## Function for training, val, test
train_val_test_split <- function(y_all, frequency, start, end, test_len = 500) {
  N <- length(y_all)
  
  i_train_start <- start
  i_train_end   <- end
  
  i_test_start <- N - test_len + 1
  i_test_end   <- N
  
  i_val_start <- end + 1
  i_val_end   <- i_test_start - 1
  
  # ts times
  t <- function(i) 1 + (i - 1) / frequency
  
  list(
    # segments
    y_train = window(y_all, start = t(i_train_start), end = t(i_train_end)),
    y_val   = window(y_all, start = t(i_val_start),   end = t(i_val_end)),
    y_test  = window(y_all, start = t(i_test_start),  end = t(i_test_end)),
    
    # combined series for "run-forward" residuals (train+val and train+val+test)
    y_train_val      = window(y_all, start = t(i_train_start), end = t(i_val_end)),
    y_train_val_test = window(y_all, start = t(i_train_start), end = t(i_test_end)),
    
    # boundaries (ts-time)
    t_val_start  = t(i_val_start),
    t_val_end    = t(i_val_end),
    t_test_start = t(i_test_start),
    t_test_end   = t(i_test_end),
    
    # boundaries (row indices)
    i_val_start  = i_val_start,
    i_val_end    = i_val_end,
    i_test_start = i_test_start,
    i_test_end   = i_test_end
  )
}

## Helper for run forward + slice val/test errors
get_val_test_errors <- function(y_all_ts, fit_model, split_obj) {
  # Validation: run forward on train+val, then slice validation window
  fit_val  <- Arima(split_obj$y_train_val, model = fit_model)
  e_val_all <- residuals(fit_val)
  e_val <- window(e_val_all, start = split_obj$t_val_start, end = split_obj$t_val_end)
  
  # Test: run forward on train+val+test, then slice test window
  fit_test <- Arima(split_obj$y_train_val_test, model = fit_model)
  e_test_all <- residuals(fit_test)
  e_test <- window(e_test_all, start = split_obj$t_test_start, end = split_obj$t_test_end)
  
  list(
    fit_val = fit_val, e_val_all = e_val_all, e_val = e_val,
    fit_test = fit_test, e_test_all = e_test_all, e_test = e_test
  )
}

## Prediction with max(P_max, L_max) observations from validation to build regressor matrix
predict_tarx_on_test <- function(fits_by_regime,
                                 e_val_he,
                                 e_test_he,
                                 ex_val_all,
                                 ex_test_all,
                                 reg_test,
                                 stations_by_regime,
                                 P_max = 12,
                                 L_max = 12) {
  
  K <- max(P_max, L_max)
  
  # Coerce regime
  reg_test <- as.character(reg_test)
  
  # Build extended HE series: last K of validation + all test
  he_ext <- c(tail(as.numeric(e_val_he), K), as.numeric(e_test_he))
  y_ext  <- he_ext  # response is HE error itself
  
  # Extended regimes: NA for history (we don't evaluate/predict regimes there)
  reg_ext <- c(rep(NA_character_, K), reg_test)
  
  # Build extended exog series in the same way
  ex_ext_all <- list()
  for (nm in names(ex_test_all)) {
    if (is.null(ex_val_all[[nm]])) stop("ex_val_all missing series: ", nm)
    ex_ext_all[[nm]] <- c(tail(as.numeric(ex_val_all[[nm]]), K),
                          as.numeric(ex_test_all[[nm]]))
  }
  
  # Base df with HE lags on extended data
  df <- data.frame(y = y_ext)
  df <- cbind(df, make_lags(he_ext, P_max, "he"))
  
  # Add exogenous lags (extended)
  for (nm in names(ex_ext_all)) {
    df <- cbind(df, make_lags(ex_ext_all[[nm]], L_max, nm))
  }
  
  # We'll only fill predictions for test portion
  n_test <- length(e_test_he)
  test_idx_ext <- (K + 1):(K + n_test)
  
  yhat_test <- rep(NA_real_, n_test)
  eval_rows <- list()
  
  for (R in names(stations_by_regime)) {
    fitR_obj <- fits_by_regime[[R]]
    if (is.null(fitR_obj) || is.null(fitR_obj$fit)) next
    
    # Indices (within EXT) corresponding to test points in this regime
    idxR_ext <- test_idx_ext[ which(reg_test == R) ]
    if (length(idxR_ext) == 0) next
    
    needed <- names(model.frame(fitR_obj$fit))  # includes "y" + chosen regressors
    
    dfR <- df[idxR_ext, , drop = FALSE]
    dfR <- dfR[, intersect(names(dfR), needed), drop = FALSE]
    
    miss <- setdiff(needed, names(dfR))
    if (length(miss) > 0) {
      warning("Regime ", R, ": missing columns in test design matrix: ", paste(miss, collapse=", "))
      next
    }
    
    keep <- complete.cases(dfR)
    if (sum(keep) < 5) next
    
    dfR2 <- dfR[keep, , drop = FALSE]
    idxR2_ext <- idxR_ext[keep]
    
    pred <- as.numeric(predict(fitR_obj$fit, newdata = dfR2))
    
    # Map extended indices -> test indices (1..n_test)
    idxR2_test <- idxR2_ext - K
    yhat_test[idxR2_test] <- pred
    
    eval_rows[[R]] <- list(
      idx = idxR2_test,
      y_obs = as.numeric(e_test_he)[idxR2_test],
      y_hat = pred
    )
  }
  
  list(yhat = yhat_test, eval_rows = eval_rows)
}

