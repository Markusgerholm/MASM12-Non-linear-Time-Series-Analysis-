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

plot_ccf_hourly_lags <- function(df_he,
                                 df_x,
                                 regimes = c("R1","R2","R3","R4","R5","R6","R7","R8"),
                                 time_col = "ts_utc",
                                 value_col = "Lufttemperatur",
                                 lag_max = 50) {
  
  x_name <- deparse(substitute(df_x))
  
  # --- Prewhiten x on FULL x-series ---
  fit_x <- auto.arima(df_x[[value_col]])
  df_x <- df_x %>% mutate(res_x = as.numeric(residuals(fit_x)))
  
  # --- Build an hourly grid (no interpolation; just a time index) ---
  t_min <- max(min(df_he[[time_col]], na.rm = TRUE),
               min(df_x[[time_col]],  na.rm = TRUE))
  t_max <- min(max(df_he[[time_col]], na.rm = TRUE),
               max(df_x[[time_col]],  na.rm = TRUE))
  
  grid <- tibble(!!time_col := seq(from = t_min, to = t_max, by = "1 hour"))
  
  # --- Join onto the grid to enforce hourly spacing ---
  # (If some hours are missing in either df, they'll be NA — that's OK.)
  he_g <- grid %>%
    left_join(df_he %>% select(all_of(time_col), res_he = res, regime),
              by = time_col) %>%
    arrange(.data[[time_col]])
  
  x_g <- grid %>%
    left_join(df_x %>% select(all_of(time_col), res_x),
              by = time_col) %>%
    arrange(.data[[time_col]])
  
  # --- Loop regimes with masking ---
  for (reg in regimes) {
    
    # Keep full hourly series, but only keep he residuals when regime is active
    y_masked <- ifelse(he_g$regime == reg, he_g$res_he, NA_real_)
    
    r <- ccf(x_g$res_x,
             y_masked,
             lag.max = lag_max,
             na.action = na.pass,
             plot = FALSE)
    
    pos <- r$lag >= 0
    
    # Effective n varies by lag because of NA masking and missing hours.
    # Use a conservative CI based on "available paired obs at lag 0".
    n0 <- sum(complete.cases(x_g$res_x, y_masked))
    ci <- if (n0 > 0) 1.96 / sqrt(n0) else NA_real_
    
    plot(r$lag[pos], r$acf[pos], type = "h",
         xlab = "Lag (hours)",
         ylab = "CCF",
         main = paste("CCF –", reg, "(", x_name, " prewhitened; hourly grid)"))
    
    abline(h = 0)
    if (!is.na(ci)) abline(h = c(-ci, ci), lty = 2)
  }
  
  invisible(list(fit_x = fit_x))
}