predValuesInputR3 <- function(inputdata, kStep){
  # Function for predicting input for R3 - Roskilde lufthavn
  x <-  inputdata
  k <-  kStep
  
  #Series: df_x[[value_col]] 
  #ARIMA(2,1,3) 
  
  #Coefficients:
  #  ar1      ar2      ma1     ma2      ma3
  #1.8755  -0.9404  -1.7951  0.8856  -0.0700
  #s.e.  0.0315   0.0322   0.0693  0.0710   0.0218
  
  #sigma^2 = 0.8327:  log likelihood = -3579.67
  #AIC=7171.35   AICc=7171.38   BIC=7206.75
  
  #Training set error measures:
  #                     ME     RMSE       MAE        MPE     MAPE      MASE        ACF1
  #Training set 0.004418726 0.911513 0.6636694 -0.3064596 4.483401 0.7984416 0.004088757
  
  # Kalman filter
  N <- length(x)
  startInd <- 40
  noPar <- 4
  init_theta <- c(0.6093, -0.2361, 0.0938,  0.0688) # ar1 ma1, ma2, ma3
  w <- c(NA_real_, diff(x)) # differenced series
  
  xt <- matrix(NA_real_, nrow = noPar, ncol = N) # parameter estimates over time
  xt[ , startInd-1] <- init_theta   # initial estimates of states for ar1 ma1 ma2 ma3 
  
  A <- diag(noPar)
  Rw <- 0.8238 # measurement noise 
  Re <- diag(noPar) * 1e-5 # system noise
  P <- diag(noPar) * 1e-5 # initial covariance matrix
  
  e <- rep(0, N) # estimated one-step prediction error
  what <- rep(NA_real_, N) # predicted w_t|t-1
  xhat1 <- rep(NA_real_, N) # predicted x_t|t-1 (levels
  xhatk <- matrix(NA_real_, N, k) # predicted x_{t+1..t+k}|t (levels)
  
  for(t in startInd:N){
    # Update the predicted state and the time-varying state vector.
    theta_pred <-  A %*% xt[ ,t-1, drop = FALSE]                         # x_{t|t-1} = A x_{t-1|t-1}
    C <- matrix(c(w[t-1], e[t-1], e[t-2], e[t-3]), nrow=1)
    
    w_hat <-  drop(C %*% theta_pred)
    what[t] <- w_hat
    xhat1[t] <- x[t-1] + w_hat
    
    # innovation from w_t
    e_t <- w[t] - w_hat
    e[t] <- e_t
    
    # --- Kalman update of parameters ---
    S  <- drop(C %*% P %*% t(C)) + Rw         # scalar
    Kt <- (P %*% t(C)) / S                    # 4x1
    
    theta_upd <- drop(theta_pred) + drop(Kt) * e_t
    xt[, t] <- theta_upd
    
    # --- Covariance update + time update ---
    P_upd <- P - Kt %*% (S * t(Kt))
    P <- A %*% P_upd %*% t(A) + Re
    
    # --- k-step forecast in levels from time t ---
    # Use UPDATED parameters at time t
    phi <- xt[1, t]
    th1 <- xt[2, t]; th2 <- xt[3, t]; th3 <- xt[4, t]
    
    # Forecast differences w_{t+k|t} with future errors set to 0 in expectation
    w_fc <- numeric(kStep)
    
    # k=1 uses w_t, e_t, e_{t-1}, e_{t-2}
    w_fc[1] <- phi * w[t] + th1 * e[t] + th2 * e[t-1] + th3 * e[t-2]
    
    if (kStep >= 2) {
      # k=2: e_{t+1}=0, keep remaining known innovations where applicable
      w_fc[2] <- phi * w_fc[1] + th2 * e[t] + th3 * e[t-1]
    }
    if (kStep >= 3) {
      w_fc[3] <- phi * w_fc[2] + th3 * e[t]
    }
    if (kStep >= 4) {
      for (h in 4:kStep) w_fc[h] <- phi * w_fc[h - 1]
    }
    
    # Convert to level forecasts: x_{t+h} = x_t + sum_{j=1..h} w_fc[j]
    xhatk[t, ] <- x[t] + cumsum(w_fc)
  }
  list(
    what = what,     # w_t|t-1
    xhat1 = xhat1,   # x_t|t-1
    xhatk = xhatk,   # x_{t+1..t+k}|t
    theta = xt,      # time-varying params
    innov = e        # innovations on w
  )
}
