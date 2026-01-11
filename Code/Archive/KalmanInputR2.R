predValuesInputR2 <- function(inputdata, kStep){
  # Function for predicting input for R2 - Köbenhavn lufthavn
  x <-  inputdata

  #Series: df_x[[value_col]] 
  #ARIMA(5,1,1) 
  
  #Coefficients:
  #  ar1      ar2      ar3      ar4      ar5      ma1
  #1.0833  -0.0149  -0.0128  -0.0742  -0.1386  -0.9527
  #s.e.  0.0201   0.0283   0.0283   0.0283   0.0196   0.0070
  
  #sigma^2 = 0.5802:  log likelihood = -3094.06
  #AIC=6202.12   AICc=6202.16   BIC=6243.43
  
  #Training set error measures:
                    #  ME      RMSE       MAE        MPE     MAPE      MASE        ACF1
  #Training set 0.003499426 0.7607213 0.5696531 -0.1638029 3.497381 0.8258147 -0.01543314
  
  # Kalman filter
  N <- length(x)
  startInd <- 40
  noPar <- 6
  init_theta <- c(1.0833, -0.0149, -0.0128, -0.0742, -0.1386, -0.9527) # ar1 ar2, ar3, ar4, ar5, ma1
  w <- c(NA_real_, diff(x)) # differenced series
  
  xt <- matrix(NA_real_, nrow = noPar, ncol = N) # parameter estimates over time
  xt[ , startInd-1] <- init_theta   # initial estimates of states for ar1 ma1 ma2 ma3 
  
  A <- diag(noPar)
  Rw <- 0.5802 # measurement noise 
  Re <- diag(noPar) * 1e-5 # system noise
  P <- diag(noPar) * 1e-5 # initial covariance matrix
  
  e <- rep(0, N) # estimated one-step prediction error
  what <- rep(NA_real_, N) # predicted w_t|t-1
  xhat1 <- rep(NA_real_, N) # predicted x_t|t-1 (levels
  xhatk <- matrix(NA_real_, N, k) # predicted x_{t+1..t+k}|t (levels)
  
  for(t in startInd:N){
    # Update the predicted state and the time-varying state vector.
    theta_pred <-  A %*% xt[ ,t-1, drop = FALSE]                         # x_{t|t-1} = A x_{t-1|t-1}
    C <- matrix(c(w[t-1], w[t-2], w[t-3], w[t-4], w[t-5], e[t-1]), nrow=1)
    
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
    
    # ---- k-step forecast of LEVELS from time t ----
    phi <- xt[1:5, t]       # phi1..phi5
    th1 <- xt[6, t]       # theta1
    
    w_fc <- numeric(kStep)
    
    # Keep last 5 w values: [w_t, w_{t-1}, ..., w_{t-4}]
    last_w <- c(w[t], w[t-1], w[t-2], w[t-3], w[t-4])
    
    for (h in 1:kStep) {
      if (h == 1) {
        # 1-step: AR part + MA(1)*e_t
        w_fc[h] <- sum(phi * last_w) + th1 * e[t]
      } else {
        # h>=2: future innovations have E[e]=0, so pure AR recursion
        w_fc[h] <- sum(phi * last_w)
      }
      
      # Update rolling window: new w becomes most recent
      last_w <- c(w_fc[h], last_w[1:(p-1)])
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
