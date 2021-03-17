fgls <- function(X,y,index) {
  # Parameters
  index_vars <- colnames(index)
  K <- ncol(X)
  N <- length(unique(index)) # dimension of cross-section 
  N_x_T <- length(y)
  T_ <- round(N_x_T/N)
  # Compute WG residuals
  mod <- within_groups(X,y,index)
  v_hat <- mod$residuals
  sigma_v <- crossprod(v_hat)/(N*(T_-1)-K)
  # Compute BG residuals 
  mod <- between_groups(X,y,index)
  u_hat <- mod$residuals
  sigma_u <- crossprod(u_hat)/(N-K)
  # Theta differencing
  sigma_eta <- sigma_u - (1/T_) * sigma_v
  theta <- as.numeric(sqrt(sigma_v / (sigma_v + T_ * sigma_eta)))
  dt <- data.table(y,X,index)
  dt <- dt[,lapply(.SD,function(i) i-(1-theta)*mean(i)),by=index_vars] 
  cols = colnames(dt)[!colnames(dt) %in% colnames(index)]
  dt <- dt[,.SD,.SDcols=cols]
  mod <- lm(y~.,dt)
}