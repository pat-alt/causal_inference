gmm <- function(X,Z,y,const=TRUE) {
  y <- as.matrix(y)
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  if (const) {
    if (!all(X[,1]==1)) {
      X <- cbind(1,X)
    }
    if (!all(Z[,1]==1)) {
      Z <- cbind(1,Z)
    }
  }
  P_z <- Z %*% qr.solve(crossprod(Z)) %*% t(Z) # projection matrix
  beta <- qr.solve(t(X)%*%P_z%*%X,t(X)%*%P_z%*%y)
  return(beta)
}
