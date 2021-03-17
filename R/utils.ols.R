ols <- function(X, y, weights=NULL, const=TRUE) {
  y <- as.matrix(y)
  X <- as.matrix(X)
  if (const) {
    if (!all(X[,1]==1)) {
      X <- cbind(1,X)
    }
  }
  if (!is.null(weights)) {
    Phi <- diag(weights)
    beta <- qr.solve(t(X) %*% Phi %*% X, t(X) %*% Phi %*% y)
  } else {
    beta <- qr.solve(X,y)
  }
  return(beta)
}
