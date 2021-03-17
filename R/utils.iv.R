iv <- function(X,Z,y,const=TRUE) {
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
  beta <- qr.solve(crossprod(Z,X),crossprod(Z,y))
  return(beta)
}
