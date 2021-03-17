cluster_se <- function(X,res,cluster) {
  M <- length(unique(cluster)) # number of groups (villages)
  X <- as.matrix(cbind(1,X))
  N <- nrow(X) # number of observations
  K <- ncol(X) # number of regressors including intercept
  dof <- (M/(M-1))*(N/(N-K)) # correction term for degrees of freedom
  inner_term <- Reduce(
    "+",
    lapply(
      unique(cluster),
      function(k) {
        row_idx <- which(cluster==k)
        if (length(row_idx)>1) {
          inner_term <- crossprod(t(crossprod(X[row_idx,], res[row_idx])))
        } else {
          inner_term <- crossprod(t(crossprod(t(matrix(X[row_idx,])), res[row_idx])))
        }
        return(inner_term)
      }
    )
  )
  vcov <- dof * qr.solve(crossprod(X)) %*% inner_term %*% qr.solve(crossprod(X))
  return(vcov)
}
