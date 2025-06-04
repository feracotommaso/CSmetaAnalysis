i2fun <- function(res) {
  if (is.null(res$vi)) stop("The 'res' object must contain sampling variances (vi).")
  if (is.null(res$sigma2)) stop("The 'res' object must contain estimated variance components (sigma2).")
  
  W <- diag(1 / res$vi)
  X <- model.matrix(res)
  
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  pct <- 100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k - res$p) / sum(diag(P)))
  
  return(pct)
}
