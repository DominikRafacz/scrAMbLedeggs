IRLS <- function(X, y) {
  X <- cbind(1, as.matrix(X))
  n <- length(y)
  y <- matrix(y, nrow = n)
  W <- diag(nrow = n)
  p <- rep(0.5, n)
  beta <- matrix(0, nrow = ncol(X))

  for (i in 1:1000) {
    beta <- beta + solve(t(X) %*% diag(as.vector(p * (1 - p))) %*% X) %*% t(X) %*% (y - p)
    p <- 1 / (1 + exp(-X %*% beta))
  }

  beta
}
