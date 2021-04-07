IRLS <- function(X, y, max_iter = 100, min_iter = 10, min_improvement = 0) {
  X <- cbind(1, as.matrix(X))
  n <- length(y)
  y <- matrix(y, nrow = n)
  W <- diag(nrow = n)
  p <- rep(0.5, n)
  beta <- matrix(0, nrow = ncol(X))

  for (i in seq_len(max_iter)) {
    inverse <-  tryCatch(solve(t(X) %*% diag(as.vector(p * (1 - p))) %*% X),
                         error = function(e) "singular")
    if (length(inverse) == 1 && inverse == "singular") {
      if (i <= min_iter) {
        inverse <-  tryCatch(solve(t(X) %*% diag(as.vector(p * (1 - p))) %*% X + diag(1e-7, nrow = ncol(X))),
                             error = function(e) "singular")
        if (length(inverse) == 1 && inverse == "singular") break
      } else break
    }

    beta_diff <- inverse %*% t(X) %*% (y - p)
    beta <- beta + beta_diff
    p <- 1 / (1 + exp(-X %*% beta))
    
    if (all(abs(beta_diff) <= min_improvement)) break
  }

  structure(list(beta = beta),
            parameters = list(max_iter = max_iter),
            class = c("IRLS", "scrAMbodeL"))
}
