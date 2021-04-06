IRLS <- function(X, y, max_iter = 100, min_iter = 10) {
  X <- cbind(1, as.matrix(X))
  n <- length(y)
  y <- matrix(y, nrow = n)
  W <- diag(nrow = n)
  p <- rep(0.5, n)
  beta <- matrix(0, nrow = ncol(X))

  for (i in seq_len(max_iter)) {
    inverse <-  tryCatch(solve(t(X) %*% diag(as.vector(p * (1 - p))) %*% X),
                         error = function(e) "singular")
    if (inverse == "singular") {
      if (i <= min_iter) {
        inverse <-  tryCatch(solve(t(X) %*% diag(as.vector(p * (1 - p))) %*% X + diag(1e-7, nrow = ncol(X))),
                             error = function(e) "singular")
        if (inverse == "singular") break
      } else break
    }

    beta <- beta + inverse %*% t(X) %*% (y - p)
    p <- 1 / (1 + exp(-X %*% beta))
  }

  structure(list(beta = beta),
            parameters = list(max_iter = max_iter),
            class = c("IRLS", "scrAMbodeL"))
}
