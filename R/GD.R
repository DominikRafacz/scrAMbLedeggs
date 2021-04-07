GD <- function(X, y, max_iter = 1000, learning_rate = .01, min_improvement = 0) {
  X <- cbind(1, as.matrix(X))
  n <- length(y)
  y <- matrix(y, nrow = n)
  p <- rep(0.5, n)
  beta <- matrix(0, nrow = ncol(X))
  
  for (i in seq_len(max_iter)) {
    first_derivative <- t(X) %*% (y - p)
    # optimal learning rate could be computed for each iteration separately
    beta_diff <- learning_rate * first_derivative
    beta <- beta + beta_diff
    p <- 1 / (1 + exp(-X %*% beta))
    
    if (all(abs(beta_diff) <= min_improvement)) break
  }
  
  structure(list(beta = beta),
            parameters = list(max_iter = max_iter,
                              learning_rate = learning_rate),
            class = c("GD", "scrAMbodeL"))
}
