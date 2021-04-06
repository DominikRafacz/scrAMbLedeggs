SGD <- function(X, y, max_iter = 10, learning_rate = .01) {
  X <- cbind(1, as.matrix(X))
  n <- length(y)
  beta <- matrix(0, nrow = ncol(X))
  
  for (i in seq_len(max_iter)) {
    # reshuffle data before each iteration
    new_obs_order <- sample.int(n)
    X <- X[new_obs_order, ]
    y <- y[new_obs_order]
    for (j in seq_along(y)) {
      # first we compute only that one prediction we use
      # as.numeric converts 1x1 matrix to scalar
      prediction <- as.numeric(1 / (1 + exp(-X[j, ] %*% beta)))
      first_derivative <- X[j, ] * (y[j] - prediction)
      beta <- beta - learning_rate * first_derivative
    }
  }
  
  structure(list(beta = beta),
            parameters = list(max_iter = max_iter,
                              learning_rate = learning_rate),
            class = c("SGD", "scrAMbodeL"))
}
