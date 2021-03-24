GD <- function(X, y) {
  X <- cbind(1, as.matrix(X))
  n <- length(y)
  y <- matrix(y, nrow = n)
  p <- rep(0.5, n)
  beta <- matrix(0, nrow = ncol(X))
  
  for (i in 1:1000) {
    first_derivative <- t(X) %*% (y - p)
    # optimal learning rate can be computed for each iteration separately
    learning_rate <- .01
    beta <- beta + learning_rate * first_derivative
    p <- 1 / (1 + exp(-X %*% beta))
  }
  
  beta
}
