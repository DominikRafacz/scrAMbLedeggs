perform_null_model <- function(X, y) {
  structure(list(beta = rbind(mean(y),
                              matrix(0, nrow = ncol(X)))),
            parameters = list(),
            class = c("null_model", "scrAMbodeL"))
}
