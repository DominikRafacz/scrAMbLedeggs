null_model <- function(dataset) {
  structure(list(beta = rbind(mean(dataset$y),
                              matrix(0, nrow = ncol(dataset$X)))),
            parameters = list(),
            class = c("null_model", "scrAMbodeL"))
}
