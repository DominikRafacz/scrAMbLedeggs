predict.scrAMbodeL <- function(object, newdata, ...) {
  X <- cbind(1, as.matrix(newdata))
  probability <- 1 / (1 + exp(-X %*% object$beta))
  prediction <- round(probability)
  data.frame(prediction, probability)
}
