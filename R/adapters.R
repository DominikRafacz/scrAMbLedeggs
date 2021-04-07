KNN <- function(X, y, k = 5, ...) {
  structure(list(wrapped_model = gknn(x = X, y = factor(y, levels = c("TRUE", "FALSE")), k, ...)),
            class = c("KNN", "adapted model", "scrAMbodeL"))
}

LDA <- function(X, y, ...) {
  structure(list(wrapped_model = lda(y ~ ., cbind(X, y = factor(y, levels = c("TRUE", "FALSE"))))),
            class = c("LDA", "adapted model", "scrAMbodeL"))
}

QDA <- function(X, y, ...) {
  structure(list(wrapped_model = lda(y ~ ., cbind(X, y = factor(y, levels = c("TRUE", "FALSE"))))),
            class = c("QDA", "adapted model", "scrAMbodeL"))
}

predict.KNN <- function(object, newdata, ...) {
  prediction <- as.numeric(predict(object$wrapped_model, newdata, type = "class", ...) == "TRUE")
  probability <- predict(object$wrapped_model, newdata, type = "prob", ...)[, "TRUE"]
  data.frame(prediction, probability)
}

predict.LDA <- function(object, newdata, ...) {
  ret <- predict(object$wrapped_model, newdata, ...)
  data.frame(prediction = as.numeric(ret$class == "TRUE"), ret$posterior)
}

predict.QDA <- function(object, newdata, ...) {
  ret <- predict(object$wrapped_model, newdata, ...)
  data.frame(prediction = as.numeric(ret$class == "TRUE"), ret$posterior)
}
