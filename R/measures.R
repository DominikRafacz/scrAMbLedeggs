accuracy <- function(label, prediction) {
  mean(label == prediction)
}

precision <- function(label, prediction) {
  sum(label == 1 & prediction == 1)/sum(prediction == 1)
}

recall <- function(label, prediction) {
  sum(label == 1 & prediction == 1)/sum(label == 1)
}

F_measure <- function(label, prediction, beta = 1) {
  prec_score <- precision(label, prediction)
  recall_score <- recall(label, prediction)
  (1 + beta^2) * prec_score * recall_score / (beta^2 * prec_score + recall_score)
}

log_likelihood <- function(label, probability) {
  sum((label - probability) ^ 2)
}
