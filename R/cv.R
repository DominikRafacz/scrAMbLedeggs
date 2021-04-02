perform_CV <- function(algorithm, X, y, num_folds = 5, ...) {
  n <- nrow(X)
  inds <- sample(1:n)
  inds <- map(1:num_folds, function(ind) inds[seq(from = ind, to = n, by = num_folds)])

  predictions <- map(inds, function(fold_ind) list(X = X[fold_ind, ], y = y[fold_ind])) %>%
    map(function(fold) algorithm(fold$X, fold$y, ...)) %>%
    map2(1:num_folds, function(model, ind) predict(model, X[-inds[[ind]],]))

  map_measure <- function(measure) function(ind) measure(y[-inds[[ind]]], predictions[[ind]]$prediction)

  tibble(fold = 1:num_folds) %>%
    mutate(accuracy = map_dbl(fold, map_measure(accuracy)),
           precision = map_dbl(fold, map_measure(precision)),
           recall = map_dbl(fold, map_measure(recall)),
           F_measure = map_dbl(fold, map_measure(F_measure)))
}
