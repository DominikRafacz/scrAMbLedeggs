perform_CV <- function(algorithm, dataset, num_folds = 5, ...) {
  algorithm_name <- algorithm[["name"]]
  algorithm <- algorithm[["value"]][[1]]
  X <- dataset[["X"]]
  y <- dataset[["y"]]
  n <- nrow(X)
  inds <- sample(1:n)
  inds <- map(1:num_folds, function(ind) inds[seq(from = ind, to = n, by = num_folds)])
  
  predictions <- map(inds, function(fold_ind) list(X = X[fold_ind, ], y = y[fold_ind])) %>%
    map(function(fold) algorithm(fold$X, fold$y, ...)) %>%
    map2(1:num_folds, function(model, ind) predict(model, X[-inds[[ind]],]))
  
  map_measure <- function(measure) function(ind) measure(y[-inds[[ind]]], predictions[[ind]]$prediction)
  map_measure_prob <- function(measure) function(ind) measure(y[-inds[[ind]]], predictions[[ind]]$probability)
  
  # null model
  null_predictions <- map(inds, function(fold_ind) list(X = X[fold_ind, ], y = y[fold_ind])) %>%
    map(function(fold) perform_null_model(fold$X, fold$y)) %>%
    map2(1:num_folds, function(model, ind) predict(model, X[-inds[[ind]],]))
  
  null_log_likelihood <- map_dbl(1:num_folds, function(ind) log_likelihood(y[-inds[[ind]]], null_predictions[[ind]]$probability))

  # TODO: add ... to tibble?
  tibble(fold = 1:num_folds) %>%
    mutate(data = attr(dataset, "name"),
           scaled = attr(dataset, "scaled"),
           algorithm = algorithm_name,
           accuracy = map_dbl(fold, map_measure(accuracy)),
           precision = map_dbl(fold, map_measure(precision)),
           recall = map_dbl(fold, map_measure(recall)),
           F_measure = map_dbl(fold, map_measure(F_measure)),
           log_likelihood = map_dbl(fold, map_measure_prob(log_likelihood)),
           null_log_likelihood = null_log_likelihood,
           R2 = 1 - log_likelihood / null_log_likelihood)
}
