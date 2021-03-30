perform_CV <- function(algorithm, X, y, num_folds = 5, ...) {
  n <- nrow(X)
  inds <- sample(1:n)
  inds <- lapply(1:num_folds,
                 function(ind) inds[seq(from = ind, to = n, by = num_folds)])
  folds <- lapply(inds,
                  function(fold_ind) list(X = X[fold_ind, ],
                                          y = y[fold_ind]))
  models <- lapply(folds,
                   function(fold) algorithm(fold$X, fold$y, ...))
}
