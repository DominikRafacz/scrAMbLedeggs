dataset <- function(name,
                    path = paste0("data/", name, ".csv"),
                    target_index = 1,
                    positive_class,
                    X_processing = identity,
                    y_processing = identity) {
  data <- read_csv(path, na = c("n/a", ""))

  X <- X_processing(data[, -target_index])
  y <- y_processing(pull(data, target_index)) == positive_class

  structure(list(X = X, y = y),
            name = name,
            scaled = FALSE,
            class = c("dataset", "list"))
}

scale.dataset <- function(x, center = TRUE, scale = TRUE) {
  x[["X"]] <- x[["X"]] %>%
    mutate(across(.fns = ~(.x - mean(.x))/sd(.x)))
  attr(x, "scaled") <- TRUE
  x
}
