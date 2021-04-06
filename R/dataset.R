dataset <- function(name,
                    path = paste0("data/", name, ".csv"),
                    target_index = 1,
                    positive_class,
                    X_processing = identity,
                    y_processing = identity) {
  data <- read_csv(path)

  X <- X_processing(data[, -target_index])
  y <- y_processing(pull(data, target_index) == positive_class)

  structure(list(X = X, y = y),
            name = name,
            class = c("dataset", "list"))
}
