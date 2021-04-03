library(targets)
library(tarchetypes)

source("R/GD.R")
source("R/SGD.R")
source("R/IRLS.R")
source("R/measures.R")
source("R/predict.R")
source("R/cv.R")
source("R/transofrm.R")

tar_option_set(
  packages = c(
    "icecream",
    "tibble",
    "rlang",
    "purrr",
    "dplyr",
    "readr"
  )
)

dataset_names <- c("breast", "creditg")
algorithm_names <- c("GD", "IRLS", "SGD")

set.seed(2137)
list(
  #breast
  tar_target(dataset_raw_file_breast, "data/breast.csv", format = "file"),
  tar_target(dataset_raw_breast, read_csv(dataset_raw_file_breast)),
  tar_target(X_breast, dataset_raw_breast[, -1]),
  tar_target(y_breast, dataset_raw_breast[, 1] == "M"),

  #creditg
  tar_target(dataset_raw_file_creditg, "data/credit-g.csv", format = "file"),
  tar_target(dataset_raw_creditg, read_csv(dataset_raw_file_creditg)),
  tar_target(X_creditg, dataset_raw_creditg[, -21] %>%
               mutate(across(where(is.character), dummify)) %>%
               map_dfc(identity)),
  tar_target(y_creditg, dataset_raw_creditg[, 21] == "bad"),

  tar_map(list(
    algorithm = rlang::syms(rep(algorithm_names, each = length(dataset_names))),
            X = rlang::syms(rep(paste0("X_", dataset_names), times = length(algorithm_names))),
            y = rlang::syms(rep(paste0("y_", dataset_names), times = length(algorithm_names)))),
    tar_target(CV, perform_CV(algorithm, X, y)))
)
