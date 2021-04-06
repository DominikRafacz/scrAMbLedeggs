library(targets)
library(tarchetypes)

source("R/GD.R")
source("R/SGD.R")
source("R/IRLS.R")
source("R/measures.R")
source("R/dataset.R")
source("R/predict.R")
source("R/cv.R")
source("R/transform.R")

tar_option_set(
  packages = c(
    "icecream",
    "ggplot2",
    "tibble",
    "rlang",
    "purrr",
    "dplyr",
    "readr",
    "tidyr"
  )
)

dataset_names <- c("breast", "creditg")
algorithm_names <- c("GD", "IRLS", "SGD")

set.seed(2137)
list(
  #breast
  tar_target(breast_data,
             dataset("breast",
                     target_index = 1, positive_class = "M")),

  #creditg
  tar_target(creditg_data,
             dataset("creditg", path = "data/credit-g.csv",
                     target_index = 21, positive_class = "bad",
                     X_processing = function(X) {
                       X %>%
                         mutate(across(where(is.character), dummify)) %>%
                         map_dfc(identity)
                     })),

  tar_map(
    list(
      algorithm = rlang::syms(rep(algorithm_names, each = length(dataset_names))),
      dataset = rlang::syms(rep(paste0(dataset_names, "_data"), times = length(algorithm_names)))),
    tar_target(CV, perform_CV(algorithm, dataset))
    ),

  tar_map(
    list(CV = rlang::syms(rlang::exec(paste0, !!!tidyr::expand_grid("CV_", algorithm_names, "_", dataset_names, "_data")))),
    tar_target(
      aggregated,
      CV %>%
        select(-fold) %>%
        group_by(dataset, algorithm) %>%
        summarise(across(everything(), mean), .groups = "drop"))
    ),

  tar_target(bound_aggregates, bind_rows(!!!rlang::syms(rlang::exec(paste0, !!!tidyr::expand_grid("aggregated_CV_", algorithm_names, "_", dataset_names, "_data")))))

  )
