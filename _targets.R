library(targets)
library(tarchetypes)

source("R/GD.R")
source("R/IRLS.R")
source("R/measures.R")
source("R/predict.R")
source("R/cv.R")

tar_option_set(
  packages = c(
    "icecream",
    "rlang",
    "purrr",
    "dplyr",
    "readr"
  )
)

set.seed(2137)
list(
  tar_target(dataset_1_raw_file, "data/breast.csv", format = "file"),
  tar_target(dataset_1_raw, read_csv(dataset_1_raw_file)),
  tar_target(X_1, dataset_1_raw[, -1]),
  tar_target(y_1, dataset_1_raw[, 1] == "M"),
  tar_map(list(algorithm = rlang::syms(c("GD", "IRLS"))),
          tar_target(CV, perform_CV(algorithm, X_1, y_1)))
)
