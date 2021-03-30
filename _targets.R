library(targets)

source("R/GD.R")

tar_option_set(
  packages = c(
    "dplyr",
    "readr"
  )
)

list(
  tar_target(dataset_1_raw_file, "data/breast.csv", format = "file"),
  tar_target(dataset_1_raw, read_csv(dataset_1_raw_file)),
  tar_target(X_1, dataset_1_raw[, -1]),
  tar_target(y_1, dataset_1_raw[, 1] == "M"),
  tar_target(model_1, GD(X_1, y_1))
)
