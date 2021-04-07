library(targets)
library(tarchetypes)

source("R/GD.R")
source("R/SGD.R")
source("R/IRLS.R")
source("R/measures.R")
source("R/adapters.R")
source("R/dataset.R")
source("R/predict.R")
source("R/cv.R")
source("R/transform.R")
source("R/null_model.R")

tar_option_set(
  packages = c(
    "biogram",
    "e1071",
    "fs",
    "ggplot2",
    "icecream",
    "knitr",
    "MASS",
    "purrr",
    "readr",
    "rlang",
    "tibble",
    "tidyr",
    "dplyr"
    )
)

library(rlang)
library(tidyr)

# imported functions -- to keep track what are their origins
# dplyr::bind_rows
# dplyr::mutate
# dplyr::select
# dplyr::pull
# ggplot2::ggplot
# purrr::map_dfc
# rlang::exec
# rlang::syms
# tidyr::expand_grid
# tidyr::pivot_longer

dataset_names <- c("breast", "creditg", "wells", "amp", "twonorm")
scaled_names <- c("unscaled", "scaled")
algorithm_names <- c("GD", "IRLS", "SGD", "KNN", "LDA", "QDA")

paste_dataset_names <- function() {
  rep(paste0(
    rep(dataset_names, each = length(scaled_names)), "_data_", scaled_names),
    times = length(algorithm_names))
}

set.seed(2137)
list(
  #data loading and preparation
  ##breast
  tar_target(breast_data_unscaled,
             dataset("breast",
                     target_index = 1, positive_class = "M")),
  tar_target(breast_data_scaled, scale(breast_data_unscaled)),
  
  ##creditg
  tar_target(creditg_data_unscaled,
             dataset("creditg", path = "data/credit-g.csv",
                     target_index = 21, positive_class = "bad",
                     X_processing = function(X) {
                       X %>%
                         mutate(across(where(is.character), dummify)) %>%
                         map_dfc(identity) %>%
                         remove_correlated() %>%
                         select(-`purpose_domestic appliance`, -purpose_repairs, -purpose_other, -purpose_retraining)
                     })),
  tar_target(creditg_data_scaled, scale(creditg_data_unscaled)),

  ##wells
  tar_target(wells_data_unscaled,
             dataset("wells",
                     target_index = 5, positive_class = "exploration",
                     X_processing = function(X) {
                       X %>%
                         select(!ends_with(c("name", "choke", "id", "no", "date", "op", "age")), -Comments) %>%
                         mutate(across(where(is.numeric), ~replace(.x, is.na(.x), median(.x, na.rm = FALSE)))) %>%
                         select(where(~!anyNA(.x))) %>%
                         select(-Rig_Datum) %>%
                         mutate(across(where(is.character), dummify)) %>%
                         map_dfc(identity) %>%
                         remove_correlated() %>%
                         select(-Mech_Stat_AB1, -Mech_Stat_Completed, -`Mech_Stat_Completed (Shut In)`, -`Mech_Stat_Plugged`, -Mech_Stat_AB2)
                     },
                     y_processing = function(y) {
                       tolower(y)
                     })),
  tar_target(wells_data_scaled, scale(wells_data_unscaled)),

  ##amp
  tar_target(amp_data_unscaled,
             dataset("amp",
                     target_index = 10, positive_class = "YES",
                     X_processing = function(X) {
                       X %>%
                         pull(Sequence) %>%
                         strsplit("") %>%
                         map(function(sequence) {
                           count_ngrams(sequence, n = 2, u = LETTERS) %>%
                             as.matrix() %>%
                             as_tibble()
                         }) %>%
                         bind_rows() %>%
                         select(where(~any(.x != 0))) %>%
                         select(-c(7, 21, 32, 40, 41, 49, 57, 61, 62, 70, 74, 82, 90, 91, 101,
                                   124, 125, 126, 127, 129, 132, 135, 136, 140, 141, 148, 155, 168,
                                   172, 188, 200, 202, 203, 205, 206, 207, 208, 209, 210, 211, 212,
                                   213, 214, 215, 217, 218, 219, 220, 221, 224, 225, 232, 235, 240,
                                   245, 246, 248, 252, 260, 266, 268, 272, 278, 280, 291, 292, 293,
                                   294, 295, 296, 298, 300, 303, 309, 310, 321, 329, 342, 343, 362,
                                   365, 366, 367, 370, 373, 374, 375, 376, 377, 378, 379, 380, 381,
                                   382, 383, 384, 385, 386, 387, 393, 397)
                                )
                     })),
  tar_target(amp_data_scaled, scale(amp_data_unscaled)),

  ##twonorm
  tar_target(twonorm_data_unscaled,
             dataset("twonorm",
                     target_index = 21, positive_class = "1")),
  tar_target(twonorm_data_scaled, scale(twonorm_data_unscaled)),

  #crossvalidating algorithms
  tar_map(
    list(
      algorithm_name = rlang::syms(rep(algorithm_names, each = length(dataset_names) * length(scaled_names))),
      dataset = rlang::syms(paste_dataset_names())),
    names = c("algorithm_name", "dataset"),
    tar_target(CV, perform_CV(algorithm_name, dataset)),
    tar_target(aggregated_CV, CV %>%
                 select(-fold) %>%
                 group_by(data, algorithm, scaled) %>%
                 summarise(across(everything(), mean), .groups = "drop"))
  ),
  
  ##aggregating results
  tar_target(bound_aggregates, bind_rows(!!!rlang::syms(rlang::exec(
    paste0, !!!tidyr::expand_grid("aggregated_CV_", algorithm_names, "_", dataset_names, "_data_", scaled_names))))),
  
  ##visualizing results
  tar_target(CV_plot_scaled,
             ggplot(bound_aggregates %>%
                      filter(scaled) %>%
                      tidyr::pivot_longer(cols = c(accuracy, precision, recall, F_measure)),
                    aes(x = data, y = value, group = algorithm, fill = algorithm)) +
               geom_bar(stat = "identity", position = "dodge") +
               facet_wrap(~name, ) +
               ggtitle("Comparison of measures for algorithms and datasets") +
               theme_bw()),
  tar_target(CV_plot_unscaled,
             ggplot(bound_aggregates %>%
                      filter(!scaled) %>%
                      tidyr::pivot_longer(cols = c(accuracy, precision, recall, F_measure)),
                    aes(x = data, y = value, group = algorithm, fill = algorithm)) +
               geom_bar(stat = "identity", position = "dodge") +
               facet_wrap(~name, ) +
               ggtitle("Comparison of measures for algorithms and datasets") +
               theme_bw()),
  
  tar_target(CV_R2_plot_scaled,
             ggplot(bound_aggregates %>%
                      filter(scaled),
                    aes(x = data, y = R2, group = algorithm, fill = algorithm)) +
               geom_bar(stat = "identity", position = "dodge") +
               ggtitle("Comparison of R2 for algorithms and datasets") +
               theme_bw()),
  tar_target(CV_R2_plot_unscaled,
             ggplot(bound_aggregates %>%
                      filter(!scaled),
                    aes(x = data, y = R2, group = algorithm, fill = algorithm)) +
               geom_bar(stat = "identity", position = "dodge") +
               ggtitle("Comparison of R2 for algorithms and datasets") +
               theme_bw()),

  tar_render(report, "report/report.Rmd")
)
