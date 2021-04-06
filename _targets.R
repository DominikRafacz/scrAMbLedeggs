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
source("R/null_model.R")

tar_option_set(
  packages = c(
    "icecream",
    "ggplot2",
    "tibble",
    "rlang",
    "purrr",
    "dplyr",
    "readr",
    "tidyr",
    "biogram"
  )
)

dataset_names <- c("breast", "creditg", "wells", "amp", "twonorm")
scaled_names <- c("unscaled", "scaled")
algorithm_names <- c("GD", "IRLS", "SGD")

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
                         remove_correlated()
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
                         mutate(across(where(is.character), dummify)) %>%
                         map_dfc(identity)
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
                         select(where(~any(.x != 0)))
                     })),
  tar_target(amp_data_scaled, scale(amp_data_unscaled)),
  
  ##twonorm
  tar_target(twonorm_data_unscaled,
             dataset("twonorm",
                     target_index = 21, positive_class = "1")),
  tar_target(twonorm_data_scaled, scale(twonorm_data_unscaled)),
  
  #null models
  tar_map(
    list(dataset = rlang::syms(paste0(
      rep(dataset_names, each = length(scaled_names)), "_data_", scaled_names))),
    tar_target(null_model, null_model(dataset)),
    tar_target(null_log_likelihood, log_likelihood(null_model))
  ),
  
  #crossvalidating algorithms
  tar_map(
    list(
      algorithm = rlang::syms(rep(algorithm_names, each = length(dataset_names) * length(scaled_names))),
      dataset = rlang::syms(rep(paste0(
        rep(dataset_names, each = length(scaled_names)), "_data_", scaled_names),
        times = length(algorithm_names)))),
    tar_target(CV, perform_CV(algorithm, dataset)),
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
               theme_bw())
)
