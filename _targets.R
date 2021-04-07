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
  ),
  error = "continue"
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
  # Constants (modifiable, though) ----
  tar_target(num_iter, c(50, 150, 400, 1000)),
  tar_target(learning_rate, c(.001, .004, .01, .04, .1)),
  tar_target(algorithm_tbl, tibble::tibble(
    name = c("GD", "IRLS", "SGD", "KNN", "LDA", "QDA"),
    value = list(GD, IRLS, SGD, KNN, LDA, QDA),
    custom = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    lrn_rate = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )),
  tar_target(custom_algorithm_tbl, algorithm_tbl %>% filter(custom)),
  tar_target(lrn_rate_algorithm_tbl, algorithm_tbl %>% filter(lrn_rate)),
  tar_target(dataset_name, c("breast", "creditg", "wells", "amp", "twonorm")),
  tar_target(dataset_target_index, c(1, 21, 5, 10, 21)),
  tar_target(dataset_positive_class, c("M", "bad", "exploration", "YES", "1")),
  tar_target(dataset_X_processing, list(
    identity,
    function(X) {
      X %>%
        mutate(across(where(is.character), dummify)) %>%
        map_dfc(identity) %>%
        remove_correlated() %>%
        select(-`purpose_domestic appliance`, -purpose_repairs, -purpose_other, -purpose_retraining)
    },
    function(X) {
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
    function(X) {
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
        remove_correlated()
    },
    identity
  )),
  tar_target(dataset_y_processing, list(
    identity,
    identity,
    tolower,
    identity,
    identity
  )),
  
  # Data import and processing ----
  data_unscaled_target <- tar_target(
    data_unscaled,
    dataset(dataset_name,
            target_index = dataset_target_index,
            positive_class = dataset_positive_class,
            X_processing = dataset_X_processing[[1]],
            y_processing = dataset_y_processing[[1]]),
    pattern = map(dataset_name, dataset_target_index, dataset_positive_class,
                  dataset_X_processing, dataset_y_processing),
    iteration = "list"
  ),
  data_scaled_target <- tar_target(
    data_scaled,
    scale(data_unscaled),
    pattern = map(data_unscaled),
    iteration = "list"
  ),
  tar_combine(data_both, data_unscaled_target, data_scaled_target,
              command = vctrs::vec_c(!!!.x, .name_spec = "{inner}")),
  
  # Basic crossvalidation ----
  tar_target(CV, perform_CV(algorithm_tbl, data_both[[1]]),
             pattern = cross(algorithm_tbl, data_both)),
  aggregated_CV_target <- tar_target(aggregated_CV, CV %>%
               select(-fold) %>%
               group_by(data, algorithm, scaled) %>%
               summarise(across(everything(), mean), .groups = "drop"),
             pattern = map(CV)),
  tar_combine(bound_aggregates, aggregated_CV_target),
  
  # CV results visualized ----
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
  
  # Convergence analysis ----
  tar_target(CV_conv, perform_CV(custom_algorithm_tbl, data_both[[1]], max_iter = num_iter),
             pattern = cross(custom_algorithm_tbl, data_both, num_iter)),
  aggregated_CV_conv_target <- tar_target(aggregated_CV_conv, CV_conv %>%
                                       select(-fold) %>%
                                       group_by(data, algorithm, scaled, max_iter) %>%
                                       summarise(across(everything(), mean), .groups = "drop"),
                                     pattern = map(CV_conv)),
  tar_combine(conv_aggregates, aggregated_CV_conv_target),
  
  tar_target(CV_conv_plot,
             ggplot(conv_aggregates %>%
                      filter(scaled),
                    aes(x = max_iter, y = log_likelihood, color = algorithm)) +
               geom_line(size = 1.6) +
               facet_wrap(~data) +
               ggtitle("Comparison of log-likelihood for different max iter values") +
               theme_bw()),
  
  # Learning rate exploration ----
  tar_target(CV_lr, perform_CV(lrn_rate_algorithm_tbl, data_both[[1]], learning_rate = learning_rate),
             pattern = cross(lrn_rate_algorithm_tbl, data_both, learning_rate)),
  aggregated_CV_lr_target <- tar_target(aggregated_CV_lr, CV_lr %>%
                                          select(-fold) %>%
                                          group_by(data, algorithm, scaled, learning_rate) %>%
                                          summarise(across(everything(), mean), .groups = "drop"),
                                        pattern = map(CV_lr)),
  tar_combine(lr_aggregates, aggregated_CV_lr_target),
  
  tar_target(CV_lr_plot,
             ggplot(lr_aggregates %>%
                      filter(scaled),
                    aes(x = learning_rate, y = log_likelihood, color = algorithm)) +
               geom_line(size = 1.6) +
               facet_wrap(~data) +
               scale_x_log10() +
               ggtitle("Comparison of log-likelihood for different learning rate values") +
               theme_bw()),

  # Report generation ----
  tar_render(report, "report/report.Rmd")
)
