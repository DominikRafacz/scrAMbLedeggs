dummify <- function(column) {
  name <- as_label(enquo(column))
  values <- unique(column)
  if (length(values) == 2) values <- values[1]
  map_df(set_names(values, paste0(name, "_", values)), function(value) as.integer(column == value))
}

remove_correlated <- function(dataset) {
  corr_matrix <- cor(dataset)
  corr_matrix[!lower.tri(corr_matrix)] <- 0
  corr_matrix <- abs(corr_matrix)

  dataset[,!apply(corr_matrix, 2, function(col) any(col > 0.7))]
}
