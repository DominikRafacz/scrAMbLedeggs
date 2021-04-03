dummify <- function(column) {
  name <- as_label(enquo(column))
  values <- unique(column)
  map_df(set_names(values, paste0(name, "_", values)), function(value) as.integer(column == value))
}
