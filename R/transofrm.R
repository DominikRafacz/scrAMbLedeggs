dummify <- function(x) map(levels(x), function(y) as.integer(x == y)) %>% as_tibble
