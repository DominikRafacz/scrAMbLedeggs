required_packages <- c("targets",
                       "dplyr")

for (package in required_packages) {
  if (!require(package, quietly = TRUE, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
