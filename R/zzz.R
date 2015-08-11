.onLoad <- function(libname, pkgname) {
  if ("viridis" %in% rownames(installed.packages())) {
    require("viridis", quietly = TRUE)
  } else{
    install.packages("viridis", type = "source")
  }
}