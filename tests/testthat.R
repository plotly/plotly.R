library(testthat)
save_outputs <- function(gg, name, ignore_ggplot=FALSE) {
  print(paste("running", name))
}
test_check("plotly", filter = "histogram")
