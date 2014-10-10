library(testthat)

save_outputs <- function(gg, name) {
  py <- plotly("TestBot", "r1neazxo9w")
  u <- py$ggplotly(gg, kwargs=list(filename=paste0("ggplot2/", name),
                                   fileopt="overwrite", auto_open=FALSE))
  writeLines(u$response$url, paste0("test-ggplot-", name, ".url"))
  pngdata <- getURLContent(paste0(u$response$url, ".png"))
  writeBin(as.raw(pngdata), paste0("test-ggplot-", name, ".png"))
  ggsave(paste0("test-ggplot-", name, "-orig.png"), plot=gg, w=7, h=5)
}

test_check("plotly")
