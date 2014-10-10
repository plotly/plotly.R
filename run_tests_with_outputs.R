library(testthat)

setwd('tests')

save_outputs <- function(gg, name, ignore_ggplot=FALSE) {
  print(paste("running", name))
  py <- plotly("TestBot", "r1neazxo9w")
  u <- py$ggplotly(gg, kwargs=list(filename=paste0("ggplot2/", name),
                                   fileopt="overwrite", auto_open=FALSE))
  plotlyUrl <- u$response$url
  writeLines(plotlyUrl, paste0("test-ggplot-", name, ".url"))
  pngdata <- getURLContent(paste0(u$response$url, ".png"))
  writeBin(as.raw(pngdata), paste0("test-ggplot-", name, ".png"))
  if (!ignore_ggplot) {
    ggsave(paste0("test-ggplot-", name, "-orig.png"), plot=gg, w=7, h=5)
  }

  # save the json
  writeLines(getURL(paste0(plotlyUrl, ".json")), paste0("test-ggplot-", name, ".json"))
}

test_check("plotly")
