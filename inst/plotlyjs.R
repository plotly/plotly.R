library(httr)
# download latest GitHub release
x <- GET('https://api.github.com/repos/plotly/plotly.js/releases/latest')
zip <- content(x)$zipball_url
# for a particular version:
# zip <- "https://github.com/plotly/plotly.js/archive/v1.25.0.zip"
tmp <- tempfile(fileext = ".zip")
download.file(zip, tmp)
unzip(tmp)
# update the plotly.js bundle
p <- Sys.glob("*plotly.js*/dist/plotly.min.js")
file.copy(p, "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js", overwrite = T)
l <- Sys.glob("*plotly.js*/LICENSE")
file.copy(l, "inst/htmlwidgets/lib/plotlyjs/LICENSE", overwrite = T)
unlink("*plotly.js*", recursive = T)
message("Manually update plotly.yaml with this version")
basename(zip)


# download latest build from master 
#download.file(
#  "https://raw.githubusercontent.com/plotly/plotly.js/master/dist/plotly.min.js", 
#  destfile = "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js"
#)


#' Update plotly's internal traces information
#' 
#' Note this script requires RSelenium and phantomjs and shouldn't be
#' run other than package maintainers
#' 
#' 
#' @param path a path pointing to the source of plotly's R package

update_attributes <- function(path = ".") {
  if (!is.na(Sys.getenv("RSTUDIO", NA))) {
    stop("This function must be called outside of RStudio")
  }
  devtools::load_all(path)
  pjs <- RSelenium::phantom()
  on.exit(pjs$stop(), add = TRUE)
  on.exit(unlink("index.html"), add = TRUE)
  remDr <- RSelenium::remoteDriver(browserName = "phantomjs")
  Sys.sleep(5)
  remDr$open(silent = TRUE)
  htmlwidgets::saveWidget(plotly::plot_ly(), "index.html")
  remDr$navigate(file.path(getwd(), "index.html"))
  Schema <- remDr$executeScript("return Plotly.PlotSchema.get()")
  devtools::use_data(Schema, overwrite = TRUE, internal = TRUE)
}

update_attributes()

# For some reason this fails as of plotly.js v1.17.0, probably because
# Plotly is a global anymore. Anyway, see plotly.js for another hack to 
# get at the schema.
