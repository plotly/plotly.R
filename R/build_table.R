#' Build a ggplot2/plotly comparison table
#' 
#' This function is mainly for development purposes, but users might also find
#' it useful to see the capabilities/limitations of \link{ggplotly}
#'
#' @details If \code{package_version} is used to specify a version,
#' this will try to install the corresponding CRAN version via
#' \code{devtools::install_version}. Otherwise, versions are passed onto
#' the \code{repo} argument in \code{devtools::install_github}.
#'
#' @param p1 New version of plotly.
#' @param gg1 The ggplot2 dependency for \code{p1}. If not specified, the currently
#' installed version is used as a dependency.
#' @param p2 Old version of plotly (should be master).
#' @param gg2 The ggplot2 dependency for \code{p2}.
#' @param width width of thumbnails (in pixels)
#' @param height height of thumbnails (in pixels)
#' @export
#' @examples \dontrun{
#' # by default, use latest CRAN versions?
#' build_table()
#' 
#' # compare #269 (with dev of version of ggplot2) versus plotly master with ggplot2 1.0.1 from CRAN
#' build_table(
#'   "ropensci/plotly#269",
#'   "hadley/ggplot2",
#'   gg2 = package_version("1.0.1")
#' )
#' 
#' # idea a pipeable syntax for building a table?
#' init_table(width, height, ...) %>%
#'   add_column("ropensci/plotly")
#' 
#' 
#' }

# TODO: smarter installing?
build_table <- function(p1, gg1, p2 = "ropensci/plotly", gg2, 
                        width = 400, height = 300, ...) {
  Sys.setenv("SAVE_PNGS" = "TRUE")
  Sys.setenv("PNG_WIDTH" = width)
  Sys.setenv("PNG_HEIGHT" = height)
  
  if (basename(getwd()) != "plotly") 
    stop("basename of working directory must be plotly")
  
  # we _always_ build the 'old' version
  if (missing(gg2)) {
    message(
      "Did not specify a ggplot2 dependency for 'old' version \n",
      "Using currently installed version: ", gg2 <- packageVersion("ggplot2")
    )
  } else {
    install_package("ggplot2", gg2)
  }
  #install_package("plotly", p2)
  # we need some way of finding the pngs created during tests
  tmpDir <- tempdir()
  Sys.setenv("PLOTLY_PREFIX" = file.path(tmpDir, "plotly1-"))
  Sys.setenv("GGPLOT_PREFIX" = file.path(tmpDir, "gglotly1-"))
  run_tests()
  
  imgfy <- function(pat) {
    sprintf(
      "<img src='%s' width='%s' height='%s' />",
      dir(path = tmpDir, pattern = pat), width, height
    )
  }
  imgs <- data.frame(
    imgfy("^gglotly1-"),
    imgfy("^plotly1-")
  )
  imgs <- cbind(
    sub("\\.png", "", sub("^[a-z]+1-", "", dir(path = tmpDir, pattern = "^plotly1-"))),
    imgs
  )
  imgs <- setNames(imgs, c("name", as.character(gg2), as.character(p2)))
  
  # if the 'new' version of plotly is specified, build against it!
  if (!missing(p1)) {
    if (missing(gg1)) {
      message(
        "Did not specify a ggplot2 dependency for 'new' version \n",
        "Using currently installed version: ", gg1 <- packageVersion("ggplot2")
      )
    } else {
      install_package("ggplot2", gg1)
    }
    install_package("plotly", p1)
    Sys.setenv("PLOTLY_PREFIX" = file.path(tmpDir, "plotly2-"))
    Sys.setenv("GGPLOT_PREFIX" = file.path(tmpDir, "gglotly2-"))
    run_tests()
    if (!identical(gg1, gg2)) imgs[[as.character(gg1)]] <- imgfy("^gglotly2-")
    imgs[[as.character(p1)]] <- imgfy("^plotly2-")
  }
  # now build the table!
  
  html <- sprintf(
    '<!DOCTYPE html>
    <html>
     <head>
      <meta charset=\"utf-8\"/>
      <style type=\"text/css\" media=\"screen\"> table td tr { border:1px solid #FF0000;} </style>
     </head>
     <body>
      %s
     </body>
    </html>', as.character(knitr::kable(imgs, format = "html", escape = FALSE)))
  tbl <- file.path(tmpDir, "index.html")
  writeLines(html, tbl)
  browseURL(tbl)
  invisible(tbl)
}

run_tests <- function() {
  owd <- setwd("tests")
  on.exit(setwd(owd))
  try(source("testthat.R"))
}

install_package <- function(pkg, x) {
  if (is.package_version(x)) {
    cmd <- sprintf(
      "Rscript -e 'devtools::install_version(\"%s\", \"%s\", type = \"source\")'",
      pkg, x
    )
  } else {
    cmd <- sprintf("Rscript -e 'devtools::install_github(\"%s\")'", x)
  }
  res <- system(cmd, wait = TRUE)
}
