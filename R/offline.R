#' Plotly Offline
#' 
#' Create a plotly visualization that doesn't require an external plotly server.
#' 
#' @param p a plotly object
#' @param height A valid CSS unit. (like "100\%", "600px", "auto") or a number, 
#' which will be coerced to a string and have "px" appended.
#' @param width A valid CSS unit. (like "100\%", "600px", "auto") or a number, 
#' which will be coerced to a string and have "px" appended.
#' @param out_dir a directory to place the visualization. 
#' If \code{NULL}, a temporary directory is used when the offline object is printed.
#' @param open_browser open the visualization after creating it?
#' @author Carson Sievert
#' @return a plotly object of class "offline"
#' @references \url{http://purchasing.plot.ly/}
#' @export
#' @examples \dontrun{
#' # If you purchased plotly offline, you should've received a private link
#' # to the source files. You may use this code to install them
#' link <- "private link" # put the link you received here
#' tmp <- tempfile(fileext = ".zip")
#' js_dir <- "~/.plotly/plotlyjs"
#' download.file(link, tmp)
#' if (!dir.exists(js_dir)) dir.create(js_dir, recursive = TRUE)
#' unzip(tmp, exdir = js_dir)
#' unlink(tmp)
#' 
#' # now start making offline plots!
#' p <- plot_ly(data = iris, x = Sepal.Width, y = Sepal.Length, color = Species,
#'   mode = "markers")
#' offline(p)
#' 
#' # works with ggplot2
#' gg <- qplot(data = iris, x = Sepal.Width, y = Sepal.Length, color = Species)
#' offline(gg)
#' 
#' # also works with shiny/rmarkdown
#' shiny::runApp(system.file("examples/UN_Simple", package = "plotly"))
#' }
#' 

offline <- function(p = last_plot(), height = 400, width = "100%", 
                    out_dir = NULL, open_browser = interactive()) {
  haz <- has_offline()
  if (!haz) offline_stop()
  p <- plotly_build(p)
  structure(
    list(
      data = to_JSON(p$data),
      layout = to_JSON(p$layout),
      id = digest::digest(p),
      height = if (is.numeric(height)) paste0(height, "px") else height,
      width = if (is.numeric(width)) paste0(width, "px") else width,
      out_dir = out_dir,
      viewer = if (open_browser) get_browser()
    ),
    class = "offline"
  )
}

new_offline <- function(data, layout, height, width, id) {
  sprintf(
    '<div class="%s loading" style="color: rgb(50,50,50);">Drawing...</div><div id="%s" style="height: %s; width: %s;" ></div><script type="text/javascript">Plotly.plot("%s", %s, %s).then(function() {$(".%s.loading").remove();})</script>', 
    id, id, height, width, id, data, layout, id
  )
}

has_offline <- function() {
  off <- Sys.getenv("plotly_offline")
  bundles <- c("plotly-matlab-offline-bundle.js", 
               "plotly-ipython-offline-bundle.js")
  haz <- all(bundles %in% list.files(off))
  # if bundles don't exist and a zip does try to unzip
  if (!haz) {
    zipf <- paste0(off, ".zip")
    if (file.exists(zipf)) {
      dir.create(off, showWarnings = FALSE, recursive = TRUE)
      unzip(zipf, exdir = off)
      haz <- all(bundles %in% list.files(off))
    }
  }
  # return the path as well as T/F 
  setNames(haz, off)
}

offline_stop <- function() {
  stop("Required source files are not located under ", 
       Sys.getenv("plotly_offline"), "\n\n",
       "If you have Plotly Offline, and those files are located under a different \n",
       "directory, you can change the default search path: \n ",
       " Sys.setenv('plotly_offline' = '/path/to/plotlyjs') \n", 
       "If you don't have Plotly Offline, you may purchase it here: \n", 
       " http://purchasing.plot.ly \n", 
       "If you have any questions, please contact team@plot.ly", call. = FALSE)
}

offline_bundle <- function(jq = FALSE) {
  haz <- has_offline()
  if (!haz) offline_stop()
  # ipython already has jQuery, and so does shiny
  f <- if (jq) "plotly-matlab-offline-bundle.js" else "plotly-ipython-offline-bundle.js"
  file.path(names(haz), f)
}

get_browser <- function() {
  # Try to view an 'embedded' version in RStudio preview. This was
  # copied/adapted from Yihui Xie's work on servr --
  # https://github.com/yihui/servr/blob/39a61972e278adc5bbd49a74c68de858bb2c144f/R/utils.R#L55-L69
  browseR = if ('tools:rstudio' %in% search()) getOption('viewer') else {
    if (Sys.getenv('RSTUDIO') == '1') getFromNamespace('viewer', 'rstudioapi')
  }
  # rstudioapi::viewer() does not seem to work when a separate R session is
  # launched from RStudio, so we need to try() and if it fails, fall back to the
  # default web browser
  if (is.null(browseR) || !is.function(browseR) ||
      inherits(try(browseR('http://www.rstudio.com'), silent = TRUE), 'try-error'))
    browseR = getOption("browser")
  browseR
}
