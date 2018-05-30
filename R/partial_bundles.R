#' Use a partial bundle of plotly.js
#'
#'
#' @param p a plotly object.
#' @param type name of the (partial) bundle. The default `'auto'` attempts to 
#' @param version version number (e.g. 1.30.0). See [here](https://github.com/plotly/plotly.js/releases)
#' for a list of valid versions and changelogs.
#' @param local either "local", "cdn", or "rawgit".
#' @param minified whether or not to use a minified js file (non-minified file can be useful for debugging plotly.js)
#' @export
#' @author Carson Sievert
#' @references <https://github.com/plotly/plotly.js/blob/master/dist/README.md>
#' @seealso `[plotly_bundle_info]()`
#'
#' @examples
#'
#' library(plotly)
#' p1 <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
#' p2 <- partial_bundle(p1)
#' f1 <- tempfile(fileext = ".html")
#' f2 <- tempfile(fileext = ".html")
#'
#' save_widget <- function(p, f) {
#'   owd <- setwd(dirname(f))
#'   on.exit(setwd(owd))
#'   htmlwidgets::saveWidget(p, f)
#'   mb <- round(file.info(f)$size / 1e6, 3)
#'   message("File is: ", mb," MB")
#' }
#' save_widget(p1, f1)
#' save_widget(p2, f2)
#'


# TODO: implement type = 'auto' which would attempts to find the smallest partial bundle that can render `p`.
# this would require, however, knowing the bundle -> trace mapping *for `p`'s plotly.js version*
partial_bundle <- function(p, type = "auto", local = TRUE, minified = TRUE) {
  
  if (!is.plotly(p)) stop("The first argument to `partial_bundle()` must be a plotly object", call. = FALSE)
  
  # Amongst all the 'print-time' htmlwidget dependencies, 
  # find the plotly.js dependency and attach some meta-info
  idx <- plotlyjsBundleIDX(p)
  p$dependencies[[idx]]$local <- local
  p$dependencies[[idx]]$minified <- minified
  p$dependencies[[idx]]$partial_bundle <- match.arg(type, c("auto", "main", names(bundleTraceMap)))
  
  plotly_build(p)
}

verify_partial_bundle <- function(p) {
  # return early if we're using the main bundle (the default)
  currentBundle <- plotlyjsBundle(p)
  bundleType <- currentBundle$partial_bundle %||% "main"
  if (identical(bundleType, "main")) return(p)
  
  # grab all the required trace types
  types <- unique(vapply(p$x$data, function(x) x[["type"]] %||% "scatter", character(1)))
  
  if (identical(bundleType, "auto")) {
    
    # resolve an auto bundle by using the 1st bundle that supports all the types
    # (ordering of bundleTraceMap is important!)
    for (i in seq_along(bundleTraceMap)) {
      if (all(types %in% bundleTraceMap[[i]])) {
        bundleType <- names(bundleTraceMap)[[i]]
        break
      }
    }
    
    if (identical(bundleType, "auto")) {
      message(
        "Couldn't find a single partial bundle that would support this plotly",
        "visualization. Using the main (full) bundle instead."
      )
      p$dependencies[[plotlyjsBundleIDX(p)]] <- plotlyMainBundle()
      return(p)
    }

  }
  
  # verify that this partial bundle actually supports this viz
  # (at this point, bundleType should never be 'auto' or 'main')
  missingTypes <- setdiff(types, bundleTraceMap[[bundleType]])
  if (length(missingTypes)) {
    msg <- sprint(
      "The '%s' bundle supports the following trace types: '%s'.\n\n",
      "This plotly visualization contains the following trace types: '%s'",
      bundle, paste(missingTypes, collapse = "', '"), paste(missingTypes, collapse = "', '")
    )
    stop(msg, call. = FALSE)
  }
  
  idx <- plotlyjsBundleIDX(p)
  bundle_name <- sprintf("plotly-%s", bundleType)
  bundle_script <- sprintf(
    "plotly-%s-%s.%sjs", 
    bundleType, currentBundle$version, 
    if (isTRUE(currentBundle$minified)) "min." else ""
  )
  
  p$dependencies[[idx]]$name <- bundle_name
  p$dependencies[[idx]]$script <- bundle_script
  p$dependencies[[idx]]$src <- list(href = "https://cdn.plot.ly")
  
  # download the relevant bundle
  if (isTRUE(p$dependencies[[idx]]$local)) {
    # TODO: implement a caching mechanism?
    try_library("curl", "partial_bundle")
    tmpfile <- file.path(tempdir(), bundle_script)
    p$dependencies[[idx]]$src$file <- dirname(tmpfile)
    if (!file.exists(tmpfile)) {
      curl::curl_download(paste0("https://cdn.plot.ly/", bundle_script), tmpfile)
    }
  }
  
  p
}

plotlyjsBundleIDX <- function(p) {
  depNames <- sapply(p$dependencies, "[[", "name")
  bundleNames <- paste0("plotly-", c("main", "auto", names(bundleTraceMap)))
  idx <- which(depNames %in% bundleNames)
  if (length(idx) != 1) stop("Couldn't find the plotlyjs bundle")
  idx
}

plotlyjsBundle <- function(p) {
  p$dependencies[[plotlyjsBundleIDX(p)]]
}


# TODO: create this object from the dist/README.md
bundleTraceMap <- list(
  basic = c(
    "scatter",
    "bar",
    "pie"
  ),
  cartesian = c(
    "scatter",
    "bar",
    "pie",
    "box",
    "heatmap",
    "histogram",
    "histogram2d",
    "histogram2dcontour",
    "contour",
    "scatterternary",
    "violin"
  ),
  geo = c(
    "scatter",
    "scattergeo",
    "choropleth"
  ),
  gl3d = c(
    "scatter",
    "scatter3d",
    "surface",
    "mesh3d",
    "cone"
  ),
  gl2d = c(
    "scatter",
    "scattergl",
    "splom",
    "pointcloud",
    "heatmapgl",
    "contourgl",
    "parcoords"
  ),
  mapbox = c(
    "scatter",
    "scattermapbox"
  ),
  finance = c(
    "scatter",
    "bar",
    "pie",
    "histogram",
    "ohlc",
    "candlestick"
  )
)



##' List trace types supported by a particular bundle
##'
##' @export
#partial_bundle_info <- function() {
#  for (i in seq_along(bundle_traces)) {
#    bundle_name <- names(bundle_traces)[[i]]
#    msg <- sprintf(
#      "The '%s' bundle size is %s MB and contains the '%s' traces",
#      bundle_name,
#      round(file.info(bundle_file(bundle_name))$size / 1000000, 3),
#      paste(bundle_traces[[i]], collapse = "', '")
#    )
#    message(msg)
#  }
#}
#
#bundle_file <- function(bundle = "basic") {
#  bundle <- match.arg(bundle, names(bundle_traces))
#  system.file("lib", paste0("plotly-", bundle, ".min.js"), package = "plotlyDepends")
#}
