#' Use a partial bundle of plotly.js
#'
#' Leveraging plotly.js' partial bundles can lead to smaller file sizes 
#' and faster rendering. The full list of available bundles, and the
#' trace types that they support, are available 
#' [here](https://github.com/plotly/plotly.js/blob/master/dist/README.md#partial-bundles) 
#' 
#' @details WARNING: use this function with caution when rendering multiple 
#' plotly graphs on a single website. That's because, if multiple plotly.js 
#' bundles are used, the most recent bundle will override the other bundles.
#' See the examples section for an example.
#'
#' @param p a plotly object.
#' @param type name of the (partial) bundle. The default, `'auto'`, attempts to  
#' find the smallest single bundle that can render `p`. If no single partial bundle
#' can render `p`, then the full bundle is used.
#' @param local whether or not to download the partial bundle so that it can be
#' viewed later without an internet connection.
#' @param minified whether or not to use a minified js file (non-minified file can be useful for debugging plotly.js)
#' @author Carson Sievert
#' @export
#' @examples
#' 
#' # ----------------------------------------------------------------------
#' # This function is always safe to use when rendering a single 
#' # plotly graph. In this case, we get a 3x file reduction.
#' # ----------------------------------------------------------------------
#'
#' library(plotly)
#' p <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
#' save_widget <- function(p, f) {
#'   owd <- setwd(dirname(f))
#'   on.exit(setwd(owd))
#'   htmlwidgets::saveWidget(p, f)
#'   mb <- round(file.info(f)$size / 1e6, 3)
#'   message("File is: ", mb," MB")
#' }
#' f1 <- tempfile(fileext = ".html")
#' f2 <- tempfile(fileext = ".html")
#' save_widget(p, f1)
#' save_widget(partial_bundle(p), f2)
#' 
#' # ----------------------------------------------------------------------
#' # But, since plotly.js bundles override one another, 
#' # be careful when putting multiple graphs in a larger document!
#' # Note how the surface (part of the gl3d bundle) renders, but the 
#' # heatmap (part of the cartesian bundle) doesn't...
#' # ----------------------------------------------------------------------
#' 
#' \dontrun{
#' library(htmltools)
#' p1 <- plot_ly(z = ~volcano) %>% 
#'   add_heatmap() %>%
#'   partial_bundle()
#' p2 <- plot_ly(z = ~volcano) %>% 
#'   add_surface() %>%
#'   partial_bundle()
#' browsable(tagList(p1, p2))
#' }
#'


partial_bundle <- function(p, type = "auto", local = TRUE, minified = TRUE) {
  
  if (!is.plotly(p)) stop("The first argument to `partial_bundle()` must be a plotly object", call. = FALSE)
  
  # Amongst all the 'print-time' htmlwidget dependencies, 
  # find the plotly.js dependency and attach some meta-info
  idx <- plotlyjsBundleIDX(p)
  p$dependencies[[idx]]$local <- local
  p$dependencies[[idx]]$minified <- minified
  p$dependencies[[idx]]$partial_bundle <- match.arg(type, c("auto", "main", names(bundleTraceMap)))
  
  # unfortunately, htmlwidgets doesn't appear to support manipulation of 
  # dependencies field during preRenderHook(), so we need to explicitly build
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
      warning(
        "Couldn't find a single partial bundle that would support this plotly ",
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
    msg <- sprintf(
      "The '%s' bundle supports the following trace types: '%s'.\n\n This plotly visualization contains the following trace types: '%s'",
      bundleType, paste(bundleTraceMap[[bundleType]], collapse = "', '"), paste(missingTypes, collapse = "', '")
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

# TODO: create this object in inst/plotlyjs.R from the dist/README.md
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
