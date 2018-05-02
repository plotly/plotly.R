#' Inspect JSON sent to plotly.js
#' 
#' This function is useful for obtaining/viewing/debugging JSON 
#' sent to plotly.js.
#' 
#' @param p a plotly or ggplot object.
#' @param jsonedit use [listviewer::jsonedit] to view the JSON?
#' @param pretty adds indentation whitespace to JSON output. Can be TRUE/FALSE
#' or a number specifying the number of spaces to indent. See [jsonlite::prettify].
#' @param ... other options passed onto [listviewer::jsonedit]
#' @export
#' @examples 
#'   
#' plotly_json(plot_ly())
#' plotly_json(plot_ly(), FALSE)

plotly_json <- function(p = last_plot(), jsonedit = interactive(), pretty = TRUE, ...) {
  plotlyJSON <- to_JSON(plotly_build(p)$x, pretty = pretty)
  if (jsonedit) {
    try_library("listviewer", "plotly_json")
    listviewer::jsonedit(plotlyJSON, mode = "form", ...)
  } else {
    plotlyJSON
  }
}

#' Acquire (and optionally display) plotly's plot schema
#' 
#' The schema contains valid attributes names, their value type, 
#' default values (if any), and min/max values (if applicable).
#' 
#' @param jsonedit use `listviewer::jsonedit` to view the JSON?
#' @param ... other options passed onto `listviewer::jsonedit`
#' @export
#' @examples 
#' s <- schema()
#' 
#' # retrieve acceptable `layout.mapbox.style` values
#' if (!is.na(Sys.getenv('MAPBOX_TOKEN', NA))) {
#'   styles <- s$layout$layoutAttributes$mapbox$style$values
#'   subplot(
#'     plot_mapbox() %>% layout(mapbox = list(style = styles[3])),
#'     plot_mapbox() %>% layout(mapbox = list(style = styles[5]))
#'   )
#' }
#' 
#' 
#' 

schema <- function(jsonedit = interactive(), ...) {
  
  if (jsonedit) {
    try_library("listviewer", "schema")
    print(listviewer::jsonedit(Schema, mode = "form"))
  }
  
  invisible(Schema)
}
