#' Acquire (and optionally display) plotly's plot schema
#' 
#' The schema contains valid attributes names, their value type, 
#' default values (if any), and min/max values (if applicable).
#' 
#' @param jsonedit use `listviewer::jsonedit` to view the JSON?
#' @param ... other options passed onto `listviewer::jsonedit`
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
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
