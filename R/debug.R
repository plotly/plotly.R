#' Obtain JSON sent to plotly.js
#' 
#' This function returns the JSON sent to plotly.js which can be useful for 
#' debugging.
#' 
#' @param p a plotly or ggplot object.
#' @param ... other options passed onto \code{listviewer::jsonedit}
#' @export
#' @examples 
#'   
#' plotly_spec(plot_ly())

plotly_spec <- function(p = plot_ly(), ...) {
  if (system.file(package = "listviewer") == "") {
    stop("This function requires the listviewer package:\n", 
         "install.packages('listviewer')", call. = FALSE)
  }
  listviewer::jsonedit(to_JSON(plotly_build(p)$x), mode = "form", ...)
}


#' Display plot schema
#' 
#' @export
#' @examples 
#'   
#' schema()

schema <- function() {
  listviewer::jsonedit(Schema, mode = "form")
}

#' Show information about attribute(s) of a given trace type
#' 
#' @export
#' @param type a trace type
#' @param attrs attributes for the race type
#' @examples 
#' 
#' explain_attrs()
explain_attrs <- function(type = "scatter", attrs = c("x", "y")) {
  jsonlite::toJSON(verify_attrs(type, attrs), pretty = TRUE)
}
