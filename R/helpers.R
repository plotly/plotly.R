#' Obtain JSON sent to plotly.js
#' 
#' This function returns the JSON sent to plotly.js which can be useful for 
#' debugging.
#' 
#' @param p a plotly or ggplot object.
#' @param as in what format should the spec be printed?
#' @param ... arguments passed onto \code{jsonlite::toJSON}
#' @export
#' @examples 
#'   
#' plotly_spec(plot_ly(), as = "json", pretty = TRUE)

plotly_spec <- function(p = plot_ly(), as = c("plain", "json"), ...) {
  spec <- plotly_build(p)$x
  if (identical("json", match.arg(as))) {
    return(to_JSON(spec, ...))
  }
  spec
}


#' Convert trace types to WebGL
#' 
#' This function will also build
#' 
#' @param p a plotly or ggplot object.
#' @param warn should a warning be produced if the trace type doesn't have 
#' a WebGL equivalent?
#' @export
#' @examples 
#' 
#' # currently no bargl trace type
#' toWebGL(qplot(1:10))
#' toWebGL(qplot(1:10, 1:10))
#' 
toWebGL <- function(p, warn = TRUE) {
  p <- plotly_build(p)
  types <- sapply(p$x$data, function(x) x[["type"]][1] %||% "scatter")
  idx <- paste0(types, "gl") %in% traces
  if (any(!idx) && warn) {
    warning(
      "The following traces don't have a WebGL equivalent: ",
      paste(which(!idx), collapse = ", ")
    )
  }
  for (i in which(idx)) {
    p$x$data[[i]]$type <- paste0(p$x$data[[i]]$type, "gl")
  }
  p
}


#' Create a complete empty plotly graph.
#' 
#' Useful when used with \link{subplot}
#' 
#' @export
plotly_empty <- function() {
  eaxis <- list(
    showticklabels = FALSE,
    showgrid = FALSE,
    zeroline = FALSE
  )
  layout(plot_ly(), xaxis = eaxis, yaxis = eaxis)
}
