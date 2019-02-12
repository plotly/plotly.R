#' Modify a plotly object inside a shiny app
#'
#' @param outputId single-element character vector indicating the output ID
#'   map to modify (if invoked from a Shiny module, the namespace will be added
#'   automatically)
#' @param session the Shiny session object to which the map belongs; usually the
#'   default value will suffice.
#' @param deferUntilFlush indicates whether actions performed against this
#'   instance should be carried out right away, or whether they should be held
#'   until after the next time all of the outputs are updated.
#' @rdname plotlyProxy
#' @export 
#' @examples
#' 
#' 
#' if (require("shiny") && interactive()) {
#'   plotly_example("shiny", "proxy_relayout")
#'   plotly_example("shiny", "proxy_mapbox")
#' }
#' 
plotlyProxy <- function(outputId, session = shiny::getDefaultReactiveDomain(), 
                        deferUntilFlush = TRUE) {
  
  # implementation very similar to leaflet::leafletProxy & DT:dataTableProxy
  if (is.null(session)) {
    stop("plotlyProxy must be called from the server function of a Shiny app")
  }
  
  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && 
      # TODO: require a recent version of R and use startsWith()?
      substring(outputId, 1, nchar(session$ns(""))) != session$ns("")) {
    outputId <- session$ns(outputId)
  }
  structure(
    list(
      session = session, 
      id = outputId,
      deferUntilFlush = deferUntilFlush
      # TODO: is there actually a use-case for this?
      #x = structure(list(), leafletData = data), 
      #dependencies = NULL
    ), 
    class = "plotly_proxy"
  )
}


# ----------------------------------------------------------------------
# TODO: implement some higher-level functions, say `plotlyProxyLayout()`,
# `plotlyProxyAddTraces()`, `plotlyProxyStyle()`, that pick the right
# method, except formula/data mappings, and possibly some argument checking 
# ----------------------------------------------------------------------


#' @param p a plotly proxy object (created with `plotlyProxy`)
#' @param method a plotlyjs method to invoke. For a list of options,
#' visit the \href{https://plot.ly/javascript/plotlyjs-function-reference}{plotlyjs function reference}
#' @param ... unnamed arguments passed onto the plotly.js method
#' @rdname plotlyProxy
#' @export
plotlyProxyInvoke <- function(p, method, ...) {
  
  if (!is.proxy(p))
    stop("p must be a proxy object. See `help(plotlyProxy)`", call. = FALSE)
  
  if (missing(method))
    stop(
      "Must provide a plotly.js method (as a character string of length 1).\n", 
      sprintf("Valid options include: '%s'", 
              paste(plotlyjs_methods(), collapse = "', '")),
      call. = FALSE
    )
  
  method <- match.arg(method, plotlyjs_methods())
  
  msg <- list(
    id = p$id,
    method = method,
    # TODO: can we leverage the plotly_build() infrastructure in a smart way?
    #  args = evalFormula(list(...), data)
    args = list(...)
  )
  
  if (isTRUE(p$deferUntilFlushed)) {
    
    p$session$onFlushed(function() {
      p$session$sendCustomMessage("plotly-calls", msg)
    }, once = TRUE)
    
  } else {
    
    p$session$sendCustomMessage("plotly-calls", msg)
    
  }
  
  p
}


plotlyjs_methods <- function() {
  c(
    "restyle", "relayout", "update", "addTraces", "deleteTraces", "moveTraces",
    "extendTraces", "prependTraces", "purge", "toImage", "downloadImage", "animate",
    "newPlot", "react", "validate", "makeTemplate", "validateTemplate", "addFrames",
    "reconfig"
  )
}


is.proxy <- function(x) {
  inherits(x, "plotly_proxy")
}
