##' Convert ggplot params to plotly.
##' @param params named list ggplot names -> values.
##' @param aesVec vector mapping ggplot names to plotly names.
##' @param defaults named list ggplot names -> values.
##' @export
##' @return named list.
##' @author Toby Dylan Hocking
paramORdefault <- function(params, aesVec, defaults){
  marker <- list()
  for(ggplot.name in names(aesVec)){
    plotly.name <- aesVec[[ggplot.name]]
    ggplot.value <- params[[ggplot.name]]
    if(is.null(ggplot.value)){
      ggplot.value <- defaults[[ggplot.name]]
    }
    if(is.null(ggplot.value)){
      stop("no ggplot default for ", ggplot.name)
    }
    convert <- aesConverters[[ggplot.name]]
    if(is.null(convert)){
      stop("no ggplot converter for ", ggplot.name)
    }
    plotly.value <- convert(ggplot.value)
    names(plotly.value) <- NULL
    marker[[plotly.name]] <- plotly.value
  }
  marker
}
