# Layout and layout style objects
# https://plot.ly/javascript-graphing-library/reference/#Layout_and_layout_style_objects
is.layout <- function(x) inherits(x, "layout")
is.layoutLike <- function(x) 
  inherits(x, c("legend", "annotations", "font", "margin", "axis"))

#' Create a layout object.
#' 
#' A layout object by itself is not sufficient for creating a plot. 
#' A layout object must be added to a plotly trace object.
#' 
#' @param title, titlefont, font, showlegend, autosize, width, height, xaxis, 
#' yaxis, legend, annotations, margin, paper_bgcolor, plot_bgcolor, hovermode, 
#' dragmode, separators, barmode, bargap, bargroupgap, barnorm, boxmode, boxgap, 
#' boxgroupgap, radialaxis, angularaxis, scene, direction, orientation, hidesources
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#layout}
#' @examples
#' 
#' # TODO: how do we ensure a single escape character goes in response body?
#' histogram(rnorm(100)) + layout(title = "$X \\sim N(0, 1)$")
#' 

layout <- function(title = NULL, titlefont = NULL, font = NULL, 
                   showlegend = NULL, autosize = NULL, width = NULL, 
                   height = NULL, xaxis = NULL, yaxis = NULL, legend = NULL, 
                   annotations = NULL, margin = NULL, paper_bgcolor = NULL, 
                   plot_bgcolor = NULL, hovermode = NULL, dragmode = NULL, 
                   separators = NULL, barmode = NULL, bargap = NULL, 
                   bargroupgap = NULL, barnorm = NULL, boxmode = NULL, 
                   boxgap = NULL, boxgroupgap = NULL, radialaxis = NULL, 
                   angularaxis = NULL, scene = NULL, direction = NULL, 
                   orientation = NULL, hidesources = NULL) {
  # In case, for some reason, we don't want to list all those arguments
#   dots <- list(...)
#   classes <- unlist(lapply(dots, class))
#   nms <- names(dots)
#   idx <- nms %in% ""
#   nms[idx] <- classes[idx]
#   names(dots) <- nms
  argz <- list(
    title = title, titlefont = titlefont, font = font, showlegend = showlegend, 
    autosize = autosize, width = width, height = height, xaxis = xaxis, 
    yaxis = yaxis, legend = legend, annotations = annotations, margin = margin, 
    paper_bgcolor = paper_bgcolor, plot_bgcolor = plot_bgcolor, 
    hovermode = hovermode, dragmode = dragmode, separators = separators, 
    barmode = barmode, bargap = bargap, bargroupgap = bargroupgap, 
    barnorm = barnorm, boxmode = boxmode, boxgap = boxgap, 
    boxgroupgap = boxgroupgap, radialaxis = radialaxis, 
    angularaxis = angularaxis, scene = scene, direction = direction, 
    orientation = orientation, hidesources = hidesources
  )
  structure(
    dropNulls(argz),
    class = "layout"
  )
}

#' Create a font object.
#' 
#' Available as: \itemize{
#' \item \code{tickfont} in \link{colorbar}, \link{xaxis}, \link{yaxis}, \link{zaxis}
#' \item \code{textfont} in \link{scatter} 
#' \item \code{font} in \link{layout}, \link{legend}, \link{annotation}
#' \item \code{titlefont} in \link{colorbar}, \link{xaxis}, \link{yaxis}, \link{zaxis}, \link{layout}
#' }
#' 
#' @param family, size, color, outlinecolor
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#font}
#' 

font <- function(family = NULL, size = NULL, color = NULL, outlinecolor = NULL) {
  argz <- list(
    family = family, size = size, color = color, outlinecolor = outlinecolor
  )
  structure(
    dropNulls(argz),
    class = "font"
  )
}

#' Create a legend object.
#' 
#' Available in \link{layout}.
#' 
#' @param x, y, traceorder, font, bgcolor, bordercolor, borderwidth, xref, yref, 
#' xanchor, yanchor
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#legend}
#' 

legend <- function(x = NULL, y = NULL, traceorder = NULL, font = NULL, 
                   bgcolor = NULL, bordercolor = NULL, borderwidth = NULL, 
                   xref = NULL, yref = NULL, xanchor = NULL, yanchor = NULL) {
  argz <- list(
    x = x, y = y, traceorder = traceorder, font = font, bgcolor = bgcolor, 
    bordercolor = bordercolor, borderwidth = borderwidth, xref = xref, 
    yref = yref, xanchor = xanchor, yanchor = yanchor
  )
  structure(
    dropNulls(argz),
    class = "legend"
  )
}

#' Create an annotation object.
#' 
#' @param x, y, xref, yref, text, showarrow, font, xanchor, yanchor, align, 
#' arrowhead, arrowsize, arrowwidth, arrowcolor, ax, ay, textangle, bordercolor, 
#' borderwidth, borderpad, bgcolor, opacity
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#annotation}

annotation <- function(x = NULL, y = NULL, xref = NULL, yref = NULL, 
                       text = NULL, showarrow = NULL, font = NULL, 
                       xanchor = NULL, yanchor = NULL, align = NULL, 
                       arrowhead = NULL, arrowsize = NULL, arrowwidth = NULL, 
                       arrowcolor = NULL, ax = NULL, ay = NULL, 
                       textangle = NULL, bordercolor = NULL, borderwidth = NULL, 
                       borderpad = NULL, bgcolor = NULL, opacity = NULL) {
  argz <- list(
    x = x, y = y, xref = xref, yref = yref, text = text, showarrow = showarrow, 
    font = font, xanchor = xanchor, yanchor = yanchor, align = align, 
    arrowhead = arrowhead, arrowsize = arrowsize, arrowwidth = arrowwidth, 
    arrowcolor = arrowcolor, ax = ax, ay = ay, textangle = textangle, 
    bordercolor = bordercolor, borderwidth = borderwidth, borderpad = borderpad, 
    bgcolor = bgcolor, opacity = opacity
  )
  structure(
    dropNulls(argz),
    class = "annotation"
  )
}


#' Create a margin object.
#' 
#' Available in \link{layout}.
#' 
#' @param l, r, b, t, pad, autoexpand, data, layout
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#margin}

margin <- function(l = NULL, r = NULL, b = NULL, t = NULL, pad = NULL, 
                   autoexpand = NULL, data = NULL, layout = NULL) {
  argz <- list(
    l = l, r = r, b = b, t = t, pad = pad, autoexpand = autoexpand, 
    data = data, layout = layout
  )
  structure(
    dropNulls(argz),
    class = "margin"
  )
}
