# Trace Auxiliary Objects
# https://plot.ly/javascript-graphing-library/reference/#Trace_auxiliary_objects
is.aux <- function(x) inherits(x, "auxiliary")

#' Create an 'error_y' auxiliary object
#' 
#' Available in \link{scatter}, \link{bar}, \link{histogram}, and \link{scatter3d}.
#' 
#' @param type, symmetric, array, value, arrayminus, valueminus, color, 
#' thickness, width, opacity, visible
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#error_y}
#' @examples \dontrun{
#' means <- with(diamonds, tapply(carat, cut, mean))
#' sds <- with(diamonds, tapply(carat, cut, sd))
#' # construct the error bar object
#' err <- error_y(array = as.numeric(sds))
#' # pass it to a trace object
#' scatter(names(means), as.numeric(means), error_y = err)
#' }
error_y <- function(type = NULL, symmetric = NULL, array = NULL, value = NULL, 
                    arrayminus = NULL, valueminus = NULL, color = NULL, 
                    thickness = NULL, width = NULL, opacity = NULL, visible = NULL) {
  argz <- list(
    type = type, symmetric = symmetric, array = array, value = value, 
    arrayminus = arrayminus, valueminus = valueminus, color = color, 
    thickness = thickness, width = width, opacity = opacity, visible = visible
  )
  structure(
    dropNulls(argz),
    class = c("error_y", "auxiliary")
  )
}

#' Create an 'error_x' auxiliary object
#' 
#' Available in \link{scatter}, \link{bar}, \link{histogram}, and \link{scatter3d}.
#' 
#' @param type, symmetric, array, value, arrayminus, valueminus, color, 
#' thickness, width, opacity, copy_ystyle, visible
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#error_x}
#' @examples \dontrun{
#' means <- with(diamonds, tapply(carat, cut, mean))
#' sds <- with(diamonds, tapply(carat, cut, sd))
#' # construct the error bar object
#' err <- error_x(array = as.numeric(sds))
#' # pass it to a trace object
#' scatter(as.numeric(means), names(means), error_x = err)
#' }
error_x <- function(type = NULL, symmetric = NULL, array = NULL, value = NULL, 
                    arrayminus = NULL, valueminus = NULL, color = NULL, 
                    thickness = NULL, width = NULL, opacity = NULL, 
                    copy_ystyle = NULL, visible = NULL) {
  argz <- list(
    type = type, symmetric = symmetric, array = array, value = value, 
    arrayminus = arrayminus, valueminus = valueminus, color = color, 
    thickness = thickness, width = width, opacity = opacity, 
    copy_ystyle = copy_ystyle, visible = visible
  )
  structure(
    dropNulls(argz),
    class = c("error_x", "auxiliary")
  )
}

#' Create a 'xbins' auxiliary object
#' 
#' Available in \link{histogram}, \link{histogram2d}, \link{histogram2dcontour}
#' 
#' @param start, end, size
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#xbins}
#' @examples \dontrun{
#' 
#' }
xbins <- function(start = NULL, end = NULL, size = NULL) {
  argz <- list(
    start = start, end = end, size = size
  )
  structure(
    dropNulls(argz),
    class = c("xbins", "auxiliary")
  )
}

#' Create a 'ybins' auxiliary object
#' 
#' Available in \link{histogram}, \link{histogram2d}, \link{histogram2dcontour}
#' 
#' @param start, end, size
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#ybins}
#' @examples \dontrun{
#' 
#' }
ybins <- function(start = NULL, end = NULL, size = NULL) {
  argz <- list(
    start = start, end = end, size = size
  )
  structure(
    dropNulls(argz),
    class = c("ybins", "auxiliary")
  )
}

#' Create a 'marker' auxiliary object
#' 
#' Available in \link{scatter}, \link{bar}, \link{histogram}, \link{box}, 
#' \link{area}, \link{scatter3d}.
#' 
#' @param color, size, symbol, line, opacity, sizeref, sizemode, colorscale, 
#' cauto, cmin, cmax, outliercolor, maxdisplayed
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#marker}
#' @examples \dontrun{
#' 
#' }
marker <- function(color = NULL, size = NULL, symbol = NULL, line = NULL, 
                   opacity = NULL, sizeref = NULL, sizemode = NULL, 
                   colorscale = NULL, cauto = NULL, cmin = NULL, cmax = NULL, 
                   outliercolor = NULL, maxdisplayed = NULL) {
  argz <- list(
    color = color, size = size, symbol = symbol, line = line, opacity = opacity, 
    sizeref = sizeref, sizemode = sizemode, colorscale = colorscale, 
    cauto = cauto, cmin = cmin, cmax = cmax, outliercolor = outliercolor, 
    maxdisplayed = maxdisplayed
  )
  structure(
    dropNulls(argz),
    class = c("marker", "auxiliary")
  )
}

#' Create a 'line' auxiliary object
#' 
#' Available in \link{scatter}, \link{box}, \link{contour}, 
#' \link{histogram2dcontour}, \link{scatter3d}, \link{marker}.
#' 
#' @param color, width, dash, opacity, shape, smoothing, outliercolor, outlierwidth
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#line}
#' @examples \dontrun{
#' 
#' }
line <- function(color = NULL, width = NULL, dash = NULL, opacity = NULL, 
                  shape = NULL, smoothing = NULL, outliercolor = NULL, 
                  outlierwidth = NULL) {
  argz <- list(
    color = color, width = width, dash = dash, opacity = opacity, shape = shape, 
    smoothing = smoothing, outliercolor = outliercolor, 
    outlierwidth = outlierwidth
  )
  structure(
    dropNulls(argz),
    class = c("line", "auxiliary")
  )
}

#' Create a 'contours' auxiliary object
#' 
#' Available in \link{contour} and \link{histogram2dcontour}.
#' 
#' @param showlines, start, end, size, coloring
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#contours}
#' @examples \dontrun{
#' 
#' }
contours <- function(showlines = NULL, start = NULL, end = NULL, 
                     size = NULL, coloring = NULL) {
  argz <- list(
    showlines = showlines, start = start, end = end, size = size, 
    coloring = coloring
  )
  structure(
    dropNulls(argz),
    class = c("contours", "auxiliary")
  )
}


#' Create a 'colorbar' auxiliary object
#' 
#' Available in \link{heatmap}, \link{contour}, \link{histogram2d}, \link{histogram2dcontour}
#' 
#' @param title, titleside, titlefont, thickness, thicknessmode, len, lenmode, 
#' autotick, nticks, ticks, showticklabels, tick0, dtick, ticklen, tickwidth, 
#' tickcolor, tickangle, tickfont, exponentformat, showexponent, x, y, xanchor, 
#' yanchor, bgcolor, outlinecolor, outlinewidth, bordercolor, borderwidth, 
#' xpad, ypad
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#colorbar}
#' @examples \dontrun{
#' 
#' }
colorbar <- function(title = NULL, titleside = NULL, titlefont = NULL, 
                     thickness = NULL, thicknessmode = NULL, len = NULL, 
                     lenmode = NULL, autotick = NULL, nticks = NULL, 
                     ticks = NULL, showticklabels = NULL, tick0 = NULL, 
                     dtick = NULL, ticklen = NULL, tickwidth = NULL, 
                     tickcolor = NULL, tickangle = NULL, tickfont = NULL, 
                     exponentformat = NULL, showexponent = NULL, x = NULL, 
                     y = NULL, xanchor = NULL, yanchor = NULL, bgcolor = NULL, 
                     outlinecolor = NULL, outlinewidth = NULL, 
                     bordercolor = NULL, borderwidth = NULL, xpad = NULL, 
                     ypad = NULL) {
  argz <- list(
    title = title, titleside = titleside, titlefont = titlefont, 
    thickness = thickness, thicknessmode = thicknessmode, len = len, 
    lenmode = lenmode, autotick = autotick, nticks = nticks, ticks = ticks, 
    showticklabels = showticklabels, tick0 = tick0, dtick = dtick, 
    ticklen = ticklen, tickwidth = tickwidth, tickcolor = tickcolor, 
    tickangle = tickangle, tickfont = tickfont, exponentformat = exponentformat, 
    showexponent = showexponent, x = x, y = y, xanchor = xanchor, 
    yanchor = yanchor, bgcolor = bgcolor, outlinecolor = outlinecolor, 
    outlinewidth = outlinewidth, bordercolor = bordercolor, 
    borderwidth = borderwidth, xpad = xpad, ypad = ypad
  )
  structure(
    dropNulls(argz),
    class = c("colorbar", "auxiliary")
  )
}

#' Create a 'stream' auxiliary object
#' 
#' Available in \link{scatter}, \link{bar}, \link{histogram}, \link{box}, 
#' \link{heatmap}, \link{contour}, \link{histogram2d}, \link{histogram2dcontour}, 
#' \link{area}, \link{scatter3d}, \link{surface}.
#' 
#' @param token, maxpoints
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#stream}
#' @examples \dontrun{
#' 
#' }
stream <- function(token = NULL, maxpoints = NULL) {
  argz <- list(
    token = token, maxpoints = maxpoints
  )
  structure(
    dropNulls(argz),
    class = c("stream", "auxiliary")
  )
}


#' Create a 'error_z' auxiliary object
#' 
#' Available in \link{scatter3d}.
#' 
#' @param type, symmetric, array, value, arrayminus, valueminus, color, 
#' thickness, width, opacity, visible.
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#error_z}
#' @examples \dontrun{
#' 
#' }
error_z <- function(type = NULL, symmetric = NULL, array = NULL, value = NULL, 
                    arrayminus = NULL, valueminus = NULL, color = NULL, 
                    thickness = NULL, width = NULL, opacity = NULL, 
                    visible = NULL) {
  argz <- list(
    type = type, symmetric = symmetric, array = array, value = value, 
    arrayminus = arrayminus, valueminus = valueminus, color = color, 
    thickness = thickness, width = width, opacity = opacity, visible = visible
  )
  structure(
    dropNulls(argz),
    class = c("error_z", "auxiliary")
  )
}
