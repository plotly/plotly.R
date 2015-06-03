# Axis Objects
# https://plot.ly/javascript-graphing-library/reference/#Axis_objects
is.axis <- function(x) inherits(x, "axis")

#' Create an xaxis object.
#' 
#' Available in \link{scene} and \link{layout}
#' 
#' @param title, titlefont, range, domain, type, rangemode, autorange, showgrid, 
#' zeroline, showline, autotick, nticks, ticks, showticklabels, tick0, dtick, 
#' ticklen, tickwidth, tickcolor, tickangle, tickfont, exponentformat, 
#' showexponent, mirror, gridcolor, gridwidth, zerolinecolor, zerolinewidth, 
#' linecolor, linewidth, anchor, overlaying, side, position, showbackground, 
#' backgroundcolor, showspikes, spikesides, spikethickness
#' @export
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#xaxis}
#' 
xaxis <- function(title = NULL, titlefont = NULL, range = NULL, domain = NULL, 
                  type = NULL, rangemode = NULL, autorange = NULL, 
                  showgrid = NULL, zeroline = NULL, showline = NULL, 
                  autotick = NULL, nticks = NULL, ticks = NULL, 
                  showticklabels = NULL, tick0 = NULL, dtick = NULL, 
                  ticklen = NULL, tickwidth = NULL, tickcolor = NULL, 
                  tickangle = NULL, tickfont = NULL, exponentformat = NULL, 
                  showexponent = NULL, mirror = NULL, gridcolor = NULL, 
                  gridwidth = NULL, zerolinecolor = NULL, zerolinewidth = NULL, 
                  linecolor = NULL, linewidth = NULL, anchor = NULL, 
                  overlaying = NULL, side = NULL, position = NULL, 
                  showbackground = NULL, backgroundcolor = NULL, 
                  showspikes = NULL, spikesides = NULL, spikethickness = NULL) {
  argz <- list(
    title = title, titlefont = titlefont, range = range, domain = domain, 
    type = type, rangemode = rangemode, autorange = autorange, 
    showgrid = showgrid, zeroline = zeroline, showline = showline, 
    autotick = autotick, nticks = nticks, ticks = ticks, 
    showticklabels = showticklabels, tick0 = tick0, dtick = dtick, 
    ticklen = ticklen, tickwidth = tickwidth, tickcolor = tickcolor, 
    tickangle = tickangle, tickfont = tickfont, exponentformat = exponentformat, 
    showexponent = showexponent, mirror = mirror, gridcolor = gridcolor, 
    gridwidth = gridwidth, zerolinecolor = zerolinecolor, 
    zerolinewidth = zerolinewidth, linecolor = linecolor, linewidth = linewidth, 
    anchor = anchor, overlaying = overlaying, side = side, position = position, 
    showbackground = showbackground, backgroundcolor = backgroundcolor, 
    showspikes = showspikes, spikesides = spikesides, 
    spikethickness = spikethickness
  )
  structure(
    dropNulls(argz),
    class = c("xaxis", "axis")
  )
}

#' Create an yaxis object.
#' 
#' Available in \link{scene} and \link{layout}
#' 
#' @inheritParams xaxis
#' @export
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#yaxis}
#' 
yaxis <- function(title = NULL, titlefont = NULL, range = NULL, domain = NULL, 
                  type = NULL, rangemode = NULL, autorange = NULL, 
                  showgrid = NULL, zeroline = NULL, showline = NULL, 
                  autotick = NULL, nticks = NULL, ticks = NULL, 
                  showticklabels = NULL, tick0 = NULL, dtick = NULL, 
                  ticklen = NULL, tickwidth = NULL, tickcolor = NULL, 
                  tickangle = NULL, tickfont = NULL, exponentformat = NULL, 
                  showexponent = NULL, mirror = NULL, gridcolor = NULL, 
                  gridwidth = NULL, zerolinecolor = NULL, zerolinewidth = NULL, 
                  linecolor = NULL, linewidth = NULL, anchor = NULL, 
                  overlaying = NULL, side = NULL, position = NULL, 
                  showbackground = NULL, backgroundcolor = NULL, 
                  showspikes = NULL, spikesides = NULL, spikethickness = NULL) {
  argz <- list(
    title = title, titlefont = titlefont, range = range, domain = domain, 
    type = type, rangemode = rangemode, autorange = autorange, 
    showgrid = showgrid, zeroline = zeroline, showline = showline, 
    autotick = autotick, nticks = nticks, ticks = ticks, 
    showticklabels = showticklabels, tick0 = tick0, dtick = dtick, 
    ticklen = ticklen, tickwidth = tickwidth, tickcolor = tickcolor, 
    tickangle = tickangle, tickfont = tickfont, exponentformat = exponentformat, 
    showexponent = showexponent, mirror = mirror, gridcolor = gridcolor, 
    gridwidth = gridwidth, zerolinecolor = zerolinecolor, 
    zerolinewidth = zerolinewidth, linecolor = linecolor, linewidth = linewidth, 
    anchor = anchor, overlaying = overlaying, side = side, position = position, 
    showbackground = showbackground, backgroundcolor = backgroundcolor, 
    showspikes = showspikes, spikesides = spikesides, 
    spikethickness = spikethickness
  )
  structure(
    dropNulls(argz),
    class = c("yaxis", "axis")
  )
}

#' Create an radialaxis object.
#' 
#' Available in \link{layout}
#' 
#' @param range, domain, orientation, showline, showticklabels, tickorientation, 
#' ticklen, tickcolor, ticksuffix, endpadding, visible
#' @export
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#radialaxis}
#' 
radialaxis <- function(range = NULL, domain = NULL, orientation = NULL, 
                       showline = NULL, showticklabels = NULL, 
                       tickorientation = NULL, ticklen = NULL, tickcolor = NULL, 
                       ticksuffix = NULL, endpadding = NULL, visible = NULL) {
  argz <- list(
    range = range, domain = domain, orientation = orientation, 
    showline = showline, showticklabels = showticklabels, 
    tickorientation = tickorientation, ticklen = ticklen, tickcolor = tickcolor,
    ticksuffix = ticksuffix, endpadding = endpadding, visible = visible
  )
  structure(
    dropNulls(argz),
    class = c("radialaxis", "axis")
  )
}

#' Create an angularaxis object.
#' 
#' Available in \link{layout}
#' 
#' @inheritParams radialaxis
#' @export
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#angularaxis}
#' 
angularaxis <- function(range = NULL, domain = NULL, orientation = NULL, 
                       showline = NULL, showticklabels = NULL, 
                       tickorientation = NULL, ticklen = NULL, tickcolor = NULL, 
                       ticksuffix = NULL, endpadding = NULL, visible = NULL) {
  argz <- list(
    range = range, domain = domain, orientation = orientation, 
    showline = showline, showticklabels = showticklabels, 
    tickorientation = tickorientation, ticklen = ticklen, tickcolor = tickcolor,
    ticksuffix = ticksuffix, endpadding = endpadding, visible = visible
  )
  structure(
    dropNulls(argz),
    class = c("angularaxis", "axis")
  )
}


#' Create an scene object.
#' 
#' Available in \link{layout}
#' 
#' @param xaxis, yaxis, zaxis, cameraposition, domain, bgcolor
#' @export
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#scene}
#' 
scene <- function(xaxis = NULL, yaxis = NULL, zaxis = NULL, 
                  cameraposition = NULL, domain = NULL, bgcolor = NULL) {
  argz <- list(
    xaxis = xaxis, yaxis = yaxis, zaxis = zaxis, 
    cameraposition = cameraposition, domain = domain, bgcolor = bgcolor
  )
  structure(
    dropNulls(argz),
    class = c("scene", "axis")
  )
}

#' Create an zaxis object.
#' 
#' Available in \link{scene}
#' 
#' @inheritParams xaxis
#' @export
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#zaxis}
#' 
zaxis <- function(title = NULL, titlefont = NULL, range = NULL, domain = NULL, 
                  type = NULL, rangemode = NULL, autorange = NULL, 
                  showgrid = NULL, zeroline = NULL, showline = NULL, 
                  autotick = NULL, nticks = NULL, ticks = NULL, 
                  showticklabels = NULL, tick0 = NULL, dtick = NULL, 
                  ticklen = NULL, tickwidth = NULL, tickcolor = NULL, 
                  tickangle = NULL, tickfont = NULL, exponentformat = NULL, 
                  showexponent = NULL, mirror = NULL, gridcolor = NULL, 
                  gridwidth = NULL, zerolinecolor = NULL, zerolinewidth = NULL, 
                  linecolor = NULL, linewidth = NULL, anchor = NULL, 
                  overlaying = NULL, side = NULL, position = NULL, 
                  showbackground = NULL, backgroundcolor = NULL, 
                  showspikes = NULL, spikesides = NULL, spikethickness = NULL) {
  argz <- list(
    title = title, titlefont = titlefont, range = range, domain = domain, 
    type = type, rangemode = rangemode, autorange = autorange, 
    showgrid = showgrid, zeroline = zeroline, showline = showline, 
    autotick = autotick, nticks = nticks, ticks = ticks, 
    showticklabels = showticklabels, tick0 = tick0, dtick = dtick, 
    ticklen = ticklen, tickwidth = tickwidth, tickcolor = tickcolor, 
    tickangle = tickangle, tickfont = tickfont, exponentformat = exponentformat, 
    showexponent = showexponent, mirror = mirror, gridcolor = gridcolor, 
    gridwidth = gridwidth, zerolinecolor = zerolinecolor, 
    zerolinewidth = zerolinewidth, linecolor = linecolor, linewidth = linewidth, 
    anchor = anchor, overlaying = overlaying, side = side, position = position, 
    showbackground = showbackground, backgroundcolor = backgroundcolor, 
    showspikes = showspikes, spikesides = spikesides, 
    spikethickness = spikethickness
  )
  structure(
    dropNulls(argz),
    class = c("zaxis", "axis")
  )
}
