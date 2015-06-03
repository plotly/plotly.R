#' Create a 'scatter' trace object
#' 
#' @param x, y, r, t, mode, name, text, error_y, error_x, marker, line, 
#' textposition, textfont, connectgaps, fill, fillcolor, opacity, xaxis, yaxis, 
#' showlegend, stream, visible, xsrc, ysrc
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#scatter}
#' @examples \dontrun{
#' # by default, scatter() creates a scatterplot
#' (p <- with(economics, scatter(date, uempmed)))
#' 
#' m <- loess(uempmed ~ as.numeric(date), economics)
#' # change the mode to get a line plot (and use `+` to add it to the scatterplot)
#' p + scatter(economics$date, fitted(m), mode = "lines") +
#'   layout(showLegend = FALSE)
#' }
scatter <- function(x = NULL, y = NULL, r = NULL, t = NULL, mode = "markers", 
                    name = NULL, text = NULL, error_y = NULL, error_x = NULL, 
                    marker = NULL, line = NULL, textposition = NULL, 
                    textfont = NULL, connectgaps = NULL, fill = NULL, 
                    fillcolor = NULL, opacity = NULL, xaxis = NULL, 
                    yaxis = NULL, showlegend = NULL, stream = NULL, 
                    visible = NULL, xsrc = NULL, ysrc = NULL) {
  # there has to be a better way to ensure arguments are evaluated in correct context
  argz <- list(
    x = x, y = y, r = r, t = t, mode = mode, name = name, text = text, 
    error_y = error_y, error_x = error_x, marker = marker, line = line, 
    textposition = textposition, textfont = textfont, connectgaps = connectgaps, 
    fill = fill, fillcolor = fillcolor, opacity = opacity, xaxis = xaxis, 
    yaxis = yaxis, showlegend = showlegend, stream = stream, visible = visible, 
    xsrc = xsrc, ysrc = ysrc
  )
  tr <- c(dropNulls(argz), list(type = "scatter"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("scatter", "plotly")
  )
}

#' Create a 'bar' trace object
#' 
#' @param x, y, name, orientation, text, error_y, error_x, marker, opacity, 
#' xaxis, yaxis, showlegend, stream, visible, xsrc, ysrc, r, t
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#bar}
#' @examples \dontrun{
#' x <- with(diamonds, tapply(price, cut, mean))
#' bar(names(x), as.numeric(x))
#' # TODO: an example of layering bar traces to get a mosiac chart?
#' }
bar <- function(x = NULL, y = NULL, name = NULL, orientation = NULL, text = NULL, 
                error_y = NULL, error_x = NULL, marker = NULL, opacity = NULL, 
                xaxis = NULL, yaxis = NULL, showlegend = NULL, stream = NULL, 
                visible = NULL, xsrc = NULL, ysrc = NULL, r = NULL, t = NULL) {
  argz <- list(
    x = x, y = y, name = name, orientation = orientation, text = text, 
    error_y = error_y, error_x = error_x, marker = marker, opacity = opacity, 
    xaxis = xaxis, yaxis = yaxis, showlegend = showlegend, stream = stream, 
    visible = visible, xsrc = xsrc, ysrc = ysrc, r = r, t = t
  )
  tr <- c(dropNulls(argz), list(type = "bar"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("bar", "plotly")
  )
}

#' Create a 'histogram' trace object
#' 
#' @param x, y, histnorm, histfunc, name, orientation, autobinx, nbinsx, xbins, 
#' autobiny, nbinsy, ybins, text, error_y, error_x, marker, opacity, xaxis, 
#' yaxis, showlegend, stream, visible, xsrc, ysrc
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#histogram}
#' 

histogram <- function(x = NULL, y = NULL, histnorm = NULL, histfunc = NULL, 
                      name = NULL, orientation = NULL, autobinx = NULL, 
                      nbinsx = NULL, xbins = NULL, autobiny = NULL, 
                      nbinsy = NULL, ybins = NULL, text = NULL, error_y = NULL, 
                      error_x = NULL, marker = NULL, opacity = NULL, 
                      xaxis = NULL, yaxis = NULL, showlegend = NULL, 
                      stream = NULL, visible = NULL, xsrc = NULL, ysrc = NULL) {
  argz <- list(
    x = x, y = y, histnorm = histnorm, histfunc = histfunc, name = name, 
    orientation = orientation, autobinx = autobinx, nbinsx = nbinsx, 
    xbins = xbins, autobiny = autobiny, nbinsy = nbinsy, ybins = ybins, 
    text = text, error_y = error_y, error_x = error_x, marker = marker, 
    opacity = opacity, xaxis = xaxis, yaxis = yaxis, showlegend = showlegend, 
    stream = stream, visible = visible, xsrc = xsrc, ysrc = ysrc
  )
  tr <- c(dropNulls(argz), list(type = "histogram"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("histogram", "plotly")
  )
}

#' Create a 'box' trace object
#' 
#' @param y, x0, x, name, boxmean, boxpoints, jitter, pointpos, whiskerwidth, 
#' fillcolor, marker, line, opacity, xaxis, yaxis, showlegend, stream, visible, 
#' xsrc, ysrc
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#box}
#' 
box <- function(y = NULL, x0 = NULL, x = NULL, name = NULL, boxmean = NULL, 
                boxpoints = NULL, jitter = NULL, pointpos = NULL, 
                whiskerwidth = NULL, fillcolor = NULL, marker = NULL, 
                line = NULL, opacity = NULL, xaxis = NULL, yaxis = NULL, 
                showlegend = NULL, stream = NULL, visible = NULL, xsrc = NULL, 
                ysrc = NULL) {
  argz <- list(
    y = y, x0 = x0, x = x, name = name, boxmean = boxmean, boxpoints = boxpoints, 
    jitter = jitter, pointpos = pointpos, whiskerwidth = whiskerwidth, 
    fillcolor = fillcolor, marker = marker, line = line, opacity = opacity, 
    xaxis = xaxis, yaxis = yaxis, showlegend = showlegend, stream = stream, 
    visible = visible, xsrc = xsrc, ysrc = ysrc
  )
  tr <- c(dropNulls(argz), list(type = "box"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("box", "plotly")
  )
}

#' Create a 'heatmap' trace object
#' 
#' @param z, x, y, name, zauto, zmin, zmax, colorscale, reversescale, showscale,
#'  colorbar, zsmooth, opacity, connectgaps, xaxis, yaxis, showlegend, stream, 
#'  text, visible, x0, dx, y0, dy, xtype, ytype
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#heatmap}
#' 
heatmap <- function(z = NULL, x = NULL, y = NULL, name = NULL, zauto = NULL, 
                    zmin = NULL, zmax = NULL, colorscale = NULL, 
                    reversescale = NULL, showscale = NULL, colorbar = NULL, 
                    zsmooth = NULL, opacity = NULL, connectgaps = NULL, 
                    xaxis = NULL, yaxis = NULL, showlegend = NULL, 
                    stream = NULL, text = NULL, visible = NULL, x0 = NULL, 
                    dx = NULL, y0 = NULL, dy = NULL, xtype = NULL, 
                    ytype = NULL) {
  argz <- list(
    z = z, x = x, y = y, name = name, zauto = zauto, zmin = zmin, zmax = zmax, 
    colorscale = colorscale, reversescale = reversescale, showscale = showscale, 
    colorbar = colorbar, zsmooth = zsmooth, opacity = opacity, 
    connectgaps = connectgaps, xaxis = xaxis, yaxis = yaxis, 
    showlegend = showlegend, stream = stream, text = text, visible = visible, 
    x0 = x0, dx = dx, y0 = y0, dy = dy, xtype = xtype, ytype = ytype
  )
  tr <- c(dropNulls(argz), list(type = "heatmap"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("heatmap", "plotly")
  )
}

#' Create a 'contour' trace object
#' 
#' @param z, x, y, name, zauto, zmin, zmax, autocontour, ncontours, contours, 
#' line, colorscale, reversescale, showscale, colorbar, connectgaps, opacity, 
#' xaxis, yaxis, showlegend, stream, visible, x0, dx, y0, dy, xtype, ytype
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#contour}
#' 
contour <- function(z = NULL, x = NULL, y = NULL, name = NULL, zauto = NULL, 
                    zmin = NULL, zmax = NULL, autocontour = NULL, 
                    ncontours = NULL, contours = NULL, line = NULL, 
                    colorscale = NULL, reversescale = NULL, showscale = NULL, 
                    colorbar = NULL, connectgaps = NULL, opacity = NULL, 
                    xaxis = NULL, yaxis = NULL, showlegend = NULL, 
                    stream = NULL, visible = NULL, x0 = NULL, dx = NULL, 
                    y0 = NULL, dy = NULL, xtype = NULL, ytype = NULL) {
  argz <- list(
    z = z, x = x, y = y, name = name, zauto = zauto, zmin = zmin, zmax = zmax, 
    autocontour = autocontour, ncontours = ncontours, contours = contours, 
    line = line, colorscale = colorscale, reversescale = reversescale, 
    showscale = showscale, colorbar = colorbar, connectgaps = connectgaps, 
    opacity = opacity, xaxis = xaxis, yaxis = yaxis, showlegend = showlegend, 
    stream = stream, visible = visible, x0 = x0, dx = dx, y0 = y0, dy = dy, 
    xtype = xtype, ytype = ytype
  )
  tr <- c(dropNulls(argz), list(type = "contour"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("contour", "plotly")
  )
}

#' Create a 'histogram2d' trace object
#' 
#' @param x, y, histnorm, histfunc, name, autobinx, nbinsx, xbins, autobiny, 
#' nbinsy, ybins, colorscale, reversescale, showscale, colorbar, zauto, zmin, 
#' zmax, zsmooth, opacity, xaxis, yaxis, showlegend, stream, visible, xsrc, ysrc
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#histogram2d}
#' 
histogram2d <- function(x = NULL, y = NULL, histnorm = NULL, histfunc = NULL, 
                        name = NULL, autobinx = NULL, nbinsx = NULL, 
                        xbins = NULL, autobiny = NULL, nbinsy = NULL, 
                        ybins = NULL, colorscale = NULL, reversescale = NULL, 
                        showscale = NULL, colorbar = NULL, zauto = NULL, 
                        zmin = NULL, zmax = NULL, zsmooth = NULL, opacity = NULL, 
                        xaxis = NULL, yaxis = NULL, showlegend = NULL, 
                        stream = NULL, visible = NULL, xsrc = NULL, ysrc = NULL) {
  argz <- list(
    x = x, y = y, histnorm = histnorm, histfunc = histfunc, name = name, 
    autobinx = autobinx, nbinsx = nbinsx, xbins = xbins, autobiny = autobiny, 
    nbinsy = nbinsy, ybins = ybins, colorscale = colorscale, 
    reversescale = reversescale, showscale = showscale, colorbar = colorbar, 
    zauto = zauto, zmin = zmin, zmax = zmax, zsmooth = zsmooth, 
    opacity = opacity, xaxis = xaxis, yaxis = yaxis, showlegend = showlegend, 
    stream = stream, visible = visible, xsrc = xsrc, ysrc = ysrc
  )
  tr <- c(dropNulls(argz), list(type = "histogram2d"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("histogram2d", "plotly")
  )
}

#TODO: Can we create this object by just doing histogram2d() + contour()????
histogram2dcontour <- function(){
  message("Use histogram2d() + contour()")
}

#' Create a 'area' trace object
#' 
#' @param r, t, name, marker, showlegend, stream, visible, angularaxis, 
#' radialaxis
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#area}
#' 
area <- function(r = NULL, t = NULL, name = NULL, marker = NULL, 
                 showlegend = NULL, stream = NULL, visible = NULL, 
                 angularaxis = NULL, radialaxis = NULL) {
  argz <- list(
    r = r, t = t, name = name, marker = marker, showlegend = showlegend, 
    stream = stream, visible = visible, angularaxis = angularaxis, 
    radialaxis = radialaxis
  )
  tr <- c(dropNulls(argz), list(type = "area"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("area", "plotly")
  )
}

#' Create a 'scatter3d' trace object
#' 
#' @param x, y, z, mode, name, text, error_z, error_y, error_x, marker, line, 
#' textposition, scene, stream, visible
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#scatter3d}
#' 
scatter3d <- function(x = NULL, y = NULL, z = NULL, mode = NULL, name = NULL, 
                      text = NULL, error_z = NULL, error_y = NULL, 
                      error_x = NULL, marker = NULL, line = NULL, 
                      textposition = NULL, scene = NULL, stream = NULL, 
                      visible = NULL) {
  argz <- list(
    x = x, y = y, z = z, mode = mode, name = name, text = text, 
    error_z = error_z, error_y = error_y, error_x = error_x, marker = marker, 
    line = line, textposition = textposition, scene = scene, stream = stream, 
    visible = visible
  )
  tr <- c(dropNulls(argz), list(type = "scatter3d"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("scatter3d", "plotly")
  )
}

#' Create a 'surface' trace object
#' 
#' @param z, x, y, name, colorscale, scene, stream, visible, type
#' @export
#' @author Carson Sievert
#' @references \url{https://plot.ly/javascript-graphing-library/reference/#surface}
#' 
surface <- function(z = NULL, x = NULL, y = NULL, name = NULL, colorscale = NULL, 
                    scene = NULL, stream = NULL, visible = NULL, type = NULL) {
  argz <- list(
    z = z, x = x, y = y, name = name, colorscale = colorscale, 
    scene = scene, stream = stream, visible = visible, type = type
  )
  tr <- c(dropNulls(argz), list(type = "surface"))
  structure(
    list(data = list(tr), layout = NULL),
    class = c("surface", "plotly")
  )
}
