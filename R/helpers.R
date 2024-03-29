#' Modify the colorbar
#' 
#' @param p a plotly object
#' @param ... arguments are documented here 
#' \url{https://plotly.com/r/reference/#scatter-marker-colorbar}.
#' @param limits numeric vector of length 2. Set the extent of the colorbar scale.
#' @param which colorbar to modify? Should only be relevant for subplots with 
#' multiple colorbars.
#' @author Carson Sievert
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' p <- plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~cyl)
#' 
#' # pass any colorbar attribute -- 
#' # https://plotly.com/r/reference/#scatter-marker-colorbar
#' colorbar(p, len = 0.5)
#' 
#' # Expand the limits of the colorbar
#' colorbar(p, limits = c(0, 20))
#' # values outside the colorbar limits are considered "missing"
#' colorbar(p, limits = c(5, 6))
#' 
#' # also works on colorbars generated via a z value
#' corr <- cor(diamonds[vapply(diamonds, is.numeric, logical(1))])
#' plot_ly(x = rownames(corr), y = colnames(corr), z = corr) %>%
#'  add_heatmap() %>%
#'  colorbar(limits = c(-1, 1))

colorbar <- function(p, ..., limits = NULL, which = 1) {
  colorbar_built(plotly_build(p), ..., limits = limits, which = which)
}

colorbar_built <- function(p, ..., limits = NULL, which = 1) {
  
  isBar <- vapply(p$x$data, is.colorbar, logical(1))
  if (sum(isBar) == 0) {
    warning("Didn't find a colorbar to modify.", call. = FALSE)
    return(p)
  }
  
  indicies <- which(isBar)[which]
  
  for (i in indicies) {
    
    tr <- p$x$data[[i]]
    hasZcolor <- inherits(tr, "zcolor")
    
    # retrain limits of the colorscale
    if (!is.null(limits)) {
      limits <- sort(limits)
      if (hasZcolor) {
        z <- p$x$data[[i]][["z"]]
        if (!is.null(dz <- dim(z))) {
          z <- c(z)
        }
        z[z < limits[1] | limits[2] < z] <- NA
        if (!is.null(dz)) dim(z) <- dz
        p$x$data[[i]]$z <- z
        p$x$data[[i]]$zmin <- limits[1]
        p$x$data[[i]]$zmax <- limits[2]
      } else {
        # since the colorscale is in a different trace, retrain all traces
        # TODO: without knowing which traces are tied to the colorscale,
        # there are always going to be corner-cases where limits are applied 
        # to more traces than it should
        p$x$data <- lapply(p$x$data, function(x) {
          type <- x[["type"]] %||% "scatter"
          col <- x$marker[["color"]]
          if (has_color_array(type, "marker") && is.numeric(col)) {
            x$marker[["color"]][col < limits[1] | limits[2] < col] <- default(NA)
            x$marker[["cmin"]] <- default(limits[1])
            x$marker[["cmax"]] <- default(limits[2])
          }
          col <- x$line[["color"]]
          if (has_color_array(type, "line") && is.numeric(col)) {
            x$line[["color"]][col < limits[1] | limits[2] < col] <- default(NA)
            x$line[["cmin"]] <- default(limits[1])
            x$line[["cmax"]] <- default(limits[2])
          }
          x
        })
      }
    }
    
    # pass along ... to the colorbar
    if (hasZcolor) {
      p$x$data[[i]]$colorbar <- modify_list(tr$colorbar, list(...))
    } else {
      p$x$data[[i]]$marker$colorbar <- modify_list(tr$marker$colorbar, list(...))
    }
  }
  
  p
}


#' Hide guides (legends and colorbars)
#'
#' @param p a plotly object.
#' @export
#' @seealso [hide_legend()], [hide_colorbar()]
#'

hide_guides <- function(p) {
  hide_legend(hide_colorbar(p))
}


#' Hide color bar(s)
#' 
#' @param p a plotly object.
#' @export
#' @seealso [hide_legend()]
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
#' hide_colorbar(p)
#'   
hide_colorbar <- function(p) {
  p <- plotly_build(p)
  for (i in seq_along(p$x$data)) {
    trace <- p$x$data[[i]]
    if (has_attr(trace$type, "showscale")) {
      p$x$data[[i]]$showscale <- default(FALSE)
    }
    if (has_attr(trace$type, "marker")) {
      p$x$data[[i]]$marker$showscale <- default(FALSE)
    }
  }
  p
}

#' Hide legend
#' 
#' @param p a plotly object.
#' @export
#' @seealso [hide_colorbar()]
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~factor(cyl))
#' hide_legend(p)

hide_legend <- function(p) {
  if (ggplot2::is.ggplot(p)) {
    p <- plotly_build(p)
  }
  p$x$.hideLegend <- TRUE
  p
}

#' Convert trace types to WebGL
#' 
#' @param p a plotly or ggplot object.
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' # currently no bargl trace type
#' toWebGL(ggplot() + geom_bar(aes(1:10)))
#' toWebGL(qplot(1:10, 1:10))
#' 
toWebGL <- function(p) {
  if (ggplot2::is.ggplot(p)) {
    p <- plotly_build(p)
  }
  p$x$.plotlyWebGl <- TRUE
  p
}


#' Create a complete empty plotly graph.
#' 
#' Useful when used with [subplot()]
#' 
#' @param ... arguments passed onto [plot_ly()]
#' 
#' @export
plotly_empty <- function(...) {
  eaxis <- list(
    showticklabels = FALSE,
    showgrid = FALSE,
    zeroline = FALSE
  )
  layout(plot_ly(...), xaxis = eaxis, yaxis = eaxis)
}


#' Encode a raster object as a data URI
#' 
#' Encode a raster object as a data URI, which is suitable for 
#' use with `layout()` \href{https://plotly.com/r/reference/#layout-images}{images}.
#' This is especially convenient for embedding raster images on a plot in 
#' a self-contained fashion (i.e., so they don't depend on external URL links).
#' 
#' @param r an object coercable to a raster object via [as.raster()]
#' @param ... arguments passed onto [as.raster()].
#' @author Carson Sievert
#' @export
#' @references <https://plotly-r.com/embedding-images.html>
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' # a red gradient (from ?as.raster)
#' r <- as.raster(matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5))
#' plot(r)
#' 
#' # embed the raster as an image
#' plot_ly(x = 1, y = 1) %>% 
#'   layout(
#'     images = list(list(
#'      source = raster2uri(r),
#'      xref = "paper", 
#'      yref = "paper", 
#'      x = 0, y = 0, 
#'      sizex = 0.5, sizey = 0.5, 
#'      xanchor = "left", yanchor = "bottom"
#'   ))
#'  ) 

raster2uri <- function(r, ...) {
  try_library("png", "raster2uri")
  # should be 4 x n matrix
  if (inherits(r, "nativeRaster")) {
    # png::writePNG directly supports nativeRaster objects
    png <- r
  } else {
    r <- grDevices::as.raster(r, ...)
    rgbs <- col2rgb(c(r), alpha = T) / 255
    nr <- dim(r)[1]
    nc <- dim(r)[2]
    reds <- matrix(rgbs[1, ], nrow = nr, ncol = nc, byrow = TRUE)
    greens <- matrix(rgbs[2, ], nrow = nr, ncol = nc, byrow = TRUE)
    blues <- matrix(rgbs[3, ], nrow = nr, ncol = nc, byrow = TRUE)
    alphas <- matrix(rgbs[4, ], nrow = nr, ncol = nc, byrow = TRUE)
    png <- array(c(reds, greens, blues, alphas), dim = c(dim(r), 4))
  }
  base64enc::dataURI(png::writePNG(png), mime = "image/png")
}
