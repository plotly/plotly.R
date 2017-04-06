#' Modify the colorbar
#' 
#' @param p a plotly object
#' @param ... arguments are documented here 
#' \url{https://plot.ly/r/reference/#scatter-marker-colorbar}.
#' @param limits numeric vector of length 2. Set the extent of the colorbar scale.
#' @param which colorbar to modify? Should only be relevant for subplots with 
#' multiple colorbars.
#' @author Carson Sievert
#' @export
#' @examples 
#' 
#' p <- plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~cyl)
#' 
#' # pass any colorbar attribute -- 
#' # https://plot.ly/r/reference/#scatter-marker-colorbar
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
        p$x$data <- lapply(p$x$data, function(x) {
          col <- x$marker[["color"]]
          x$marker[["color"]][col < limits[1] | limits[2] < col] <- NA
          x$marker[["cmin"]] <- limits[1]
          x$marker[["cmax"]] <- limits[2]
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
#' @seealso \code{\link{hide_legend}()}, \code{\link{hide_colorbar}()}
#'

hide_guides <- function(p) {
  hide_legend(hide_colorbar(p))
}


#' Hide color bar(s)
#' 
#' @param p a plotly object.
#' @export
#' @seealso \code{\link{hide_legend}()}
#' @examples
#' 
#' p <- plot_ly(mtcars, x = ~wt, y = ~cyl, color = ~cyl)
#' hide_colorbar(p)
#'   
hide_colorbar <- function(p) {
  p <- plotly_build(p)
  for (i in seq_along(p$x$data)) {
    trace <- p$x$data[[i]]
    if (has_attr(trace$type, "showscale")) {
      p$x$data[[i]]$showscale <- FALSE
    }
    if (has_attr(trace$type, "marker")) {
      p$x$data[[i]]$marker$showscale <- FALSE
    }
  }
  p
}

#' Hide legend
#' 
#' @param p a plotly object.
#' @export
#' @seealso \code{\link{hide_colorbar}()}
#' @examples 
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
#' @examples 
#' 
#' # currently no bargl trace type
#' toWebGL(qplot(1:10))
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
#' Useful when used with \code{\link{subplot}()}
#' 
#' @param ... arguments passed onto \code{\link{plot_ly}()}
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

#' #' Convenience function for running examples 
#' #' 
#' #' @ex 
#' 
#' run_example <- function(ex = "plotlyEvents") {
#'   ex_dir <- system.file("examples", package = "plotly")
#'   rmd_files <- dir(ex_dir, "\\.Rmd$", recursive = TRUE)
#'   r_files <- dir(ex_dir, "\\.R$", recursive = TRUE)
#'   #ex <- match.arg(ex, unique(dirname(c(rmd_files, r_files))))
#'   if (ex %in% dirname(rmd_files)) {
#'     dirName <- filepath(ex_dir, ex)
#'     owd <- setwd(dirName)
#'     on.exit(setwd(owd))
#'     rmarkdown::render(dirName)
#'     return(invisible())
#'   }
#'   
#' }
