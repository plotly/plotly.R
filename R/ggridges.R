# Get data for ridge plots
#
# @param data dataframe, the data returned by `ggplot2::ggplot_build()`.
# @param na.rm boolean, from params
#
# @return dataframe containing plotting data 
#
get_ridge_data <- function(data, na.rm) {
  if (isTRUE(na.rm)) {
    data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
  }
  
  #if dataframe is empty there's nothing to draw
  if (nrow(data) == 0) return(list())
  
  # remove all points that fall below the minimum height
  data$ymax[data$height < data$min_height] <- NA
  
  # order data
  data <- data[order(data$ymin, data$x), ]
  
  # remove missing points
  missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
  ids <- cumsum(missing_pos) + 1
  data$group <- paste0(data$group, "-", ids)
  data[!missing_pos, ]
}


# Prepare plotting data for ggridges
# @param closed boolean, should the polygon be closed at bottom (TRUE for
#   geom_density_ridges2, FALSE for geom_density_ridges)
prepare_ridge_chart <- function(data, prestats_data, layout, params, p, closed = FALSE, ...) {
  d <- get_ridge_data(data, params$na.rm)
  
  # split data into separate groups
  groups <- split(d, factor(d$group))
  
  # sort list so lowest ymin values are in the front (opposite of ggridges)
  o <- order(
    unlist(
      lapply(
        groups,
        function(data) data$ymin[1]
      )
    ),
    decreasing = FALSE
  )
  groups <- groups[o]
  
  # for each group create a density + vline + point as applicable
  res <- lapply(
    rev(groups),
    function(x) {
      draw_stuff <- split(x, x$datatype)
      
      # first draw the basic density ridge part
      stopifnot(!is.null(draw_stuff$ridgeline))
      
      d2 <- d1 <- draw_stuff$ridgeline
      if (!closed) d2$colour <- NA # no colour for density bottom line
      
      d1$y <- d1$ymax
      d1$alpha <- 1 # don't use fill alpha for line alpha
      
      ridges <- list(
        to_basic(prefix_class(d2, "GeomDensity")),
        to_basic(prefix_class(d1, "GeomLine"))
      )
      # attach the crosstalk group/set
      ridges[[1]] <- structure(ridges[[1]], set = attr(d2, 'set')) # Density
      ridges[[2]] <- structure(ridges[[2]], set = attr(d1, 'set')) # Line
      
      if ('vline' %in% names(draw_stuff)) {
        draw_stuff$vline$xend <- draw_stuff$vline$x
        draw_stuff$vline$yend <- draw_stuff$vline$ymax
        draw_stuff$vline$y    <- draw_stuff$vline$ymin
        draw_stuff$vline$colour <- draw_stuff$vline$vline_colour
        draw_stuff$vline$size <- draw_stuff$vline$vline_size
        
        vlines <- to_basic(
          prefix_class(draw_stuff$vline, 'GeomSegment'), 
          prestats_data, layout, params, p, ...
        )
        # attach the crosstalk group/set
        vlines <- structure(vlines, set = attr(draw_stuff$vline, 'set'))
        ridges <- c(ridges, list(vlines))
      }
      
      # points
      if ('point' %in% names(draw_stuff)) {
        draw_stuff$point$y <- draw_stuff$point$ymin
        
        # use point aesthetics
        draw_stuff$point$shape  <- draw_stuff$point$point_shape
        draw_stuff$point$fill   <- draw_stuff$point$point_fill
        draw_stuff$point$stroke <- draw_stuff$point$point_stroke
        draw_stuff$point$alpha  <- draw_stuff$point$point_alpha
        draw_stuff$point$colour <- draw_stuff$point$point_colour
        draw_stuff$point$size   <- draw_stuff$point$point_size
        
        points <- to_basic(
          prefix_class(as.data.frame(draw_stuff$point), # remove ridge classes
                       'GeomPoint'), 
          prestats_data, layout, params, p, ...
        )
        # attach the crosstalk group/set
        points <- structure(points, set = attr(draw_stuff$point, 'set'))
        ridges <- c(ridges, list(points))
      }
      
      ridges
    }
  )
  res
}


#' @export
to_basic.GeomDensityRidgesGradient <- function(data, prestats_data, layout, params, p, ...) {
  res <- prepare_ridge_chart(data, prestats_data, layout, params, p, FALSE, ...)
  # set list depth to 1
  unlist(res, recursive = FALSE)
}


#' @export
to_basic.GeomDensityRidges <- function(data, prestats_data, layout, params, p, ...) {
  to_basic(
    prefix_class(data, 'GeomDensityRidgesGradient'),
    prestats_data, layout, params, p, 
    closed = FALSE, 
    ...
  )
}


#' @export
to_basic.GeomDensityRidges2 <- function(data, prestats_data, layout, params, p, ...) {
  to_basic(
    prefix_class(data, 'GeomDensityRidgesGradient'),
    prestats_data, layout, params, p, 
    closed = TRUE, 
    ...
  )
}



#' @export
to_basic.GeomDensityLine <- function(data, prestats_data, layout, params, p, ...) {
  to_basic(prefix_class(data, 'GeomDensity'))
}



#' @export
to_basic.GeomRidgeline <- function(data, prestats_data, layout, params, p, ...) {
  to_basic(
    prefix_class(data, 'GeomDensityRidgesGradient'),
    prestats_data, layout, params, p, ...
  )
}


#' @export
to_basic.GeomRidgelineGradient <- function(data, prestats_data, layout, params, p, ...) {
  d <- get_ridge_data(data, params$na.rm)
  
  # split data into separate groups
  groups <- split(d, factor(d$group))
  
  # sort list so lowest ymin values are in the front (opposite of ggridges)
  o <- order(
    unlist(
      lapply(
        groups,
        function(data) data$ymin[1]
      )
    ),
    decreasing = FALSE
  )
  groups <- groups[o]
  
  # for each group create a density + vline + point as applicable
  res <- lapply(
    rev(groups),
    function(x) {
      
      draw_stuff <- split(x, x$datatype)
      
      # first draw the basic density ridge part
      
      stopifnot(!is.null(draw_stuff$ridgeline))
      d2 <- d1 <- draw_stuff$ridgeline
      d2$colour <- NA # no colour for density area
      d2$fill_plotlyDomain <- NA
      
      d1$y <- d1$ymax
      d1$alpha <- 1 # don't use fill alpha for line alpha
      
      # calculate all the positions where the fill type changes
      fillchange <- c(FALSE, d2$fill[2:nrow(d2)] != d2$fill[1:nrow(d2)-1])
      
      # and where the id changes
      idchange <- c(TRUE, d2$group[2:nrow(d2)] != d2$group[1:nrow(d2)-1])
      
      # make new ids from all changes in fill style or original id
      d2$ids <- cumsum(fillchange | idchange)
      
      # get fill color for all ids
      fill <- d2$fill[fillchange | idchange]
      
      # rows to be duplicated
      dupl_rows <- which(fillchange & !idchange)
      d2$y <- d2$ymax
      if (length(dupl_rows) > 0) {
        rows <- d2[dupl_rows, ]
        rows$ids <- d2$ids[dupl_rows-1]
        rows <- rows[rev(seq_len(nrow(rows))), , drop = FALSE]
        # combine original and duplicated d2
        d2 <- rbind(d2, rows)
      }
      
      # split by group to make polygons
      d2 <- tibble::deframe(tidyr::nest(d2, .by = 'ids'))
      
      ridges <- c(
        d2,
        list(
          to_basic(prefix_class(d1, "GeomLine"))
        )
      )
      
      ridges
    }
  )
  # set list depth to 1
  unlist(res, recursive = FALSE)
}



#' @export
geom2trace.GeomRidgelineGradient <- function(data, params, p) {
  # munching for polygon
  positions <- data.frame(
    x = c(data$x   , rev(data$x)),
    y = c(data$ymax, rev(data$ymin))
  )

  L <- list(
    x          = positions[["x"]],
    y          = positions[["y"]],
    text       = uniq(data[["hovertext"]]),
    key        = data[["key"]],
    customdata = data[["customdata"]],
    frame      = data[["frame"]],
    ids        = positions[["ids"]],
    type       = "scatter",
    mode       = "lines",
    line       = list(
      width = aes2plotly(data, params, linewidth_or_size(GeomPolygon)),
      color = toRGB('black'),
      dash  = aes2plotly(data, params, "linetype")
    ),
    fill       = "toself",
    fillcolor  = toRGB(unique(data$fill[1])),
    hoveron    = hover_on(data)
  )
  compact(L)
}
