#' @export
to_basic.GeomRidgeline <- function(data, prestats_data, layout, params, p, ...) {
  d <- basic_ridge_data(data, params)
  d2 <- d1 <- d
  d1$y <- d1$ymax
  d2$colour <- NA
  list(
    prefix_class(d1, "GeomPath"),
    to_basic(prefix_class(d2, "GeomDensity"))
  )
}

#' @export
to_basic.GeomJoy <- to_basic.GeomRidgeline

#' @export
to_basic.GeomJoy2 <- function(data, prestats_data, layout, params, p, ...) {
  d <- basic_ridge_data(data, params, coord, layout)
  dp <- d
  dp$y <- dp$ymax
  list(
    prefix_class(dp, "GeomPath"),
    to_basic(prefix_class(d, "GeomDensity"))
  )
}

basic_ridge_data <- function(data, params, coord, panel_params) {
  if (isTRUE(params$na.rm)) {
    data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
  }
  
  #if dataframe is empty there's nothing to draw
  if (nrow(data) == 0) return(list())
  
  # remove all points that fall below the minimum height
  data$ymax[data$height < data$min_height] <- NA
  
  # Instead of removing NA values from the data and plotting a single
  # polygon, we want to "stop" plotting the polygon whenever we're
  # missing values and "start" a new polygon as soon as we have new
  # values.  We do this by creating an id vector for polygonGrob that
  # has distinct polygon numbers for sequences of non-NA values and NA
  # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
  # 4, 4, 4, NA)
  data <- data[order(data$ymin, data$x), ]
  missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
  ids <- cumsum(missing_pos) + 1
  data$group <- paste0(data$group, "-", ids)
  data[!missing_pos, ]
  
  ## TODO: perform munching within group?
  #positions <- plyr::summarise(
  #  data,
  #  x = c(x, rev(x)),
  #  y = c(ymax, rev(ymin)),
  #  id = c(ids, rev(ids))
  #)
  #munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)
  #
  ## munching for line
  #positions <- plyr::summarise(data, x = x, y = ymax, id = ids)
  #munched_line <- ggplot2::coord_munch(coord, positions, panel_params)
  #
  #list(polygon = munched_poly, line = munched_line)
}


prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
