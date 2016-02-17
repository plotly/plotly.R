# layer -> trace conversion
layers2traces <- function(data, prestats_data, layers) {
  # attach a "geom class" to each layer of data for method dispatch 
  data <- Map(function(x, y) prefix_class(x, class(y$geom)[1]), data, layers)
  # transform special case geoms into their "basic" counterpart
  # (e.g., geom_line() is really geom_path() with data sorted by x)
  data <- Map(to_basic, data, prestats_data)
  # each ggplot2 layer can be comprised of one or more plotly.js traces
  # so, we split each layer of data into chunks (one chunk per trace)
  data <- lapply(data, function(x) {
    idx <- names(x) %in% c(markSplit[[class(x)[1]]], "PANEL")
    idx <- idx & !sapply(x, anyNA)
    s <- interaction(as.list(x[idx]))
    split(x, s, drop = TRUE)
  })
  params <- lapply(layers, function(x) {
    y <- c(x$geom_params, x$stat_params)
    y[!duplicated(y)]
  })
  # convert each layer to a list of traces
  trace.list <- lapply(data, function(x) lapply(x, geom2trace, params))
  # attach axis anchors for each trace
  Map(function(x, y) { 
    Map(function(s, t) { 
      s$xaxis <- unique(t$xaxis); s$yaxis <- unique(t$yaxis); s 
    }, x, y)
  }, trace.list, data)
}

# ---------------------------------------------------------------------------
#' Convert a geom, which are special cases of other geoms, to "basic" geoms.
#' --------------------------------------------------------------------------

to_basic <- function(data, prestats_data, ...) {
  UseMethod("to_basic")
}

#' @export
to_basic.GeomViolin <- function(data, prestats_data, ...) {
  warning(
    "plotly.js does not yet support violin plots. \n",
    "Converting to boxplot instead.",
    call. = FALSE
  )
  to_basic.GeomBoxplot(data, prestats_data)
}

#' @export
to_basic.GeomBoxplot <- function(data, prestats_data, ...) {
  # Preserve default colour values using fill:
  if (!is.null(data$fill)) {
    prestats_data$fill <- NULL
    dat <- unique(data[c("x", "fill")])
    prestats_data <- plyr::join(prestats_data, dat, by = "x")
  }
  prefix_class(prestats_data, "GeomBoxplot")
}

##' @export
#to_basic.GeomSmooth <- function(data, prestats_data, ...) {
#  list(
#    prefix_class(data, "GeomPath"),
#    prefix_class(data, "GeomRibbon")
#  )
#}

#' @export
to_basic.GeomSegment <- function(data, prestats_data, ...) {
  # Every row is one segment, we convert to a line with several
  # groups which can be efficiently drawn by adding NA rows.
  data$group <- seq_len(nrow(data))
  others <- data[!names(data) %in% c("x", "y", "xend", "yend")]
  data <- with(data, {
    rbind(cbind(x, y, others),
          cbind(x = xend, y = yend, others))
  })
  group2NA(data, "GeomPath")
}

#' @export
to_basic.GeomRect <- function(data, prestats_data, ...) {
  data$group <- seq_len(nrow(data))
  others <- data[!names(data) %in% c("xmin", "ymin", "xmax", "ymax")]
  g$data <- with(data, {
    rbind(cbind(x = xmin, y = ymin, others),
          cbind(x = xmin, y = ymax, others),
          cbind(x = xmax, y = ymax, others),
          cbind(x = xmax, y = ymin, others))
  })
  replace_class(data, "GeomPolygon", "GeomRect")
}

#' @export
to_basic.GeomRibbon <- function(data, prestats_data, ...) {
  data <- ribbon_dat(data)
  replace_class(data, "GeomPolygon", "GeomRibbon")
}

#' @export
to_basic.GeomPath <- function(data, prestats_data, ...) {
  group2NA(data)
}

#' @export
to_basic.GeomLine <- function(data, prestats_data, ...) {
  data <- group2NA(data[order(data$x), ])
  replace_class(data, "GeomPath", "GeomLine")
}

#' @export
to_basic.GeomBar <- function(data, prestats_data, ...) {
  data <- group2NA(data)
  data[!is.na(data$y), ]
}

#' @export
to_basic.GeomContour <- function(data, prestats_data, ...) {
  prefix_class(prestats_data, "GeomContour")
}

#' @export
to_basic.GeomDensity <- function(data, prestats_data, ...) {
  replace_class(data, "GeomArea", "GeomDensity")
}

#' @export
to_basic.GeomDensity2d <- function(data, prestats_data, ...) {
  prefix_class(prestats_data, "GeomDensity2d")
}

#' @export
to_basic.GeomAbline <- function(data, prestats_data, ...) {
  N <- nrow(data)
  m <- data$slope
  b <- data$intercept
  xmin <- min(prestats_data$globxmin, na.rm = T)
  xmax <- max(prestats_data$globxmax, na.rm = T)
  data$plotly_id <- seq_len(N)
  l <- list()
  for (i in seq_len(N)) {
    # the NAs tell plotly to draw different traces for each line
    l$x <- c(l$x, xmin, xmax, NA) 
    l$y <- c(l$y, xmin * m[i] + b[i], xmax * m[i] + b[i], NA)
    l$plotly_id <- c(l$plotly_id, rep(i, 3))
  }
  data <- plyr::join(data, data.frame(l), by = "plotly_id")
  data <- group2NA(data)
  replace_class(data, "GeomPath", "GeomAbline")
}

#' @export
to_basic.GeomHline <- function(data, prestats_data, ...) {
  N <- nrow(data)
  yint <- data$yintercept
  if (is.factor(data$x)) {
    s <- sort(data$x)
    xmin <- as.character(s[1])
    xmax <- as.character(s[length(s)])
  } else {
    xmin <- min(prestats_data$globxmin, na.rm = TRUE)
    xmax <- max(prestats_data$globxmax, na.rm = TRUE)
  }
  data$plotly_id <- seq_len(N)
  l <- list()
  for (i in seq_len(N)) {
    l$x <- c(l$x, xmin, xmax, NA) 
    l$y <- c(l$y, yint[i], yint[i], NA)
    l$plotly_id <- c(l$plotly_id, rep(i, 3))
  }
  data <- plyr::join(data, data.frame(l), by = "plotly_id")
  data <- group2NA(data)
  replace_class(data, "GeomPath", "GeomHline")
}

#' @export
to_basic.GeomVline <- function(data, prestats_data, ...) {
  N <- nrow(data)
  xint <- data$xintercept
  if (is.factor(data$y)) {
    s <- sort(data$y)
    ymin <- as.character(s[1])
    ymax <- as.character(s[length(s)])
  } else {
    ymin <- min(prestats_data$globymin, na.rm = TRUE)
    ymax <- max(prestats_data$globymax, na.rm = TRUE)
  }
  data$plotly_id <- seq_len(N)
  l <- list()
  for (i in seq_len(N)) {
    l$x <- c(l$x, xint[i], xint[i], NA) 
    l$y <- c(l$y, ymin, ymax, NA)
    l$plotly_id <- c(l$plotly_id, rep(i, 3))
  }
  data <- plyr::join(data, data.frame(l), by = "plotly_id")
  data <- group2NA(data)
  replace_class(data, "GeomPath", "GeomVline")
}

#' @export
to_basic.GeomJitter <- function(data, prestats_data, ...) {
  if ("size" %in% names(data)) {
    params$sizemin <- min(prestats_data$globsizemin)
    params$sizemax <- max(prestats_data$globsizemax)
  }
  replace_class(data, "GeomPoint", "GeomJitter")
}

#' @export
to_basic.GeomPoint <- function(data, prestats_data, ...) {
  if (length(unique(data$size)) > 1 && is.null(data$text)) {
    data$text <- paste("size:", data$size)
  }
  data
}

#' @export
to_basic.default <- function(data, prestats_data, ...) {
  data
}

# ---------------------------------------------------------------------------
#' Convert "basic" geoms to plotly.js traces.
#' --------------------------------------------------------------------------
#' 
geom2trace <- function(data, params) {
  UseMethod("geom2trace")
}

#' @export
geom2trace.GeomBlank <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    name = params$name,
    text = data$text,
    type = "scatter",
    mode = "markers",
    marker = list(opacity = 0)
  )
}

#' @export
geom2trace.GeomPath <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    name = params$name,
    text = data$text,
    type = "scatter",
    mode = "lines",
    line = list(
      # TODO: 
      # (1) track plotly.js issue for line.fill
      # (2) What about alpha? Does that go in colour?
      width = mm2pixels(uniq(data$size %||% GeomPath$default_aes$size)),
      color = toRGB(uniq(data$colour %||% GeomPath$default_aes$colour)),
      dash = lty2dash(uniq(data$linetype %||% GeomPath$default_aes$linetype))
    )
  )
}

#' @export
geom2trace.GeomPoint <- function(data, params) {
  shape <- uniq(data$shape %||% GeomPoint$default_aes$shape)
  L <- list(
    x = data$x,
    y = data$y,
    text = as.character(data$text),
    type = "scatter",
    mode = "markers",
    marker = list(
      autocolorscale = FALSE,
      color = toRGB(uniq(data$fill %||% GeomPoint$default_aes$fill)),
      opacity = uniq(data$alpha %||% 1),
      size = mm2pixels(uniq(data$size %||% GeomPoint$default_aes$size)),
      symbol = pch2symbol(shape),
      line = list(
        width = mm2pixels(uniq(data$stroke %||% GeomPoint$default_aes$stroke)),
        color = toRGB(uniq(data$colour %||% GeomPoint$default_aes$colour))
      )
    )
  )
  # fill is only relevant for pch=15:20
  idx <- shape %in% 15:20
  if (any(idx)) {
    L$marker$color[idx] <- L$marker$line$color[idx]
  }
  # `marker.color` should actually be `marker.line.color` for pch != 
  # pch=32 is a transparent circle
  if (any(shape %in% 32)) {
    L$marker$opacity[shape %in% 32] <- 0
  }
  L
}

#' @export
geom2trace.GeomPolygon <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    name = params$name,
    text = data$text,
    type = "scatter",
    mode = "lines",
    line = list(
      # TODO: 
      # (1) track plotly.js issue for line.fill
      # (2) What about alpha? Does that go in colour?
      width = mm2pixels(uniq(data$size %||% GeomPath$default_aes$size)),
      color = toRGB(uniq(data$colour %||% GeomPath$default_aes$colour)),
      dash = lty2dash(uniq(data$linetype %||% GeomPath$default_aes$linetype))
    ),
    fill = "tozerox",
    fillcolor = toRGB(
      uniq(data$fill %||% GeomPolygon$default_aes$fill), 
      uniq(data$alpha %||% GeomPolygon$default_aes$alpha)
    )
  )
}


#' @export
geom2trace.GeomText <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    text = data$label,
    textfont = list(
      size = data$size %||% 12,
      color = data$colour
    ),
    type = "scatter",
    mode = "text"
  )
}

#' @export
geom2trace.GeomBar <- function(data, params) {
  
  # TODO: how to trigger orientation="h" for flipped coordinates?
  
  # # for stacked bar charts, plotly cumulates bar heights, but ggplot doesn't
  # if (layout$barmode == "stack") {
  #   # could speed up this function with environments or C/C++
  #   unStack <- function(vec) {
  #     n <- length(vec)
  #     if (n == 1) return(vec)
  #     seq.n <- seq_len(n)
  #     names(vec) <- seq.n
  #     vec <- sort(vec)
  #     for (k in seq(2, n)) {
  #       vec[k] <- vec[k] - sum(vec[seq(1, k-1)])
  #     }
  #     as.numeric(vec[as.character(seq.n)])
  #   }
  #   ys <- lapply(trace.list, "[[", "y")
  #   xs <- lapply(trace.list, "[[", "x")
  #   x.vals <- unique(unlist(xs))
  #   # if there are two or more y-values (for a particular x value),
  #   # then modify those y-values so they *add up* to the correct value(s)
  #   for (val in x.vals) {
  #     zs <- lapply(xs, function(x) which(x == val))
  #     ys.given.x <- Map(function(x, y) y[x], zs, ys)
  #     if (length(unlist(ys.given.x)) < 2) next
  #     st <- unStack(unlist(ys.given.x))
  #     lens <- sapply(ys.given.x, length)
  #     trace.seq <- seq_along(trace.list)
  #     ws <- split(st, rep(trace.seq, lens))
  #     for (tr in seq_along(ws)) {
  #       idx <- zs[[tr]]
  #       replacement <- ws[[tr]]
  #       if (length(idx) > 0  && length(replacement) > 0) 
  #         trace.list[[tr]]$y[idx] <- replacement
  #     }
  #   }
  # }
  
  
  x <- if ("x.name" %in% names(data)) data$x.name else data$x
  x <- to_milliseconds(x)
  # if there is more than one y-value for a particular combination of
  # x, PANEL, and group; then take the _max_ y.
  data$x <- x
  dat <- plyr::ddply(data, c("x", "PANEL", if ("group" %in% names(data)) "group"),
                     plyr::summarise, count = max(y))
  L <- list(
    x = dat$x,
    y = dat$count,
    type = "bar",
    # text only makes sense if no dimension reduction occurred
    text = if (nrow(dat) == nrow(data)) data$text else NULL,
    name = params$name,
    marker = list(color = toRGB(params$fill))
  )
  if (!is.null(params$colour)) {
    L$marker$line <- list(color = toRGB(params$colour))
    L$marker$line$width <- params$size %||% 1
  }
  if (!is.null(params$alpha)) L$opacity <- params$alpha
  L
}

#' @export
geom2trace.GeomStep <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    name = params$name,
    type = "scatter",
    mode = "lines",
    line = paramORdefault(params, aes2step, ggplot2::GeomPath$default_aes)
  )
}

#' @export
geom2trace.GeomBoxplot <- function(data, params) {
  list(
    y = data$y,
    name = params$name,
    type = "box",
    # TODO: translate marker styling for outliers!
    line = paramORdefault(params, aes2line, ggplot2::GeomBoxplot$default_aes),
    fillcolor = toRGB(params$fill %||% "white")
  )
}

#' @export
geom2trace.GeomTile <- function(data, params) {
  x <- unique(data$x)
  y <- unique(data$y)
  list(
    x = x,
    y = y,
    z = t(matrix(data$fill.name, nrow = length(x), ncol = length(y))),
    name = params$name,
    type = "heatmap",
    mode = "lines",
    line = paramORdefault(params, aes2line, ggplot2::GeomPath$default_aes)
  )
}

#' @export
geom2trace.GeomContour <- function(data, params) {
  x <- unique(data$x)
  y <- unique(data$y)
  list(
    x = x,
    y = y,
    z = t(matrix(data$z, nrow = length(x), ncol = length(y))),
    name = params$name,
    type = "contour",
    line = paramORdefault(params, aes2line, ggplot2::GeomPath$default_aes),
    contours = list(coloring = "lines")
  )
}

#' @export
geom2trace.GeomDensity2d <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    name = params$name,
    type = "histogram2dcontour",
    line = paramORdefault(params, aes2line, ggplot2::GeomPath$default_aes),
    contours = list(coloring = "lines")
  )
}

#' @export
geom2trace.GeomErrorbar <- function(data, params) {
  make.errorbar(data, params, "y")
}

#' @export
geom2trace.GeomErrorbarh <- function(data, params) {
  make.errorbar(data, params, "x")
}

#' @export
geom2trace.GeomArea <- function(data, params) {
  list(
    x = c(data$x[1], data$x, tail(data$x, n = 1)),
    y = c(0, data$y, 0),
    name = params$name,
    type = "scatter",
    line = paramORdefault(params, aes2line, ggplot2::GeomRibbon$default_aes),
    fill = "tozeroy",
    fillcolor = toRGB(params$fill %||% "grey20", params$alpha)
  )
}

#' @export
geom2trace.GeomSmooth <- function(data, params) {
  #TODO: how to two geoms?
  geom2trace.GeomPath(data, params)
}

#' @export
geom2trace.default <- function(data, params) {
  warning(
    "geom_", class(data)[1], "() has yet to be implemented in plotly.\n",
    "  If you'd like to see this geom implemented,\n",
    "  Please open an issue with your example code at\n",
    "  https://github.com/ropensci/plotly/issues"
  )
  NULL
}

# ---------------------------------------------------------------------------
#' Utility functions
#' --------------------------------------------------------------------------
#' 

#' Drawing ggplot2 geoms with a group aesthetic is most efficient in
#' plotly when we convert groups of things that look the same to
#' vectors with NA.
group2NA <- function(data) {
  poly.list <- split(data, data$group, drop = TRUE)
  is.group <- names(data) == "group"
  poly.na.list <- list()
  forward.i <- seq_along(poly.list)
  ## When group2NA is called on geom_polygon (or geom_rect, which is
  ## treated as a basic polygon), we need to retrace the first points
  ## of each group, see https://github.com/ropensci/plotly/pull/178
  retrace.first.points <- "polygon" %in% class(data)
  for (i in forward.i) {
    no.group <- poly.list[[i]][, !is.group, drop = FALSE]
    na.row <- no.group[1, ]
    na.row[, c("x", "y")] <- NA
    retrace.first <- if (retrace.first.points) {
      no.group[1,]
    }
    poly.na.list[[paste(i, "forward")]] <-
      rbind(no.group, retrace.first, na.row)
  }
  if (retrace.first.points) {
    backward.i <- rev(forward.i[-1])[-1]
    for (i in backward.i) {
      no.group <- poly.list[[i]][1, !is.group, drop = FALSE]
      na.row <- no.group[1, ]
      na.row[, c("x", "y")] <- NA
      poly.na.list[[paste(i, "backward")]] <- rbind(no.group, na.row)
    }
    if (length(poly.list) > 1) {
      first.group <- poly.list[[1]][1, !is.group, drop = FALSE]
      poly.na.list[["last"]] <- rbind(first.group, first.group)
    }
  }
  data <- do.call(rbind, poly.na.list)
  if (is.na(data$x[nrow(data)])) {
    data <- data[-nrow(data), ]
  }
  data
}

# Make a trace for geom_errorbar -> error_y or geom_errorbarh ->
# error_x.
make.errorbar <- function(data, params, xy){
  tr <- list(
    x = data$x,
    y = data$y,
    type = "scatter",
    mode = "none"
  )
  err.name <- paste0("error_", xy)
  min.name <- paste0(xy, "min")
  max.name <- paste0(xy, "max")
  e <- list(
    array = data[[max.name]] - data[[xy]],
    type = "data",
    width = params$width,
    symmetric = TRUE,
    color = toRGB(params$colour)
  )
  arrayminus <- data[[xy]] - data[[min.name]]
  if(!isTRUE(all.equal(e$array, arrayminus))){
    e$arrayminus <- arrayminus
    e$symmetric <- FALSE
  }
  tr[[err.name]] <- e    
  tr
}

# function to transform geom_ribbon data into format plotly likes
# (note this function is also used for geom_smooth)
ribbon_dat <- function(dat) {
  n <- nrow(dat)
  o <- order(dat$x)
  o2 <- order(dat$x, decreasing = TRUE)
  used <- c("x", "ymin", "ymax")
  not_used <- setdiff(names(dat), used)
  # top-half of ribbon
  tmp <- dat[o, ]
  others <- tmp[not_used]
  dat1 <- cbind(x = tmp$x, y = tmp$ymax, others)
  dat1[n+1, ] <- cbind(x = tmp$x[n], y = tmp$ymin[n], others[n, ])
  # bottom-half of ribbon
  tmp2 <- dat[o2, ]
  others2 <- tmp2[not_used]
  dat2 <- cbind(x = tmp2$x, y = tmp2$ymin, others2)
  rbind(dat1, dat2)
}


dat2params <- function(d) {
  params <- c(names(aesConverters), "fill")
  l <- as.list(d[names(d) %in% params])
  lapply(l, unique)
}

# Convert R pch point codes to plotly "symbol" codes.
pch2symbol <- function(x) {
  lookup <- list(
    "0" = "square-open",
    "1" = "circle-open",
    "2" = "triangle-up-open",
    "3" = "cross-thin-open",
    "4" = "x-thin-open",
    "5" = "diamond-open",
    "6" = "triangle-down-open",
    "7" = "square-x-open",
    "8" = "asterisk-open",
    "9" = "diamond-x-open",
    "10" = "circle-cross-open",
    "11" = "hexagram-open",
    "12" = "square-cross-open",
    "13" = "circle-x-open",
    "14" = "square-open-dot",
    "15" = "square",
    "16" = "circle",
    "17" = "triangle-up",
    "18" = "diamond",
    "19" = "circle",
    "20" = "circle",
    "21" = "circle",
    "22" = "square",
    "23" = "diamond",
    "24" = "triangle-up",
    "25" = "triangle-down",
    "32" = "circle",
    "35" = "hash-open",
    "42" = "asterisk-open",
    "43" = "cross-thin-open",
    "45" = "line-ew-open",
    "47" = "line-ne-open",
    "48" = "circle-open",
    "79" = "circle-open",
    "88" = "x-thin-open",
    "92" = "line-nw-open",
    "95" = "line-ew-open",
    "111" = "circle-open",
    "o" = "circle-open",
    "O" = "circle-open",
    "+" = "cross-thin-open"
  )
  as.character(lookup[as.character(x)])
}

# Convert R lty line type codes to plotly "dash" codes.
lty2dash <- function(x) {
  lookup <- list(
    "0" = "none",
    "1" = "solid",
    "2" = "dash",
    "3" = "dot",
    "4" = "dashdot",
    "5" = "longdash",
    "6" = "longdashdot",
    "blank" = "none",
    "solid" = "solid",
    "dashed" = "dash",
    "dotted" = "dot",
    "dotdash" = "dashdot",
    "longdash" = "longdash",
    "twodash" = "longdashdot",
    "22" = "dash",
    "42" = "dot",
    "44" = "dashdot",
    "13" = "longdash",
    "1343" = "longdashdot",
    "73" = "dash",
    "2262" = "dotdash",
    "12223242" = "dotdash",
    "F282" = "dash",
    "F4448444" = "dash",
    "224282F2" = "dash",
    "F1" = "dash"
  )
  as.character(lookup[as.character(x)])
}
