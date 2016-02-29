# layer -> trace conversion
layers2traces <- function(data, prestats_data, layers, layout, scales, labels) {
  # Attach a "geom class" to each layer of data for method dispatch 
  data <- Map(function(x, y) prefix_class(x, class(y$geom)[1]), data, layers)
  # Extract parameters for each layer
  params <- lapply(layers, function(x) {
    c(x$geom_params, x$stat_params, x$aes_params)
  })
  # Discrete non_position_scales require multiple traces per layer
  split_scales <- list()
  for (sc in scales$non_position_scales()$scales) {
    if (sc$is_discrete()) {
      split_scales[[sc$aesthetics]] <- sc
    }
  }
  # Convert "high-level" geoms to their "low-level" counterpart
  # This may involve preprocessing the data, for example:
  # 1. geom_line() is really geom_path() with data sorted by x
  # 2. geom_smooth() is really geom_path() + geom_ribbon()
  #
  # This has to be done in a loop, since some layers are really two layers, 
  # (we need to replicate the data/params in those cases)
  #
  # We also extract the domain/range of discrete mapping(s) for later use
  datz <- list()
  paramz <- list()
  keyz <- list()
  for (i in seq_along(data)) {
    psd <- prestats_data[[i]]
    key <- unique(psd[, names(psd) %in% names(split_scales), drop = FALSE])
    # this order (should) determine the ordering of traces (within layer)
    key <- key[do.call(order, key), , drop = FALSE]
    for (j in names(key)) {
      key[[paste0(j, "_range")]] <- scales$get_scales(j)$map_df(key)[[1]]
    }
    pm <- params[[i]]
    dat <- to_basic(data[[i]], psd, layout, pm)
    if (inherits(dat, "list")) {
      pm <- replicate(length(dat), pm, simplify = FALSE)
      key <- replicate(length(dat), key, simplify = FALSE)
    } else {
      dat <- list(dat)
      key <- list(key)
    }
    datz <- c(datz, dat)
    paramz <- c(paramz, pm)
    keyz <- c(keyz, key)
  }
  
  # now to the actual layer -> trace conversion
  trace.list <- list()
  for (i in seq_along(datz)) {
    d <- datz[[i]]
    # _always_ split on fill for polygons (plotly.js can't do two polygons
    # with different fill in a single trace)
    split_by <- c(names(split_scales), if (inherits(d, "GeomPolygon")) "fill")
    idx <- names(d) %in% c(split_by, "PANEL")
    idx <- idx & !sapply(d, anyNA)
    s <- interaction(as.list(d[idx]))
    # split layer data into a list of data frames 
    dl <- split(d, s, drop = TRUE)
    # list of traces for this layer
    trs <- lapply(dl, geom2trace, paramz)
    # attach name/legendgroup to this trace if appropriate
    if (length(trs) > 1 && length(split_by) > 0 && prod(dim(keyz[[i]])) > 0) {
      key <- keyz[[i]]
      valz <- Map(function(x, y) paste0(x, ": ", y), labels[split_by], key[split_by])
      entries <- Reduce(function(x, y) paste0(x, "<br>", y), valz)
      # build a dictionary for looking up the trace/entry names
      key_range <- key[grepl("_range$", names(key))]
      keys <- Reduce(function(x, y) paste0(x, ".", y), key_range)
      entries <- setNames(entries, keys)
      # strip off the trailing panel info
      names(trs) <- sub("\\.[0-9]+$", "", names(trs))
      # order traces to match the ordering of the scale mapping(s)
      trs <- compact(trs[names(entries)])
      for (k in names(trs)) {
        trs[[k]]$name <- entries[[k]]
        trs[[k]]$legendgroup <- entries[[k]]
      }
    } else {
      trs <- lapply(trs, function(x) { x$showlegend <- FALSE; x})
    }
    # finally, each trace is with respect to which axis?
    for (j in seq_along(trs)) {
      panel <- unique(dl[[j]]$PANEL)
      trs[[j]]$xaxis <- sub("axis", "", layout[panel, "xaxis"])
      trs[[j]]$yaxis <- sub("axis", "", layout[panel, "yaxis"])
    }
    trace.list <- c(trace.list, trs)
  }
  trace.list
}

# ---------------------------------------------------------------------------
#' Convert a geom, which are special cases of other geoms, to "basic" geoms.
#' --------------------------------------------------------------------------

to_basic <- function(data, prestats_data, layout, params, ...) {
  UseMethod("to_basic")
}

#' @export
to_basic.GeomViolin <- function(data, prestats_data, layout, params, ...) {
  # TODO: it should be possible to implement this via GeomPolygon
  # just need preprocess the data, then:
  # replace_class(data, "GeomPolygon",  "GeomViolin")
  warning(
    "plotly.js does not yet support violin plots. \n",
    "Converting to boxplot instead.",
    call. = FALSE
  )
  to_basic.GeomBoxplot(data, prestats_data)
}

#' @export
to_basic.GeomBoxplot <- function(data, prestats_data, layout, params, ...) {
  # 'trained' aesthetics that we're interested in mapping from data to prestats
  aez <- c("fill", "colour", "size", "alpha", "linetype", "shape", "x")
  dat <- data[names(data) %in% c(aez, "group")]
  pre <- prestats_data[!names(prestats_data) %in% aez]
  prefix_class(merge(pre, dat, by = "group", sort = FALSE), "GeomBoxplot")
}

#' @export
to_basic.GeomSmooth <- function(data, prestats_data, layout, params, ...) {
  dat <- replace_class(data, "GeomPath", "GeomSmooth")
  if (!identical(params$se, FALSE)) {
    data$colour <- NULL
    dat <- list(dat, prefix_class(ribbon_dat(data), "GeomPolygon"))
  }
  dat
}

#' @export
to_basic.GeomRibbon <- function(data, prestats_data, layout, params, ...) {
  prefix_class(ribbon_dat(data), "GeomPolygon")
}

#' @export
to_basic.GeomArea <- function(data, prestats_data, layout, params, ...) {
  prefix_class(ribbon_dat(data), "GeomPolygon")
}

#' @export
to_basic.GeomLine <- function(data, prestats_data, layout, params, ...) {
  data <- group2NA(data[order(data$x), ])
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomStep <- function(data, prestats_data, layout, params, ...) {
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomSegment <- function(data, prestats_data, layout, params, ...) {
  # Every row is one segment, we convert to a line with several
  # groups which can be efficiently drawn by adding NA rows.
  data$group <- seq_len(nrow(data))
  others <- data[!names(data) %in% c("x", "y", "xend", "yend")]
  data <- with(data, {
    rbind(cbind(x, y, others),
          cbind(x = xend, y = yend, others))
  })
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomRect <- function(data, prestats_data, layout, params, ...) {
  data$group <- seq_len(nrow(data))
  others <- data[!names(data) %in% c("xmin", "ymin", "xmax", "ymax")]
  g$data <- with(data, {
    rbind(cbind(x = xmin, y = ymin, others),
          cbind(x = xmin, y = ymax, others),
          cbind(x = xmax, y = ymax, others),
          cbind(x = xmax, y = ymin, others))
  })
  prefix_class(data, "GeomPolygon")
}

#' @export
to_basic.GeomRaster <- function(data, prestats_data, layout, params, ...) {
  # TODO: what if nrow(data) != nrow(prestats_data)?
  data$z <- prestats_data$fill
  if (is.discrete(prestats_data$fill)) {
    data <- prefix_class(data, "GeomRect")
    to_basic(data, prestats_data, layout, params)
  } else {
    prefix_class(data, "GeomTile")
  }
}

#' @export
to_basic.GeomTile <- function(data, prestats_data, layout, params, ...) {
  data$z <- prestats_data$fill
  if (is.discrete(prestats_data$fill)) {
    data <- prefix_class(data, "GeomRect")
    to_basic(data, prestats_data, layout, params)
  } else {
    data
  }
}

#' @export
to_basic.GeomContour <- function(data, prestats_data, layout, params, ...) {
  prefix_class(data, "GeomPolygon")
}

#' @export
to_basic.GeomDensity2d <- function(data, prestats_data, layout, params, ...) {
  prefix_class(data, "GeomPolygon")
}

#' @export
to_basic.GeomDensity <- function(data, prestats_data, layout, params, ...) {
  prefix_class(data, "GeomArea")
}

#' @export
to_basic.GeomAbline <- function(data, prestats_data, layout, params, ...) {
  data <- unique(data[c("PANEL", "intercept", "slope", "group")])
  data$group <- seq_len(nrow(data))
  lay <- tidyr::gather(layout, variable, x, xmin:xmax)
  data <- merge(lay[c("PANEL", "x")], data, by = "PANEL")
  data$y <- with(data, intercept + slope * x)
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomHline <- function(data, prestats_data, layout, params, ...) {
  data <- unique(data[c("PANEL", "yintercept", "group")])
  data$group <- seq_len(nrow(data))
  lay <- tidyr::gather(layout, variable, x, xmin:xmax)
  data <- merge(lay[c("PANEL", "x")], data, by = "PANEL")
  data$y <- data$yintercept
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomVline <- function(data, prestats_data, layout, params, ...) {
  data <- unique(data[c("PANEL", "xintercept", "group")])
  data$group <- seq_len(nrow(data))
  lay <- tidyr::gather(layout, variable, y, ymin:ymax)
  data <- merge(lay[c("PANEL", "y")], data, by = "PANEL")
  data$x <- data$xintercept
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomJitter <- function(data, prestats_data, layout, params, ...) {
  if ("size" %in% names(data)) {
    params$sizemin <- min(prestats_data$globsizemin)
    params$sizemax <- max(prestats_data$globsizemax)
  }
  prefix_class(data, "GeomPoint")
}

#' @export
to_basic.default <- function(data, prestats_data, params, ...) {
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
  list()
}

#' @export
geom2trace.GeomPath <- function(data, params) {
  data <- group2NA(data)
  L <- list(
    x = data$x,
    y = data$y,
    text = data$text,
    type = "scatter",
    mode = "lines",
    line = list(
      # TODO: line width array? -- https://github.com/plotly/plotly.js/issues/147
      width = aes2plotly(data, params, "size")[1],
      color = toRGB(
        aes2plotly(data, params, "colour"),
        aes2plotly(data, params, "alpha")
      ),
      dash = aes2plotly(data, params, "linetype")
    )
  )
  if (inherits(data, "GeomStep")) L$line$shape <- "hv"
  L
}

#' @export
geom2trace.GeomPoint <- function(data, params) {
  shape <- aes2plotly(data, params, "shape")
  if (length(unique(data$size)) > 1 && is.null(data$text)) {
    data$text <- paste("size:", data$size)
  }
  L <- list(
    x = data$x,
    y = data$y,
    text = data$text,
    type = "scatter",
    mode = "markers",
    marker = list(
      autocolorscale = FALSE,
      color = aes2plotly(data, params, "fill"),
      opacity = aes2plotly(data, params, "alpha"),
      size = aes2plotly(data, params, "size"),
      symbol = shape,
      line = list(
        width = aes2plotly(data, params, "stroke"),
        color = aes2plotly(data, params, "colour")
      )
    )
  )
  # for 'closed' shapes, marker color should inherit from line color
  idx <- !grepl("open", shape)
  L$marker$color[idx] <- L$marker$line$color[idx]
  L
}

#' @export
geom2trace.GeomBar <- function(data, params) {
  list(
    x = data$x,
    y = data$count %||% data$y,
    type = "bar",
    marker = list(
      autocolorscale = FALSE,
      color = aes2plotly(data, params, "fill"),
      opacity = aes2plotly(data, params, "alpha"),
      line = list(
        width = aes2plotly(data, params, "size"),
        color = aes2plotly(data, params, "colour")
      )
    )
  )
}

#' @export
geom2trace.GeomPolygon <- function(data, params) {
  data <- group2NA(data)
  # TODO: do this for more density-like measures??
  if ("level" %in% names(data)) {
    data$level <- paste("Level:", data$level)
  }
  list(
    x = data$x,
    y = data$y,
    text = data$text %||% data$level,
    type = "scatter",
    mode = "lines",
    line = list(
      # NOTE: line attributes must be constant on a polygon
      width = aes2plotly(data, params, "size"),
      color = aes2plotly(data, params, "colour"),
      dash = aes2plotly(data, params, "linetype")
    ),
    fill = "tozerox",
    fillcolor = toRGB(
      aes2plotly(data, params, "fill"),
      aes2plotly(data, params, "alpha")
    )
  )
  
}

#' @export
geom2trace.GeomBoxplot <- function(data, params) {
  list(
    x = data$x,
    y = data$y,
    type = "box",
    fillcolor = toRGB(
      aes2plotly(data, params, "fill"),
      aes2plotly(data, params, "alpha")
    ),
    # marker styling must inherit from GeomPoint$default_aes
    # https://github.com/hadley/ggplot2/blob/ab42c2ca81458b0cf78e3ba47ed5db21f4d0fc30/NEWS#L73-L77
    marker = list(
      opacity = GeomPoint$default_aes$alpha,
      outliercolor = toRGB(GeomPoint$default_aes$colour),
      line = list(
        width = mm2pixels(GeomPoint$default_aes$stroke),
        color = toRGB(GeomPoint$default_aes$colour)
      ),
      size = mm2pixels(GeomPoint$default_aes$size)
    ),
    line = list(
      color = aes2plotly(data, params, "colour"),
      width = aes2plotly(data, params, "size")
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
      # TODO: how to translate fontface/family?
      size = aes2plotly(data, params, "size"),
      color = toRGB(
        aes2plotly(data, params, "fill"),
        aes2plotly(data, params, "alpha")
      )
    ),
    type = "scatter",
    mode = "text"
  )
}

#' @export
geom2trace.GeomTile <- function(data, params) {
  x <- sort(unique(data$x))
  y <- sort(unique(data$y))
  # z should **always** be numeric (this is a heatmap!)
  data$z <- scales::rescale(data$z)
  which.rng <- c(which.min(data$z), which.max(data$z))
  list(
    x = x,
    y = y,
    z = matrix(data$z, nrow = length(x), ncol = length(y)),
    colorscale = setNames(data[which.rng, c("z", "fill")], NULL),
    type = "heatmap",
    showscale = FALSE,
    autocolorscale = FALSE
  )
}

#' @export
geom2trace.GeomErrorbar <- function(data, params) {
  make_errorbar(data, params, "y")
}

#' @export
geom2trace.GeomErrorbarh <- function(data, params) {
  make_errorbar(data, params, "x")
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
  if (!"group" %in% names(data)) return(data)
  poly.list <- split(data, data$group, drop = TRUE)
  is.group <- names(data) == "group"
  poly.na.list <- list()
  forward.i <- seq_along(poly.list)
  ## When group2NA is called on geom_polygon (or geom_rect, which is
  ## treated as a basic polygon), we need to retrace the first points
  ## of each group, see https://github.com/ropensci/plotly/pull/178
  retrace.first.points <- inherits(data, "GeomPolygon")
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
make_errorbar <- function(data, params, xy){
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
    width = data$width[1] %||% 0.5 / 2,
    symmetric = TRUE,
    color = toRGB(uniq(data$colour %||% "black"))
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

aes2plotly <- function(data, params, aes = "size") {
  # data can have multiple "geom classes" -- we want default_aes from the 
  # very first class
  geom <- rev(grep("^Geom", class(data), value = TRUE))[1]
  # modify %||% so that NA is considered NULL
  #"%|x|%" <- function(x, y) {
  #  if (length(x) == 1) {
  #    if (is.na(x)) x <- NULL
  #  }
  #  x %||% y
  #}
  vals <- uniq(data[[aes]]) %||% params[[aes]] %||% 
    ggfun(geom)$default_aes[[aes]] %||% NA
  converter <- switch(
    aes, 
    size = mm2pixels, 
    stroke = mm2pixels, 
    colour = toRGB, 
    fill = toRGB, 
    linetype = lty2dash,
    shape = pch2symbol,
    alpha = function(x) { x[is.na(x)] <- 1; x }
  )
  if (is.null(converter)) {
    warning("A converter for ", aes, " wasn't found. \n", 
            "Please report this issue to: \n",
            "https://github.com/ropensci/plotly/issues/new", call. = FALSE)
    converter <- identity
  }
  converter(vals)
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
