# layer -> trace conversion
layers2traces <- function(data, prestats_data, layout, p) {
  # Attach a "geom class" to each layer of data for method dispatch
  data <- Map(function(x, y) prefix_class(x, class(y$geom)[1]), data, p$layers)
  
  # Extract parameters (and "hovertext aesthetics") in each layer
  params <- Map(function(x, y) {
    param <- c(
      y[["geom_params"]], y[["stat_params"]], y[["aes_params"]], 
      position = ggtype(y, "position")
    )
    
    # consider "calculated" aesthetics (e.g., density, count, etc)
    calc_aes <- y$stat$default_aes[ggfun("is_calculated_aes")(y$stat$default_aes)]
    map <- c(y$mapping, calc_aes)
    
    # add on plot-level mappings, if they're inherited
    if (isTRUE(y$inherit.aes)) map <- c(map, p$mapping)
    
    # turn symbol (e.g., ..count..) & call (e.g. calc(count)) mappings into text labels 
    map <- ggfun("make_labels")(map)

    # filter tooltip aesthetics down to those specified in `tooltip` arg 
    if (!identical(p$tooltip, "all")) {
      map <- map[names(map) %in% p$tooltip | map %in% p$tooltip]
    }
    
    # throw out positional coordinates if we're hovering on fill
    if (identical("fills", hover_on(x))) {
      map <- map[!names(map) %in% c("x", "xmin", "xmax", "y", "ymin", "ymax")]
    }
    
    # disregard geometry mapping in hovertext for GeomSf
    if ("GeomSf" %in% class(y$geom)) {
      map <- map[!names(map) %in% "geometry"]
    }
    
    param[["hoverTextAes"]] <- map
    param
  }, data, p$layers)
  
  hoverTextAes <- lapply(params, "[[", "hoverTextAes")
  # attach a new column (hovertext) to each layer of data
  # (mapped to the text trace property)
  data <- Map(function(x, y) {
    if (nrow(x) == 0) return(x)
    # make sure the relevant aes exists in the data
    for (i in seq_along(y)) {
      aesName <- names(y)[[i]]
      if (!aesName %in% names(x)) next
      # TODO: should we be getting the name from scale_*(name) first?
      varName <- y[[i]]
      # "automatically" generated group aes is not informative
      if (identical("group", unique(varName, aesName))) next
      # add a line break if hovertext already exists
      if ("hovertext" %in% names(x)) x$hovertext <- paste0(x$hovertext, br())
      # text aestheic should be taken verbatim (for custom tooltips)
      prefix <- if (identical(aesName, "text")) "" else paste0(varName, ": ")
      # look for the domain, if that's not found, provide the range (useful for identity scales)
      txt <- x[[paste0(aesName, "_plotlyDomain")]] %||% x[[aesName]]
      suffix <- tryNULL(format(txt, justify = "none")) %||% ""
      # put the height of the bar in the tooltip
      if (inherits(x, "GeomBar") && identical(aesName, "y")) {
        suffix <- format(x[["ymax"]] - x[["ymin"]], justify = "none")
      }
      x$hovertext <- paste0(x$hovertext, prefix, suffix)
    }
    x$hovertext <- x$hovertext %||% ""
    x
  }, data, hoverTextAes)
  
  # draw legends only for discrete scales
  discreteScales <- list()
  for (sc in p$scales$non_position_scales()$scales) {
    if (sc$is_discrete()) {
      discreteScales[[sc$aesthetics]] <- sc
    }
  }
  # Convert "high-level" geoms to their "low-level" counterpart
  # This may involve preprocessing the data, for example:
  # 1. geom_line() is really geom_path() with data sorted by x
  # 2. geom_smooth() is really geom_path() + geom_ribbon()
  datz <- list()
  paramz <- list()
  for (i in seq_along(data)) {
    # This has to be done in a loop, since some layers are really two layers,
    # (and we need to replicate the data/params in those cases)
    set <- attr(data[[i]], "set")
    d <- to_basic(data[[i]], prestats_data[[i]], layout, params[[i]], p)
    d <- structure(d, set = set)
    if (is.data.frame(d)) d <- list(d)
    for (j in seq_along(d)) {
      datz <- c(datz, d[j])
      paramz <- c(paramz, params[i])
    }
  }
  # now to the actual layer -> trace conversion
  trace.list <- list()
  for (i in seq_along(datz)) {
    d <- datz[[i]]
    # variables that produce multiple traces and deserve their own legend entries
    split_legend <- paste0(names(discreteScales), "_plotlyDomain")
    # add variable that produce multiple traces, but do _not_ deserve entries
    split_by <- c(split_legend, "PANEL", "frame", split_on(d))
    # ensure the factor level orders (which determines traces order)
    # matches the order of the domain values
    split_vars <- intersect(split_by, names(d))
    lvls <- unique(d[split_vars])
    lvls <- lvls[do.call(order, lvls), , drop = FALSE]
    separator <- new_id()
    fac <- factor(
      apply(d[split_vars], 1, paste, collapse = separator),
      levels = unique(apply(lvls, 1, paste, collapse = separator))
    )
    if (all(is.na(fac))) fac <- 1
    dl <- split(d, fac, drop = TRUE)
    # list of traces for this layer
    trs <- Map(geom2trace, dl, paramz[i], list(p))
    # attach the crosstalk group/set
    trs <- Map(function(x, y) { x$set <- attr(y, "set"); x}, trs, dl)
    # if we need a legend, set name/legendgroup/showlegend
    # note: this allows us to control multiple traces from one legend entry
    if (any(split_legend %in% names(d))) {
      nms <- strsplit(names(trs), separator, fixed = TRUE)
      nms <- vapply(nms, function(x) {
        y <- unique(x[seq_along(split_legend)])
        if (length(y) > 1) paste0("(", paste(y, collapse = ","), ")") else y
      }, character(1))
      trs <- Map(function(x, y) {
        x$name <- y
        x$legendgroup <- y
        # depending on the geom (e.g. smooth) this may be FALSE already
        x$showlegend <- x$showlegend %||% TRUE
        x
      }, trs, nms)
    } else {
      trs <- lapply(trs, function(x) { x$showlegend <- FALSE; x })
    }
    
    # each trace is with respect to which axis?
    for (j in seq_along(trs)) {
      panel <- unique(dl[[j]]$PANEL)
      trs[[j]]$xaxis <-  sub("axis", "", layout$layout[panel, "xaxis"])
      trs[[j]]$yaxis <-  sub("axis", "", layout$layout[panel, "yaxis"])
    }
    trace.list <- c(trace.list, trs)
  }
  trace.list
}


#' Convert a geom to a "basic" geom.
#'
#' This function makes it possible to convert ggplot2 geoms that
#' are not included with ggplot2 itself. Users shouldn't need to use
#' this function. It exists purely to allow other package authors to write
#' their own conversion method(s).
#'
#' @param data the data returned by `ggplot2::ggplot_build()`.
#' @param prestats_data the data before statistics are computed.
#' @param layout the panel layout.
#' @param params parameters for the geom, statistic, and 'constant' aesthetics
#' @param p a ggplot2 object (the conversion may depend on scales, for instance).
#' @param ... currently ignored
#' @export
to_basic <- function(data, prestats_data, layout, params, p, ...) {
  UseMethod("to_basic")
}

#' @export
to_basic.GeomCol <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomBar")
}

#' @export
to_basic.GeomViolin <- function(data, prestats_data, layout, params, p, ...) {
  n <- nrow(data)
  revData <- data[order(data[["y"]], decreasing = TRUE), ]
  idx <- !names(data) %in% c("x", "xmin", "xmax")
  data <- rbind(
    cbind(x = data[["x"]] - data$violinwidth / 2, data[, idx]),
    cbind(x = revData[["x"]] + revData$violinwidth / 2, revData[, idx])
  )
  if (!is.null(data$hovertext)) data$hovertext <- paste0(data$hovertext, br())
  data$hovertext <- paste0(
    data$hovertext, "density: ", format(data$density, justify = "none")
  )
  prefix_class(data, c("GeomPolygon", "GeomViolin"))
}

#' @export
to_basic.GeomBoxplot <- function(data, prestats_data, layout, params, p, ...) {
  aez <- names(GeomBoxplot$default_aes)
  for (i in aez) {
    prestats_data[[i]] <- NULL
  }
  vars <- c("PANEL", "group", "key", aez, grep("_plotlyDomain$", names(data), value = T))
  prefix_class(
    merge(prestats_data, data[names(data) %in% vars], by = c("PANEL", "group"), sort = FALSE),
    "GeomBoxplot"
  )
}

#' @export
to_basic.GeomSmooth <- function(data, prestats_data, layout, params, p, ...) {
  if (nrow(data) == 0) {
    return(prefix_class(data, "GeomBlank"))
  }
  dat <- prefix_class(data, "GeomPath")
  # alpha for the path is always 1 (see GeomSmooth$draw_key)
  dat$alpha <- 1
  if (!identical(params$se, FALSE)) {
    dat2 <- prefix_class(ribbon_dat(data), c("GeomPolygon", "GeomSmooth"))
    dat2$colour <- NULL
    dat <- list(dat, dat2)
  }
  dat
}

#' @export
to_basic.GeomRibbon <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(ribbon_dat(data), "GeomPolygon")
}

#' @export
to_basic.GeomArea <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(ribbon_dat(data), "GeomPolygon")
}

#' @export
to_basic.GeomDensity <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(ribbon_dat(data), "GeomPolygon")
}

#' @export
to_basic.GeomLine <- function(data, prestats_data, layout, params, p, ...) {
  data <- data[order(data[["x"]]), ]
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomStep <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomSegment <- function(data, prestats_data, layout, params, p, ...) {
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
to_basic.GeomRect <- function(data, prestats_data, layout, params, p, ...) {
  data$group <- seq_len(nrow(data))
  others <- data[!names(data) %in% c("xmin", "ymin", "xmax", "ymax", "y", "x")]
  dat <- with(data, {
    rbind(cbind(x = xmin, y = ymin, others),
          cbind(x = xmin, y = ymax, others),
          cbind(x = xmax, y = ymax, others),
          cbind(x = xmax, y = ymin, others))
  })
  prefix_class(dat, c("GeomPolygon", "GeomRect"))
}

#' @export
to_basic.GeomSf <- function(data, prestats_data, layout, params, p, ...) {
  
  data[["geometry"]] <- sf::st_sfc(data[["geometry"]])
  data <- sf::st_as_sf(data, sf_column_name = "geometry")
  geom_type <- sf::st_geometry_type(data)
  # st_cast should "expand" a collection into multiple rows (one per feature)
  if ("GEOMETRYCOLLECTION" %in% geom_type) {
    data <- sf::st_cast(data)
    geom_type <- sf::st_geometry_type(data)
  }
  data <- remove_class(data, "sf")
  
  basic_type <- dplyr::recode(
    as.character(geom_type),
    TRIANGLE = "GeomPolygon",
    TIN = "GeomPolygon",
    POLYHEDRALSURFACE = "GeomPolygon",
    SURFACE = "GeomPolygon",
    CURVE = "GeomPath",
    MULTISURFACE = "GeomPolygon",
    MULTICURVE = "GeomPath",
    CURVEPOLYGON = "GeomPolygon",
    COMPOUNDCURVE = "GeomPath",
    CIRCULARSTRING = "GeomPath",
    MULTIPOLYGON = "GeomPolygon",
    MULTILINESTRING = "GeomPath",
    MULTIPOINT = "GeomPoint",
    POLYGON = "GeomPolygon",
    LINESTRING = "GeomPath",
    POINT = "GeomPoint"
  )
  
  # return a list of data frames...one for every geometry (a la, GeomSmooth)
  d <- split(data, basic_type)
  for (i in seq_along(d)) {
    d[[i]] <- prefix_class(
      fortify_sf(d[[i]]), c(names(d)[[i]], "GeomSf")
    )
  }
  if (length(d) == 1) d[[1]] else d
}

#' @export
to_basic.GeomMap <- function(data, prestats_data, layout, params, p, ...) {
  common <- intersect(data$map_id, params$map$id)
  data <- data[data$map_id %in% common, , drop = FALSE]
  map <- params$map[params$map$id %in% common, , drop = FALSE]
  # TODO: do we need coord_munch() as in GeomMap$draw_panel()
  data$id <- data$map_id
  data$map_id <- NULL
  data$group <- NULL
  data <- merge(data, map, by = "id", sort = FALSE)
  data$group <- interaction(data[names(data) %in% c("PANEL", "group", "id")])
  prefix_class(data, c("GeomPolygon", "GeomMap"))
}

#' @export
to_basic.GeomAnnotationMap <- function(data, prestats_data, layout, params, p, ...) {
  # TODO: we could/should? reduce this data down to the panel limits, but 
  # probably more effort than it's worth
  d <- params$map
  
  # add hovertext
  hasRegion <- isTRUE(p$tooltip %in% c("all", "region"))
  hasSubRegion <- isTRUE(p$tooltip %in% c("all", "subregion"))
  d$hovertext <- d$hovertext %||% paste0(
    if (hasRegion) d$region, if (hasSubRegion) paste0(br(), d$subregion)
  )
  prefix_class(d, c("GeomPolygon", "GeomAnnotationMap"))
}

#' @export
to_basic.GeomRaster <- function(data, prestats_data, layout, params, p, ...) {
  data <- prefix_class(data, "GeomTile")
  to_basic(data, prestats_data, layout, params)
}

#' @export
to_basic.GeomRasterAnn <- function(data, prestats_data, layout, params, p, ...) {
  # rasters are handled in ggplotly.R since they are layout specific
  prefix_class(data, "GeomBlank")
}

#' @export
to_basic.GeomTile <- function(data, prestats_data, layout, params, p, ...) {
  # geom2trace.GeomTile is a heatmap, which requires continuous fill
  if (is.discrete(prestats_data$fill)) {
    data <- prefix_class(data, "GeomRect")
    to_basic(data, prestats_data, layout, params, p)
  } else {
    data
  }
}

#' @export
to_basic.GeomHex <- function(data, prestats_data, layout, params, p, ...) {
  # see ggplot2:::hexGrob
  dx <- resolution(data[["x"]], FALSE)
  dy <- resolution(data[["y"]], FALSE)/sqrt(3)/2 * 1.15
  hexC <- hexbin::hexcoords(dx, dy, n = 1)
  n <- nrow(data)
  data$size <- ifelse(data$size < 1, data$size ^ (1 / 6), data$size ^ 6)
  x <- rep.int(hexC[["x"]], n) * rep(data$size, each = 6) + rep(data[["x"]], each = 6)
  y <- rep.int(hexC[["y"]], n) * rep(data$size, each = 6) + rep(data[["y"]], each = 6)
  data <- data[rep(seq_len(n), each = 6), ]
  data[["x"]] <- x
  data[["y"]] <- y
  data$group <- rep(seq_len(n), each = 6)
  prefix_class(data, c("GeomPolygon", "GeomHex"))
}

#' @export
to_basic.GeomContour <- function(data, prestats_data, layout, params, p, ...) {
  if (!"fill" %in% names(data)) data$fill <- NA
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomDensity2d <- function(data, prestats_data, layout, params, p, ...) {
  if ("hovertext" %in% names(data)) {
    data$hovertext <- paste0(data$hovertext, br())
  }
  data$hovertext <- paste0(
    data$hovertext, "Level: ", format(data$level,  justify = "none")
  )
  if (!"fill" %in% names(data)) data$fill <- NA
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomAbline <- function(data, prestats_data, layout, params, p, ...) {
  # ugh, we can't trust the group here
  data$group <- interaction(
    data[!grepl("group", names(data)) & !vapply(data, anyNA, logical(1))]
  )
  lay <- tidyr::gather_(layout$layout, "variable", "x", c("x_min", "x_max"))
  data <- merge(lay[c("PANEL", "x")], data, by = "PANEL")
  data[["y"]] <- with(data, intercept + slope * x)
  prefix_class(data, c("GeomHline", "GeomPath"))
}

#' @export
to_basic.GeomHline <- function(data, prestats_data, layout, params, p, ...) {
  # ugh, we can't trust the group here
  data$group <- do.call(paste,
    data[!grepl("group", names(data)) & !vapply(data, anyNA, logical(1))]
  )
  lay <- tidyr::gather_(layout$layout, "variable", "x", c("x_min", "x_max"))
  data <- merge(lay[c("PANEL", "x")], data, by = "PANEL")
  data[["y"]] <- data$yintercept
  prefix_class(data, c("GeomHline", "GeomPath"))
}

#' @export
to_basic.GeomVline <- function(data, prestats_data, layout, params, p, ...) {
  # ugh, we can't trust the group here
  data$group <- do.call(paste,
    data[!grepl("group", names(data)) & !vapply(data, anyNA, logical(1))]
  )
  lay <- tidyr::gather_(layout$layout, "variable", "y", c("y_min", "y_max"))
  data <- merge(lay[c("PANEL", "y")], data, by = "PANEL")
  data[["x"]] <- data$xintercept
  prefix_class(data, c("GeomVline", "GeomPath"))
}

#' @export
to_basic.GeomJitter <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomPoint")
}


#' @export
to_basic.GeomErrorbar <- function(data, prestats_data, layout, params, p, ...) {
  # width for ggplot2 means size of the entire bar, on the data scale
  # (plotly.js wants half, in pixels)
  data <- merge(data, layout$layout, by = "PANEL", sort = FALSE)
  data$width <- (data[["xmax"]] - data[["x"]]) /(data[["x_max"]] - data[["x_min"]])
  data$fill <- NULL
  prefix_class(data, "GeomErrorbar")
}

#' @export
to_basic.GeomErrorbarh <- function(data, prestats_data, layout, params, p, ...) {
  # height for ggplot2 means size of the entire bar, on the data scale
  # (plotly.js wants half, in pixels)
  data <- merge(data, layout$layout, by = "PANEL", sort = FALSE)
  data$width <- (data[["ymax"]] - data[["y"]]) / (data[["y_max"]] - data[["y_min"]])
  data$fill <- NULL
  prefix_class(data, "GeomErrorbarh")
}

#' @export
to_basic.GeomLinerange <- function(data, prestats_data, layout, params, p, ...) {
  
  if (!is.null(data[["y"]])) {
    data$width <- 0
    return(prefix_class(data, "GeomErrorbar"))
  }
  
  # reshape data so that x/y reflect path data
  data$group <- seq_len(nrow(data))
  data <- tidyr::gather_(data, "recodeVariable", "y", c("ymin", "ymax"))
  data <- data[order(data$group), ]
  # fix the hovertext (by removing the "irrelevant" aesthetic)
  recodeMap <- p$mapping[dplyr::recode(data[["recodeVariable"]], "ymax" = "ymin", "ymin" = "ymax")]
  data$hovertext <- Map(function(x, y) { 
    paste(x[!grepl(y, x)], collapse = br())
  }, strsplit(data$hovertext, br()), paste0("^", recodeMap, ":"))
  
  prefix_class(data, "GeomPath")
}

#' @export
to_basic.GeomPointrange <- function(data, prestats_data, layout, params, p, ...) {
  data$width <- 0
  list(
    prefix_class(data, "GeomErrorbar"),
    prefix_class(data, "GeomPoint")
  )
}

#' @export
to_basic.GeomDotplot <- function(data, prestats_data, layout, params, p, ...) {
  if (identical(params$binaxis, "y")) {
    dotdia <- params$dotsize * data$binwidth[1]/(layout$layout$y_max - layout$layout$y_min)
    data$size <- as.numeric(grid::convertHeight(grid::unit(dotdia, "npc"), "mm")) / 2
    data$x <- (data$countidx - 0.5) * (as.numeric(dotdia) * 6)
  } else {
    dotdia <- params$dotsize * data$binwidth[1]/(layout$layout$x_max - layout$layout$x_min)
    data$size <- as.numeric(grid::convertWidth(grid::unit(dotdia, "npc"), "mm")) / 2
    # TODO: why times 6?!?!
    data$y <- (data$countidx - 0.5) * (as.numeric(dotdia) * 6)
  }
  prefix_class(data, "GeomPoint")
}

#' @export
to_basic.GeomSpoke <- function(data, prestats_data, layout, params, p, ...) {
  # if radius/angle are a constant, still add them to the hovertext
  # NOTE: it'd be more accurate, but more complicated, to use the aes mapping
  for (var in c("radius", "angle")) {
    if (length(unique(data[[var]])) != 1) next
    data[["hovertext"]] <- paste0(
      data[["hovertext"]], br(), var, ": ", format(data[[var]], justify = "none")
    )
  }
  prefix_class(to_basic.GeomSegment(data), "GeomSpoke")
}

#' @export
to_basic.GeomCrossbar <- function(data, prestats_data, layout, params, p, ...) {
  # from GeomCrossbar$draw_panel()
  middle <- base::transform(data, x = xmin, xend = xmax, yend = y, size = size * params$fatten, alpha = NA)
  list(
    prefix_class(to_basic.GeomRect(data), "GeomCrossbar"),
    prefix_class(to_basic.GeomSegment(middle), "GeomCrossbar")
  )
}
utils::globalVariables(c("xmin", "xmax", "y", "size"))

#' @export
to_basic.GeomRug  <- function(data, prestats_data, layout, params, p, ...) {
  # allow the tick length to vary across panels
  layout <- layout$layout
  layout$tickval_y <- 0.03 * abs(layout$y_max - layout$y_min)
  layout$tickval_x <- 0.03 * abs(layout$x_max - layout$x_min)
  data <- merge(data, layout[c("PANEL", "x_min", "x_max", "y_min", "y_max", "tickval_y", "tickval_x")])
  
  # see GeomRug$draw_panel()
  rugs <- list()
  sides <- params$sides
  others <- data[!names(data) %in% c("x", "y")]
  if (!is.null(data[["x"]])) {
    if (grepl("b", sides)) {
      rugs$b <- with(
        data, data.frame(
          x = x, 
          xend = x,
          y = y_min, 
          yend = y_min + tickval_y,
          others
        )
      )
    }
    if (grepl("t", sides)) {
      rugs$t <- with(
        data, data.frame(
          x = x, 
          xend = x,
          y = y_max - tickval_y, 
          yend = y_max,
          others
        )
      )
    }
  }
  if (!is.null(data[["y"]])) {
    if (grepl("l", sides)) {
      rugs$l <- with(
        data, data.frame(
          x = x_min, 
          xend = x_min + tickval_x,
          y = y, 
          yend = y,
          others
        )
      )
    }
    if (grepl("r", sides)) {
      rugs$r <- with(
        data, data.frame(
          x = x_max - tickval_x, 
          xend = x_max,
          y = y, 
          yend = y,
          others
        )
      )
    }
  }
  
  lapply(rugs, function(d) {
    prefix_class(to_basic.GeomSegment(d), "GeomRug")
  })
}

#' @export
to_basic.GeomQuantile <- function(data, prestats_data, layout, params, p, ...){
  dat <- split(data, data$quantile)
  dat <- lapply(dat, prefix_class, y = "GeomPath")
  dat
}

#' @export
to_basic.default <- function(data, prestats_data, layout, params, p, ...) {
  data
}

#' Convert a "basic" geoms to a plotly.js trace.
#'
#' This function makes it possible to convert ggplot2 geoms that
#' are not included with ggplot2 itself. Users shouldn't need to use
#' this function. It exists purely to allow other package authors to write
#' their own conversion method(s).
#'
#' @param data the data returned by `plotly::to_basic`.
#' @param params parameters for the geom, statistic, and 'constant' aesthetics
#' @param p a ggplot2 object (the conversion may depend on scales, for instance).
#' @export
geom2trace <- function(data, params, p) {
  if (nrow(data) == 0) return(geom2trace.GeomBlank(data, params, p))
  UseMethod("geom2trace")
}

#' @export
geom2trace.GeomBlank <- function(data, params, p) {
  list(visible = FALSE)
}

#' @export
geom2trace.GeomPath <- function(data, params, p) {
  data <- group2NA(data)
  L <- list(
    x = data[["x"]],
    y = data[["y"]],
    text = uniq(data[["hovertext"]]),
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "scatter",
    mode = "lines",
    name = if (inherits(data, "GeomSmooth")) "fitted values",
    line = list(
      # TODO: line width array? -- https://github.com/plotly/plotly.js/issues/147
      width = aes2plotly(data, params, "size")[1],
      color = toRGB(
        aes2plotly(data, params, "colour"),
        aes2plotly(data, params, "alpha")
      ),
      dash = aes2plotly(data, params, "linetype")
    ),
    hoveron = hover_on(data)
  )
  if (inherits(data, "GeomStep")) L$line$shape <- params$direction %||% "hv"
  compact(L)
}

#' @export
geom2trace.GeomPoint <- function(data, params, p) {
  shape <- aes2plotly(data, params, "shape")
  color <- aes2plotly(data, params, "colour")
  isDotPlot <- inherits(data, "GeomDotplot")
  L <- list(
    x = data[["x"]],
    y = data[["y"]],
    text = if (isDotPlot) data[["key"]] else uniq(data[["hovertext"]]),
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "scatter",
    mode = "markers",
    marker = list(
      autocolorscale = FALSE,
      color = color,
      opacity = aes2plotly(data, params, "alpha"),
      size = aes2plotly(data, params, "size"),
      symbol = shape,
      line = list(
        width = aes2plotly(data, params, "stroke"),
        color = color
      )
    ),
    hoveron = hover_on(data)
  )
  # fill is only relevant for pch %in% 21:25
  pch <- uniq(data$shape) %||% params$shape %||% GeomPoint$default_aes$shape
  if (any(idx <- pch %in% 21:25) || any(idx <- !is.null(data[["fill_plotlyDomain"]]))) {
    L$marker$color[idx] <- aes2plotly(data, params, "fill")[idx]
  }
  compact(L)
}

#' @export
geom2trace.GeomBar <- function(data, params, p) {
  # TODO: does position play a role here?
  #pos <- params$position %||% "stack"
  flip <- inherits(p$coordinates, "CoordFlip")
  
  if (!flip) {
    width <- with(data, xmax - xmin)
    # TODO: does this cause rounding issues when inverse transforming for dynamicTicks?
    x <- with(data, (xmax + xmin) / 2)
    base <- data[["ymin"]]
    y <- with(data, ymax - ymin)
  } else {
    width <- with(data, xmax - xmin)
    # TODO: does this cause rounding issues when inverse transforming for dynamicTicks?
    y <- with(data, (xmax + xmin) / 2)
    base <- data[["ymin"]]
    x <- with(data, ymax - ymin)
  }

  compact(list(
    orientation = if (flip) "h" else "v",
    width = width,
    base = base,
    x = x,
    y = y,
    text = uniq(data[["hovertext"]]),
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "bar",
    marker = list(
      autocolorscale = FALSE,
      color = toRGB(
        aes2plotly(data, params, "fill"),
        aes2plotly(data, params, "alpha")
      ),
      line = list(
        width = aes2plotly(data, params, "size"),
        color = aes2plotly(data, params, "colour")
      )
    )
  ))
}

#' @export
geom2trace.GeomPolygon <- function(data, params, p) {

  data <- group2NA(data)
  
  L <- list(
    x = data[["x"]],
    y = data[["y"]],
    text = uniq(data[["hovertext"]]),
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "scatter",
    mode = "lines",
    line = list(
      width = aes2plotly(data, params, "size"),
      color = toRGB(
        aes2plotly(data, params, "colour"),
        aes2plotly(data, params, "alpha")
      ),
      dash = aes2plotly(data, params, "linetype")
    ),
    fill = "toself",
    fillcolor = toRGB(
      aes2plotly(data, params, "fill"),
      aes2plotly(data, params, "alpha")
    ),
    hoveron = hover_on(data)
  )
  if (inherits(data, "GeomSmooth")) L$hoverinfo <- "x+y"
  if (inherits(data, "GeomCrossbar")) L$hoverinfo <- "none"
  compact(L)
}

#' @export
geom2trace.GeomBoxplot <- function(data, params, p) {
  compact(list(
    x = data[["x"]],
    y = data[["y"]],
    hoverinfo = "y",
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
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
  ))
}


#' @export
geom2trace.GeomText <- function(data, params, p) {
  compact(list(
    x = data[["x"]],
    y = data[["y"]],
    text = data[["label"]],
    hovertext = data[["hovertext"]],
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    textfont = list(
      # TODO: how to translate fontface/family?
      size = aes2plotly(data, params, "size"),
      color = toRGB(
        aes2plotly(data, params, "colour"),
        aes2plotly(data, params, "alpha")
      )
    ),
    type = "scatter",
    mode = "text",
    hoveron = hover_on(data)
  ))
}

#' @export
geom2trace.GeomTile <- function(data, params, p) {
  x <- sort(unique(data[["x"]]))
  y <- sort(unique(data[["y"]]))
  # make sure we're dealing with a complete grid
  g <- expand.grid(x = x, y = y)
  g$order <- seq_len(nrow(g))
  g <- merge(g, data, by = c("x", "y"), all.x = TRUE)
  g <- g[order(g$order), ]
  # put fill domain on 0-1 scale for colorscale purposes
  g$fill_plotlyDomain <- scales::rescale(g$fill_plotlyDomain)
  # create the colorscale
  colScale <- unique(g[, c("fill_plotlyDomain", "fill")])
  # colorscale goes crazy if there are NAs
  colScale <- colScale[stats::complete.cases(colScale), ]
  colScale <- colScale[order(colScale$fill_plotlyDomain), ]
  compact(list(
    x = x,
    y = y,
    z = matrix(g$fill_plotlyDomain, nrow = length(y), ncol = length(x), byrow = TRUE),
    text = matrix(g$hovertext, nrow = length(y), ncol = length(x), byrow = TRUE),
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    colorscale = setNames(colScale, NULL),
    type = "heatmap",
    showscale = FALSE,
    autocolorscale = FALSE
  ))
}

#' @export
geom2trace.GeomErrorbar <- function(data, params, p) {
  make_error(data, params, "y")
}

#' @export
geom2trace.GeomErrorbarh <- function(data, params, p) {
  make_error(data, params, "x")
}

#' @export
geom2trace.default <- function(data, params, p) {
  warning(
    "geom_", class(data)[1], "() has yet to be implemented in plotly.\n",
    "  If you'd like to see this geom implemented,\n",
    "  Please open an issue with your example code at\n",
    "  https://github.com/ropensci/plotly/issues"
  )
  list()
}

# ---------------------------------------------------------------------------
# Utility functions
# --------------------------------------------------------------------------

# given a geom, should we split on any continuous variables?
# this is necessary for some geoms, for example, polygons
# since plotly.js can't draw two polygons with different fill in a single trace
split_on <- function(dat) {
  lookup <- list(
    GeomHline = c("linetype", "colour", "size"),
    GeomVline = c("linetype", "colour", "size"),
    GeomAbline = c("linetype", "colour", "size"),
    GeomPath = c("fill", "colour", "size"),
    GeomPolygon = c("fill", "colour", "size"),
    GeomBar = "fill",
    GeomBoxplot = c("colour", "fill", "size"),
    GeomErrorbar = "colour",
    GeomErrorbarh = "colour",
    GeomText = "colour"
  )
  # try to split on the domain (for sensible trace ordering)
  for (i in names(lookup)) {
    domainName <- paste0(lookup[[i]], "_plotlyDomain")
    idx <- domainName %in% names(dat)
    lookup[[i]][idx] <- domainName[idx]
  }
  # search all the classes for relevant splits (moving from specific->generic) 
  splits <- NULL
  for (i in class(dat)) {
    splits <- splits %||% lookup[[i]]
  }
  # if hovering on fill, we need to split on hovertext
  if (identical(hover_on(dat), "fills")) {
    splits <- c(splits, "hovertext")
  }
  # make sure the variable is in the data, and is non-constant
  splits <- splits[splits %in% names(dat)]
  # is there more than one unique value for this aes split in the data?
  for (i in splits) {
    if (length(unique(dat[, i])) < 2) {
      splits <- setdiff(splits, i)
    }
  }
  splits
}

# given a geom, are we hovering over points or fill?
hover_on <- function(data) {
  if (inherits(data, c("GeomHex", "GeomRect", "GeomMap", "GeomMosaic", "GeomAnnotationMap")) ||
      # is this a "basic" polygon?
      identical("GeomPolygon", grep("^Geom", class(data), value = T))) {
    "fills"
  } else {
    "points"
  }
}

# make trace with errorbars
make_error <- function(data, params, xy = "x") {
  color <- aes2plotly(data, params, "colour")
  e <- list(
    x = data[["x"]],
    y = data[["y"]],
    text = uniq(data[["hovertext"]]),
    key = data[["key"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "scatter",
    mode = "lines",
    opacity = aes2plotly(data, params, "alpha"),
    line = list(color = "transparent")
  )
  e[[paste0("error_", xy)]] <- list(
    array = data[[paste0(xy, "max")]] - data[[xy]],
    arrayminus = data[[xy]] - data[[paste0(xy, "min")]],
    type = "data",
    width = data$width[1] / 2,
    symmetric = FALSE,
    color = color
  )
  compact(e)
}

# function to transform geom_ribbon data into format plotly likes
# (note this function is also used for geom_smooth)
ribbon_dat <- function(dat) {
  n <- nrow(dat)
  o <- order(dat[["x"]])
  o2 <- order(dat[["x"]], decreasing = TRUE)
  used <- c("x", "ymin", "ymax", "y")
  not_used <- setdiff(names(dat), used)
  # top-half of ribbon
  tmp <- dat[o, ]
  others <- tmp[not_used]
  dat1 <- cbind(x = tmp[["x"]], y = tmp[["ymin"]], others)
  dat1[n+1, ] <- data.frame(x = tmp[["x"]][n], y = tmp[["ymin"]][n], others[n, ])
  # bottom-half of ribbon
  tmp2 <- dat[o2, ]
  others2 <- tmp2[not_used]
  dat2 <- cbind(x = tmp2[["x"]], y = tmp2[["ymax"]], others2)
  structure(rbind(dat1, dat2), class = oldClass(dat))
}

aes2plotly <- function(data, params, aes = "size") {
  geom <- class(data)[1]
  
  # Hack to support this geom_sf hack 
  # https://github.com/tidyverse/ggplot2/blob/505e4bfb/R/sf.R#L179-L187
  defaults <- if (inherits(data, "GeomSf")) {
    type <- if (any(grepl("point", class(data)))) "point" else if (any(grepl("line", class(data)))) "line" else ""
    ggfun("default_aesthetics")(type)
  } else {
    ggfun(geom)$default_aes
  }
  
  vals <- uniq(data[[aes]]) %||% params[[aes]] %||% defaults[[aes]] %||% NA
  converter <- switch(
    aes, 
    size = mm2pixels, 
    stroke = mm2pixels, 
    colour = toRGB, 
    fill = toRGB, 
    linetype = lty2dash,
    shape = pch2symbol,
    alpha = function(x) { x[is.na(x)] <- 1; x },
    width = function(x) { x / 2},
    height = function(x) { x / 2}
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
  x <- as.character(x)
  idx <- x %in% names(lookup)
  if (any(idx)) {
    x[idx] <- lookup[x[idx]]
  }
  as.character(x)
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
    "2262" = "dashdot" ,
    "12223242" = "dashdot" ,
    "F282" = "dash",
    "F4448444" = "dash",
    "224282F2" = "dash",
    "F1" = "dash"
  )
  x <- as.character(x)
  idx <- x %in% names(lookup)
  if (any(idx)) {
    x[idx] <- lookup[x[idx]]
  }
  as.character(x)
}
