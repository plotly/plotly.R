# layer -> trace conversion
layers2traces <- function(data, prestats_data, layout, p) {
  # Attach a "geom class" to each layer of data for method dispatch
  data <- Map(function(x, y) prefix_class(x, class(y$geom)[1]), data, p$layers)
  
  # Extract parameters (and "hovertext aesthetics") in each layer
  params <- Map(function(x, y) {
    param <- c(
      y[["computed_geom_params"]] %||% y[["geom_params"]], 
      y[["computed_stat_params"]] %||% y[["stat_params"]], 
      y[["aes_params"]], 
      position = ggtype(y, "position")
    )
    
    map <- getAesMap(p, y)
    
    # consider "calculated" aesthetics (e.g., density, count, etc)
    calc_aes <- y$stat$default_aes[ggfun("is_calculated_aes")(y$stat$default_aes)]
    calc_aes <- calc_aes[!names(calc_aes) %in% names(map)]
    
    map <- c(calc_aes, map)
    
    # turn symbol (e.g., ..count..) & call (e.g. calc(count)) mappings into text labels 
    map <- ggfun("make_labels")(map)


    # filter tooltip aesthetics down to those specified in `tooltip` arg 
    if (!identical(p$tooltip, "all")) {
      # rectify tooltips, ggplot automatically convert `color` => `colour`
      p$tooltip[p$tooltip == "color"] <- "colour"
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
      nm <- paste(sc$aesthetics, collapse = "_")
      discreteScales[[nm]] <- sc
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
  
  # ggplot2 >v3.4.2 (specifically #4879) moved the guides system to ggproto, 
  # which moved the location of the name->value fields
  guides <- if (inherits(p$guides, "ggproto")) p$guides$guides else p$guides
  aes_no_guide <- names(guides)[vapply(guides, identical, logical(1), "none")]
  
  for (i in seq_along(datz)) {
    d <- datz[[i]]
    # variables that produce multiple traces and deserve their own legend entries
    split_legend <- paste0(
      setdiff(names(discreteScales), aes_no_guide), 
      "_plotlyDomain"
    )
    # add variable that produce multiple traces, but do _not_ deserve entries
    split_by <- c(split_legend, aes_no_guide, "PANEL", "frame", split_on(d))
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
to_basic.GeomFunction <- function (data, prestats_data, layout, params, p, ...) {
   data$y <- params$fun(data$x)
   prefix_class(data, "GeomPath")
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
  data <- data[order(data[["x"]]), ]
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

  data <- sf::st_as_sf(data)
  geom_type <- sf::st_geometry_type(data)
  # st_cast should "expand" a collection into multiple rows (one per feature)
  if ("GEOMETRYCOLLECTION" %in% geom_type) {
    data <- sf::st_cast(data)
    geom_type <- sf::st_geometry_type(data)
  }
  
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
    d[[i]] <- remove_class(d[[i]], "sf")
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
  if (is.discrete(data$fill_plotlyDomain %||% NA_character_)) {
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
  nm <- linewidth_or_size(GeomHex)
  size <- data[[nm]]
  data[[nm]] <- ifelse(size < 1, size ^ (1 / 6), size ^ 6)
  x <- rep.int(hexC[["x"]], n) * rep(data[[nm]], each = 6) + rep(data[["x"]], each = 6)
  y <- rep.int(hexC[["y"]], n) * rep(data[[nm]], each = 6) + rep(data[["y"]], each = 6)
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
  lay <- tidyr::pivot_longer(
    data = layout$layout, cols = c("x_min", "x_max"), values_to = "x", names_to = "variable"
  ) 
  lay <- as.data.frame(lay)
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
  x <- if (inherits(p$coordinates, "CoordFlip")) "y" else "x"
  lay <- tidyr::pivot_longer(
    data = layout$layout, cols = paste0(x, c("_min", "_max")), values_to = x, names_to = "variable"
  ) 
  lay <- as.data.frame(lay)
  if (nrow(data) > 0) {
    data <- merge(lay[c("PANEL", x)], data, by = "PANEL")
  }
  data[["x"]] <- data[[x]]
  data[["y"]] <- data$yintercept
  prefix_class(data, c("GeomHline", "GeomPath"))
}

#' @export
to_basic.GeomVline <- function(data, prestats_data, layout, params, p, ...) {
  # ugh, we can't trust the group here
  data$group <- do.call(paste,
    data[!grepl("group", names(data)) & !vapply(data, anyNA, logical(1))]
  )
  y <- if (inherits(p$coordinates, "CoordFlip")) "x" else "y"
  lay <- tidyr::pivot_longer(
    data = layout$layout, cols = paste0(y, c("_min", "_max")), values_to = y, names_to = "variable"
  ) 
  lay <- as.data.frame(lay)
  if (nrow(data) > 0) {
    data <- merge(lay[c("PANEL", y)], data, by = "PANEL")
  }
  data[["y"]] <- data[[y]]
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
  data$width <- if (params[["flipped_aes"]]) {
    (data[["ymax"]] - data[["y"]]) /(data[["y_max"]] - data[["y_min"]])  
  } else {
    (data[["xmax"]] - data[["x"]]) /(data[["x_max"]] - data[["x_min"]])
  }
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
  data <- tidyr::pivot_longer(
    data = data, cols = c("ymin", "ymax"), values_to = "y", names_to = "recodeVariable"
  )
  data <- as.data.frame(data)
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
  middle <- base::transform(data, x = xmin, xend = xmax, yend = y, alpha = NA)
  nm <- linewidth_or_size(GeomCrossbar)
  data[[nm]] <- data[[nm]] * (params$fatten %||% formals(geom_crossbar)$fatten)
  list(
    prefix_class(to_basic.GeomRect(data), "GeomCrossbar"),
    prefix_class(to_basic.GeomSegment(middle), "GeomCrossbar")
  )
}
utils::globalVariables(c("xmin", "xmax", "y", "size", "linewidth", "COL", "PANEL", "ROW", "yaxis"))

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

# ggalluvial::GeomStratum
#' @export
to_basic.GeomStratum <- function(data, ...) {
  to_basic.GeomRect(data, ...)
}

# ggalluvial::GeomAlluvium
#' @export 
to_basic.GeomAlluvium <- function(data, ...) {
  # geom_alluvium by default generates a data.frame with a colour column and sets it to 0, which leads to an error when trying to get the colour from the number and grid::col2rgb complains that colors must be positive integers.
  cols <- unique(data$colour)
  if (length(cols) == 1 && cols[1] == 0) {
    data$colour <- NULL
  }
  
  data <- data[order(data$x), ]
  row_number <- nrow(data)
  data_rev <- data[rev(seq_len(row_number)), ]
  unused_aes <- setdiff(names(data), c("x", "y", "ymin", "ymax"))
  
  d <- structure(rbind(
    cbind(x = data$x, y = data$ymin, data[unused_aes]),
    cbind(x = data$x[row_number], y = data$ymin[row_number], data[row_number, unused_aes]),
    cbind(x = data_rev$x, y = data_rev$ymax, data_rev[unused_aes])
  ), class = class(data))
  
  prefix_class(d, "GeomPolygon") 
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
    customdata = data[["customdata"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "scatter",
    mode = "lines",
    name = if (inherits(data, "GeomSmooth")) "fitted values",
    line = list(
      # TODO: line width array? -- https://github.com/plotly/plotly.js/issues/147
      width = aes2plotly(data, params, linewidth_or_size(GeomPath))[1],
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
    customdata = data[["customdata"]],
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
  pch <- uniq(data$shape) %||% params$shape %||% GeomPoint$use_defaults(NULL)$shape
  if (any(idx <- pch %in% 21:25) || any(idx <- !is.null(data[["fill_plotlyDomain"]]))) {
    fill_value <- aes2plotly(data, params, "fill")
    if (length(idx) == 1) {
      L$marker$color <- fill_value
    } else {
      L$marker$color[idx] <- fill_value[idx]
    }
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
    customdata = data[["customdata"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "bar",
    # plotly.js v2.0 changed default to textposition='auto', meaning
    # text will display by default, which makes sense for plot_ly() maybe, 
    # but not ggplotly()
    # https://github.com/plotly/orca/issues/374
    textposition = "none",
    marker = list(
      autocolorscale = FALSE,
      color = toRGB(
        aes2plotly(data, params, "fill"),
        aes2plotly(data, params, "alpha")
      ),
      line = list(
        width = aes2plotly(data, params, linewidth_or_size(GeomBar)),
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
    customdata = data[["customdata"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "scatter",
    mode = "lines",
    line = list(
      width = aes2plotly(data, params, linewidth_or_size(GeomPolygon)),
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
  # marker styling must inherit from GeomPoint$default_aes
  # https://github.com/hadley/ggplot2/blob/ab42c2ca81458b0cf78e3ba47ed5db21f4d0fc30/NEWS#L73-L7
  point_defaults <- GeomPoint$use_defaults(NULL)
  compact(list(
    x = data[["x"]],
    y = data[["y"]],
    hoverinfo = "y",
    key = data[["key"]],
    customdata = data[["customdata"]],
    frame = data[["frame"]],
    ids = data[["ids"]],
    type = "box",
    fillcolor = toRGB(
      aes2plotly(data, params, "fill"),
      aes2plotly(data, params, "alpha")
    ),
    # markers/points
    marker = list(
      opacity = point_defaults$alpha,
      outliercolor = toRGB(point_defaults$colour),
      line = list(
        width = mm2pixels(point_defaults$stroke),
        color = toRGB(point_defaults$colour)
      ),
      size = mm2pixels(point_defaults$size)
    ),
    line = list(
      color = aes2plotly(data, params, "colour"),
      width = aes2plotly(data, params, linewidth_or_size(GeomBoxplot))
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
    customdata = data[["customdata"]],
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
    customdata = data[["customdata"]],
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
  # Support of bi-directional GeomErrorbar introduced with ggplot2 3.3.0
  # g <- ggplot() + geom_errorbar(aes(y = "A", xmin = 1, xmax = 2))
  # ggplotly(g)
# Support of bi-directional GeomErrorbar introduced with ggplot2 3.3.0:
# g <- ggplot() + geom_errorbar(aes(y = "A", xmin = 1, xmax = 2))
# ggplotly(g)
if (params[["flipped_aes"]]) { 
    make_error(data, params, "x")
  } else {
    make_error(data, params, "y")
  }
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
    GeomHline = c("linetype", "colour", "size", "linewidth"),
    GeomVline = c("linetype", "colour", "size", "linewidth"),
    GeomAbline = c("linetype", "colour", "size", "linewidth"),
    GeomPath = c("fill", "colour", "size", "linewidth"),
    GeomPolygon = c("fill", "colour", "size", "linewidth"),
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
  # if xy is NULL: set xy to mean of xy_min and xy_max
  data[[xy]] <- data[[xy]] %||% ((data[[paste0(xy, "min")]] + data[[paste0(xy, "max")]]) / 2)  
  color <- aes2plotly(data, params, "colour")
  e <- list(
    x = data[["x"]],
    y = data[["y"]],
    text = uniq(data[["hovertext"]]),
    key = data[["key"]],
    customdata = data[["customdata"]],
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
  if (n == 0) return(dat)
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
  
  vals <- uniq(data[[aes]]) %||% params[[aes]]
  
  if (is.null(vals)) {
    # Hack to support this geom_sf hack 
    # https://github.com/tidyverse/ggplot2/blob/505e4bfb/R/sf.R#L179-L187
    defaults <- if (inherits(data, "GeomSf") && exists("default_aesthetics", envir = asNamespace("ggplot2"))) {
      type <- if (any(grepl("[P-p]oint", class(data)))) "point" else if (any(grepl("[L-l]ine", class(data)))) "line" else ""
      ggfun("default_aesthetics")(type)
    } else {
      geom_obj <- ggfun(geom)
      # If the first class of `data` is a data.frame,
      # ggfun() returns a function because ggplot2 now
      # defines data.frame in it's namespace
      # https://github.com/ropensci/plotly/pull/1481
      if ("default_aes" %in% names(geom_obj)) geom_obj$use_defaults(NULL) else NULL
    }
    vals <- defaults[[aes]]
  }
  vals <- vals %||% NA
  
  converter <- switch(
    aes, 
    size = mm2pixels,
    linewidth = mm2pixels,
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


# ggplot2 3.4.0 deprecated size in favor of linewidth in line-based geoms (e.g.,
# GeomLine, GeomRect, etc) and elements (e.g., element_line(), element_rect(),
# etc). Note that, some geoms (e.g., GeomBoxplot, GeomSf) can have both 
# linewidth and size
linewidth_or_size <- function(x) {
  UseMethod("linewidth_or_size")
}

#' @export
linewidth_or_size.Geom <- function(x) {
  if ("linewidth" %in% x$aesthetics()) "linewidth" else "size"
}

#' @export
linewidth_or_size.element <- function(x) {
  if ("linewidth" %in% names(x)) "linewidth" else "size"
}

#' @export
linewidth_or_size.default <- function(x) {
  if (get_package_version("ggplot2") >= "3.4") "linewidth" else "size"
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
