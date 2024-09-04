#' Convert ggplot2 to plotly
#'
#' This function converts a [ggplot2::ggplot()] object to a 
#' plotly object. 
#' 
#' @details Conversion of relative sizes depends on the size of the current 
#' graphics device (if no device is open, width/height of a new (off-screen) 
#' device defaults to 640/480). In other words, `height` and
#' `width` must be specified at runtime to ensure sizing is correct.
#' For examples on how to specify the output container's `height`/`width` in a 
#' shiny app, see `plotly_example("shiny", "ggplotly_sizing")`.
#' 
#'
#' @param p a ggplot object.
#' @param width Width of the plot in pixels (optional, defaults to automatic sizing).
#' @param height Height of the plot in pixels (optional, defaults to automatic sizing).
#' @param tooltip a character vector specifying which aesthetic mappings to show
#' in the tooltip. The default, "all", means show all the aesthetic mappings
#' (including the unofficial "text" aesthetic). The order of variables here will
#' also control the order they appear. For example, use
#' `tooltip = c("y", "x", "colour")` if you want y first, x second, and
#' colour last.
#' @param dynamicTicks should plotly.js dynamically generate axis tick labels? 
#' Dynamic ticks are useful for updating ticks in response to zoom/pan
#' interactions; however, they can not always reproduce labels as they 
#' would appear in the static ggplot2 image.
#' @param layerData data from which layer should be returned?
#' @param originalData should the "original" or "scaled" data be returned?
#' @param source a character string of length 1. Match the value of this string 
#' with the source argument in [event_data()] to retrieve the 
#' event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @param ... arguments passed onto methods.
#' @export
#' @author Carson Sievert
#' @references \url{https://plotly.com/ggplot2/}
#' @seealso [plot_ly()]
#' @examples \dontrun{
#' # simple example
#' ggpenguins <- qplot(bill_length_mm , body_mass_g, 
#' data = palmerpenguins::penguins, color = species)
#' ggplotly(ggpenguins)
#'
#' data(canada.cities, package = "maps")
#' viz <- ggplot(canada.cities, aes(long, lat)) +
#'   borders(regions = "canada") +
#'   coord_equal() +
#'   geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
#' ggplotly(viz, tooltip = c("text", "size"))
#' 
#' # linked scatterplot brushing
#' d <- highlight_key(mtcars)
#' qplot(data = d, x = mpg, y = wt) %>%
#'   subplot(qplot(data = d, x = mpg, y = vs)) %>% 
#'   layout(title = "Click and drag to select points") %>%
#'   highlight("plotly_selected")
#' 
#' 
#' # more brushing (i.e. highlighting) examples
#' demo("crosstalk-highlight-ggplotly", package = "plotly")
#' 
#' # client-side linked brushing in a scatterplot matrix
#' highlight_key(palmerpenguins::penguins) %>%
#'   GGally::ggpairs(aes(colour = Species), columns = 1:4) %>%
#'   ggplotly(tooltip = c("x", "y", "colour")) %>%
#'   highlight("plotly_selected")
#' }
#'
ggplotly <- function(p = ggplot2::last_plot(), width = NULL, height = NULL,
                     tooltip = "all", dynamicTicks = FALSE, 
                     layerData = 1, originalData = TRUE, source = "A", ...) {
  UseMethod("ggplotly", p)
}

#' @export
ggplotly.NULL <- function(...) {
  htmltools::browsable(htmltools::div(...))
}

#' @export
ggplotly.plotly <- function(p = ggplot2::last_plot(), width = NULL, height = NULL,
                            tooltip = "all", dynamicTicks = FALSE, 
                            layerData = 1, originalData = TRUE, source = "A", ...) {
  p
}

#' @export
ggplotly.ggmatrix <- function(p = ggplot2::last_plot(), width = NULL,
                              height = NULL, tooltip = "all", dynamicTicks = FALSE, 
                              layerData = 1, originalData = TRUE, source = "A", ...) {
  dots <- list(...)

  # provide a sensible crosstalk if none is already provided (makes ggnostic() work at least)
  if (!crosstalk_key() %in% names(p$data)) {
    p$data[[crosstalk_key()]] <- p$data[[".rownames"]] %||% seq_len(nrow(p$data))
    attr(p$data, "set") <- dots[["set"]] %||% new_id()
  }
  subplotList <- list()
  for (i in seq_len(p$ncol)) {
    columnList <- list()
    for (j in seq_len(p$nrow)) {
      thisPlot <- p[j, i]
      if (i == 1) {
        # should the first column contain axis labels?
        if (p$showYAxisPlotLabels %||% TRUE) thisPlot <- thisPlot + ylab(p$yAxisLabels[j])
      } else {
        # y-axes are never drawn on the interior, and diagonal plots are densities,
        # so it doesn't make sense to synch zoom actions on y
        thisPlot <- thisPlot + ylab(NULL) +
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          )
      }
      columnList <- c(
        columnList, list(ggplotly(
          thisPlot, tooltip = tooltip, dynamicTicks = dynamicTicks, 
          layerData = layerData, originalData = originalData, source = source,
          width = width, height = height
        ))
      )
    }
    # conditioned on a column in a ggmatrix, the x-axis should be on the
    # same scale.
    s <- subplot(columnList, nrows = p$nrow, margin = 0.01, shareX = TRUE,
                 titleY = TRUE, titleX = TRUE)
    subplotList <- c(subplotList, list(s))
  }
  s <- subplot(subplotList, nrows = 1, margin = 0.01, 
               titleY = TRUE, titleX = TRUE) %>% 
    hide_legend() %>%
    layout(dragmode = "select")
  if (robust_nchar(p$title) > 0) {
    s <- layout(s, title = p$title)
  }
  for (i in seq_along(p$xAxisLabels)) {
    s$x$layout[[sub("^xaxis1$", "xaxis", paste0("xaxis", i))]]$title <- p$xAxisLabels[[i]]
  }
  if (length(p$yAxisLabels)) {
    s$x$layout$margin$l <- s$x$layout$margin$l + 50
  }
  
  config(s)
}

#' @export
ggplotly.ggplot <- function(p = ggplot2::last_plot(), width = NULL,
                            height = NULL, tooltip = "all", dynamicTicks = FALSE,  
                            layerData = 1, originalData = TRUE, source = "A", ...) {
  l <- gg2list(p, width = width, height = height, tooltip = tooltip, 
               dynamicTicks = dynamicTicks, layerData = layerData, 
               originalData = originalData, source = source, ...)
  config(as_widget(l))
}

#' Convert a ggplot to a list.
#' @param p ggplot2 plot.
#' @param width Width of the plot in pixels (optional, defaults to automatic sizing).
#' @param height Height of the plot in pixels (optional, defaults to automatic sizing).
#' @param tooltip a character vector specifying which aesthetic tooltips to show in the
#' tooltip. The default, "all", means show all the aesthetic tooltips
#' (including the unofficial "text" aesthetic).
#' @param dynamicTicks accepts the following values: `FALSE`, `TRUE`, `"x"`, or `"y"`.
#' Dynamic ticks are useful for updating ticks in response to zoom/pan/filter
#' interactions; however, there is no guarantee they reproduce axis tick text 
#' as they would appear in the static ggplot2 image.
#' @param layerData data from which layer should be returned?
#' @param originalData should the "original" or "scaled" data be returned?
#' @param source a character string of length 1. Match the value of this string 
#' with the source argument in [event_data()] to retrieve the 
#' event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @param ... currently not used
#' @return a 'built' plotly object (list with names "data" and "layout").
#' @export
gg2list <- function(p, width = NULL, height = NULL, 
                    tooltip = "all", dynamicTicks = FALSE, 
                    layerData = 1, originalData = TRUE, source = "A", ...) {
  
  # To convert relative sizes correctly, we use grid::convertHeight(),
  # which requires a known output (device) size.
  dev_fun <- if (capabilities("aqua") || capabilities("png")) {
    grDevices::png
  } else if (capabilities("jpeg")) {
    grDevices::jpeg 
  } else if (is_installed("Cairo")) {
    function(filename, ...) Cairo::Cairo(file = filename, ...)
  } else {
    stop(
      "No Cairo or bitmap device is available. Such a graphics device is required to convert sizes correctly in ggplotly().\n\n", 
      " You have two options:\n",
      "  (1) install.packages('Cairo')\n",
      "  (2) compile R to use a bitmap device (png or jpeg)",
      call. = FALSE
    )
  }
  # if a device (or RStudio) is already open, use the device size as default size
  if (!is.null(grDevices::dev.list()) || is_rstudio()) {
    width <- width %||% default(grDevices::dev.size("px")[1])
    height <- height %||% default(grDevices::dev.size("px")[2])
  }
  # open the device and make sure it closes on exit
  dev_fun(filename = tempfile(), width = width %||% 640, height = height %||% 480)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  # check the value of dynamicTicks
  dynamicValues <- c(FALSE, TRUE, "x", "y")
  if (length(setdiff(dynamicTicks, dynamicValues))) {
   stop(
     sprintf(
       "`dynamicValues` accepts the following values: '%s'", 
       paste(dynamicValues, collapse = "', '")
     ), call. = FALSE
    )
  }
  
  # ------------------------------------------------------------------------
  # Our internal version of ggplot2::ggplot_build(). Modified from
  # https://github.com/hadley/ggplot2/blob/0cd0ba/R/plot-build.r#L18-L92
  # ------------------------------------------------------------------------
  ggplotly_build <- function(p) {
    plot <- ggfun("plot_clone")(p)
    
    if (length(plot$layers) == 0) {
      plot <- plot + geom_blank()
    }
    layers <- plot$layers
    layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
    
    # save crosstalk sets before this attribute gets squashed
    sets <- lapply(layer_data, function(y) attr(y, "set"))
    
    scales <- plot$scales
    
    # Apply function to layer and matching data
    by_layer <- function(f) {
      out <- vector("list", length(data))
      for (i in seq_along(data)) {
        out[[i]] <- f(l = layers[[i]], d = data[[i]])
      }
      out
    }
    
    # ggplot2 3.1.0.9000 introduced a Layer method named setup_layer() 
    # currently, LayerSf is the only core-ggplot2 Layer that makes use
    # of it https://github.com/tidyverse/ggplot2/pull/2875#issuecomment-438708426
    data <- layer_data
    if (get_package_version("ggplot2") > "3.1.0") {
      data <- by_layer(function(l, d) if (is.function(l$setup_layer)) l$setup_layer(d, plot) else d)
    }
    
    # Initialise panels, add extra data for margins & missing facetting
    # variables, and add on a PANEL variable to data
    layout <- ggfun("create_layout")(plot$facet, plot$coordinates)
    data <- layout$setup(data, plot$data, plot$plot_env)
    
    # save the domain of the group for display in tooltips
    groupDomains <- Map(function(x, y) {
      aes_g <- y$mapping[["group"]] %||% plot$mapping[["group"]]
      tryNULL(rlang::eval_tidy(aes_g, x))
    }, data, layers)
    
    # for simple (StatIdentity) geoms, add crosstalk key to aes mapping
    # (effectively adding it as a group)
    # later on, for more complicated geoms (w/ non-trivial summary statistics),
    # we construct a nested key mapping (within group)
    layers <- Map(function(x, y) {
      if (crosstalk_key() %in% names(y) && !"key" %in% names(x[["mapping"]]) && 
          inherits(x[["stat"]], "StatIdentity")) {
        # ggplot2 v3.3.4 started using the computed_mapping (instead of mapping)
        # field to inform the compute_aesthetics() method, so in order to add
        # the crosstalk key, we need to add to that field (when present)
        # https://github.com/tidyverse/ggplot2/pull/4475
        nm <- if ("computed_mapping" %in% names(x)) "computed_mapping" else "mapping"
        x[[nm]] <- c(x[[nm]], key = as.name(crosstalk_key()))
      }
      x
    }, layers, layer_data)
    
    # Compute aesthetics to produce data with generalised variable names
    data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
    if (exists("setup_plot_labels", envir = asNamespace("ggplot2"))) {
      # Mirror ggplot2/#5879
      plot$labels <- ggfun("setup_plot_labels")(plot, layers, data)
    }
    
    
    # add frame to group if it exists
    data <- lapply(data, function(d) { 
      if (!"frame" %in% names(d)) return(d)
      d$group <- with(d, paste(group, frame, sep = "-"))
      d
    })
    
    # The computed aesthetic codes the groups as integers
    # Here we build a map each of the integer values to the group label
    group_maps <- Map(function(x, y) {
      tryCatch({
        x_group <- x[["group"]]
        names(x_group) <- y
        x_group <- x_group[!duplicated(x_group)]
        x_group
      }, error = function(e) NULL
      )
    }, data, groupDomains)
    
    # Before mapping x/y position, save the domain (for discrete scales)
    # to display in tooltip.
    data <- lapply(data, function(d) {
      d[["x_plotlyDomain"]] <- d[["x"]]
      d[["y_plotlyDomain"]] <- d[["y"]]
      d
    })
    # And since we're essentially adding an "unknown" (to ggplot2) 
    # aesthetic, add it to the dropped_aes field to avoid fals positive
    # warnings (https://github.com/tidyverse/ggplot2/pull/4866)
    layers <- lapply(layers, function(l) {
      l$stat$dropped_aes <- c(l$stat$dropped_aes, "x_plotlyDomain")
      l$stat$dropped_aes <- c(l$stat$dropped_aes, "y_plotlyDomain")
      l
    })
    
    # Transform all scales
    data <- lapply(data, scales_transform_df, scales = scales)
    
    # Map and train positions so that statistics have access to ranges
    # and all positions are numeric
    scale_x <- function() scales$get_scales("x")
    scale_y <- function() scales$get_scales("y")
    
    layout$train_position(data, scale_x(), scale_y())
    
    data <- layout$map_position(data)
    
    # build a mapping between group and key
    # if there are multiple keys within a group, the key is a list-column
    reComputeGroup <- function(x, layer = NULL) {
      # 1-to-1 link between data & visual marks -- group == key
      if (inherits(layer$geom, "GeomDotplot")) {
        x <- split(x, x[["PANEL"]])
        x <- lapply(x, function(d) { 
          d[["group"]] <- do.call("order", d[c("x", "group")]) 
          d 
        })
        x <- dplyr::bind_rows(x)
      }
      if (inherits(layer$geom, "GeomSf")) {
        x <- split(x, x[["PANEL"]])
        x <- lapply(x, function(d) { 
          d[["group"]] <- seq_len(nrow(d))
          d 
        })
        # I think this is safe?
        x <- suppressWarnings(dplyr::bind_rows(x))
      }
      x
    }
    
    nestedKeys <- Map(function(x, y, z) { 
      key <- y[[crosstalk_key()]]
      if (is.null(key) || inherits(z[["stat"]], "StatIdentity")) return(NULL)
      x <- reComputeGroup(x, z)
      tib <- tibble::as_tibble(x[c("PANEL", "group")])
      tib[["key"]] <- key
      nested <- tidyr::nest(tib, key = key)
      # reduce the dimensions of list column elements from 2 to 1
      nested$key <- lapply(nested$key, function(x) x[[1]])
      nested
    }, data, layer_data, layers)
    
    # for some geoms (e.g. boxplots) plotly.js needs the "pre-statistics" data
    # we also now provide the option to return one of these two
    prestats_data <- data
    data <- by_layer(function(l, d) l$compute_statistic(d, layout))
    data <- by_layer(function(l, d) l$map_statistic(d, plot))
    
    # Make sure missing (but required) aesthetics are added
    scales_add_missing(plot, c("x", "y"))
    
    # Reparameterise geoms from (e.g.) y and width to ymin and ymax
    data <- by_layer(function(l, d) l$compute_geom_1(d))
    
    # compute_geom_1 can reorder the rows from `data`, making groupDomains
    # invalid. We rebuild groupDomains based on the current `data` and the
    # group map we built before.
    groupDomains <- Map(function(x, y) {
      tryCatch({
        names(y)[match(x$group, y)]
      }, error = function(e) NULL
      )
    }, data, group_maps)
    
    # there are some geoms (e.g. geom_dotplot()) where attaching the key 
    # before applying the statistic can cause problems, but there is still a 
    # 1-to-1 corresponding between graphical marks and 
    
    # Apply position adjustments
    data <- by_layer(function(l, d) l$compute_position(d, layout))
    
    # Reset position scales, then re-train and map.  This ensures that facets
    # have control over the range of a plot: is it generated from what's
    # displayed, or does it include the range of underlying data
    layout$reset_scales()
    layout$train_position(data, scale_x(), scale_y())
    layout$setup_panel_params()
    data <- layout$map_position(data)
    
    # Train and map non-position scales
    npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
      lapply(data, scales_train_df, scales = npscales)
      # this for loop is unique to plotly -- it saves the "domain"
      # of each non-positional scale for display in tooltips
      for (sc in npscales$scales) {
        data <- lapply(data, function(d) {
          # scale may not be relevant for every layer data
          if (any(names(d) %in% sc$aesthetics)) {
            d[paste0(sc$aesthetics, "_plotlyDomain")] <- d[sc$aesthetics]
          }
          d
        })
      }
      data <- lapply(data, scales_map_df, scales = npscales)
    }
    
    # Fill in defaults etc.
    data <- by_layer(function(l, d) l$compute_geom_2(d))
    
    # Let layer stat have a final say before rendering
    data <- by_layer(function(l, d) l$finish_statistics(d))
    
    # Let Layout modify data before rendering
    data <- layout$finish_data(data)
    
    # if necessary, attach key
    data <- Map(function(x, y, z) { 
      if (!length(y)) return(x)
      x <- reComputeGroup(x, z)
      # dplyr issue??? https://github.com/tidyverse/dplyr/issues/2701
      attr(y$group, "n") <- NULL
      # https://github.com/plotly/plotly.R/issues/2013
      if (!identical(class(x$group), class(y$group))) {
        x$group <- as.character(x$group)
        y$group <- as.character(y$group)
      }
      suppressMessages(dplyr::left_join(x, y))
    }, data, nestedKeys, layers)
    
    structure(
      list(
        data = data, layout = layout, plot = plot, 
        env = environment()
      ), 
      class = "ggplot_built"
    )
  }
  
  # Allow thematic to add new defaults to the plot object based on it's theme
  built <- if (isNamespaceLoaded("thematic")) {
    tns <- asNamespace("thematic")
    tns$ggthematic_build(p, ggplotly_build, tns$thematic_get_theme(resolve = TRUE))
  } else {
    ggplotly_build(p)
  }
  
  # Assign all the objects available to ggplotly_build() to this functions environment
  built_env <- built$env
  envir <- environment()
  for (var in ls(built_env)) {
    assign(var, built_env[[var]], envir = envir)
  }
  
  theme <- calculated_theme_elements(plot)
  
  # Translate plot wide theme elements to plotly.js layout
  pm <- unitConvert(theme$plot.margin, "pixels")
  gglayout <- list(
    margin = list(t = pm[[1]], r = pm[[2]], b = pm[[3]], l = pm[[4]]),
    plot_bgcolor = toRGB(theme$panel.background$fill),
    paper_bgcolor = toRGB(theme$plot.background$fill),
    font = text2font(theme$text)
  )
  # main plot title
  if (robust_nchar(plot$labels$title) > 0) {
    gglayout$title <- list(
      text = faced(plot$labels$title, theme$plot.title$face),
      font = text2font(theme$plot.title),
      # don't translate vjust to y since they since have very different meaning...
      # y is allowed to span the paper coordinate whereas vjust it local to it's grob
      x = theme$plot.title$hjust,
      xref = "paper"
    )
    gglayout$margin$t <- gglayout$margin$t + gglayout$title$font$size
  }
  # ensure there's enough space for the modebar (this is based on a height of 1em)
  # https://github.com/plotly/plotly.js/blob/dd1547/src/components/modebar/index.js#L171
  gglayout$margin$t <- gglayout$margin$t + 16
  
  # important stuff like layout$panel_params is already flipped, but
  # plot$scales/plot$labels/data aren't. We flip x/y trace data at the very end
  # and scales in the axis loop below.
  if (inherits(plot$coordinates, "CoordFlip")) {
    plot$labels[c("x", "y")] <- plot$labels[c("y", "x")]
  }
  
  # important panel summary stats
  nPanels <- nrow(layout$layout)
  nRows <- max(layout$layout$ROW)
  nCols <- max(layout$layout$COL)
  
  # panel -> plotly.js axis/anchor info
  # (assume a grid layout by default)
  layout$layout <- dplyr::mutate(
    layout$layout,
    xaxis = COL,
    yaxis = ROW,
    xanchor = nRows,
    yanchor = 1L
  )
  if (inherits(plot$facet, "FacetWrap")) {
    if (plot$facet$params$free$x && plot$facet$params$free$y) {
      layout$layout <- dplyr::mutate(
        layout$layout,
        xaxis = PANEL,
        yaxis = PANEL,
        xanchor = PANEL,
        yanchor = PANEL
      )
    } else if (plot$facet$params$free$x) {
      layout$layout <- dplyr::mutate(
        layout$layout,
        xaxis = PANEL,
        xanchor = ROW
      )
    } else if (plot$facet$params$free$y) {
      layout$layout <- dplyr::mutate(
        layout$layout,
        yaxis = PANEL,
        yanchor = COL
      )
    }
    # anchor X axis to the lowest plot in its column
    layout$layout <- dplyr::group_by(layout$layout, !!rlang::sym("xaxis"))
    layout$layout <-  dplyr::mutate(layout$layout, xanchor = max(as.integer(yaxis)))
  }
  layout$layout <- as.data.frame(layout$layout)

  # format the axis/anchor to a format plotly.js respects
  layout$layout$xaxis <- paste0("xaxis", sub("^1$", "", layout$layout$xaxis))
  layout$layout$yaxis <- paste0("yaxis", sub("^1$", "", layout$layout$yaxis))
  layout$layout$xanchor <- paste0("y", sub("^1$", "", layout$layout$xanchor))
  layout$layout$yanchor <- paste0("x", sub("^1$", "", layout$layout$yanchor))
  # for some layers2traces computations, we need the range of each panel
  layout$layout$x_min <- sapply(layout$panel_params, function(z) { min(z[["x"]]$dimension %()% z$x.range %||% z$x_range) })
  layout$layout$x_max <- sapply(layout$panel_params, function(z) { max(z[["x"]]$dimension %()% z$x.range %||% z$x_range) })
  layout$layout$y_min <- sapply(layout$panel_params, function(z) { min(z[["y"]]$dimension %()% z$y.range %||% z$y_range) })
  layout$layout$y_max <- sapply(layout$panel_params, function(z) { max(z[["y"]]$dimension %()% z$y.range %||% z$y_range) })
  
  # layers -> plotly.js traces
  plot$tooltip <- tooltip
  data <- Map(function(x, y) {
    tryCatch({ x$group_plotlyDomain <- y; x }, error = function(e) x)
  }, data, groupDomains)
  
  # reattach crosstalk key-set attribute
  data <- Map(function(x, y) structure(x, set = y), data, sets)
  traces <- layers2traces(data, prestats_data, layout, plot)
  
  gglayout <- layers2layout(gglayout, layers, layout$layout)
  
  # default to just the text in hover info, mainly because of this
  # https://github.com/plotly/plotly.js/issues/320
  traces <- lapply(traces, function(tr) {
    tr$hoverinfo <- tr$hoverinfo %||%"text"
    tr
  })
  # show only one legend entry per legendgroup
  grps <- sapply(traces, "[[", "legendgroup")
  traces <- Map(function(x, y) {
    if (!is.null(x[["frame"]])) return(x)
    x$showlegend <- isTRUE(x$showlegend) && y
    x
  }, traces, !duplicated(grps))
  
  # ------------------------------------------------------------------------
  # axis/facet/margin conversion
  # ------------------------------------------------------------------------
  
  # panel margins must be computed before panel/axis loops
  # (in order to use get_domains())
  panelMarginX <- unitConvert(
    theme[["panel.spacing.x"]] %||% theme[["panel.spacing"]],
    "npc", "width"
  )
  panelMarginY <- unitConvert(
    theme[["panel.spacing.y"]] %||% theme[["panel.spacing"]],
    "npc", "height"
  )
  # space for _interior_ facet strips
  if (inherits(plot$facet, "FacetWrap")) {
    stripSize <- unitConvert(
      theme[["strip.text.x"]] %||% theme[["strip.text"]],
      "npc", "height"
    )
    panelMarginY <- panelMarginY + stripSize
    # space for ticks/text in free scales
    if (plot$facet$params$free$x) {
      axisTicksX <- unitConvert(
        theme[["axis.ticks.x"]] %||% theme[["axis.ticks"]],
        "npc", "height"
      )
      # allocate enough space for the _longest_ text label
      axisTextX <- theme[["axis.text.x"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(layout$panel_params, function(pp) { pp[["x"]]$get_labels %()% pp$x.labels }))
      lab <- longest_element(labz)
      panelMarginY <- panelMarginY + axisTicksX +
        bbox(lab, axisTextX$angle, unitConvert(axisTextX, "npc", "height"))[["height"]]
    }
    if (plot$facet$params$free$y) {
      axisTicksY <- unitConvert(
        theme[["axis.ticks.y"]] %||% theme[["axis.ticks"]],
        "npc", "width"
      )
      # allocate enough space for the _longest_ text label
      axisTextY <- theme[["axis.text.y"]] %||% theme[["axis.text"]]
      labz <- unlist(lapply(layout$panel_params, function(pp) { pp[["y"]]$get_labels %()% pp$y.labels }))
      lab <- longest_element(labz)
      panelMarginX <- panelMarginX + axisTicksY +
        bbox(lab, axisTextY$angle, unitConvert(axisTextY, "npc", "width"))[["width"]]
    }
  }
  margins <- c(
    rep(panelMarginX, 2),
    rep(panelMarginY, 2)
  )
  doms <- get_domains(nPanels, nRows, margins)
  
  for (i in seq_len(nPanels)) {
    lay <- layout$layout[i, ]
    for (xy in c("x", "y")) {
      # find axis specific theme elements that inherit from their parent
      theme_el <- function(el) {
        theme[[paste0(el, ".", xy)]] %||% theme[[el]]
      }
      axisTicks <- theme_el("axis.ticks")
      axisText <- theme_el("axis.text")
      axisTitle <- theme_el("axis.title")
      axisLine <- theme_el("axis.line")
      panelGrid <- theme_el("panel.grid.major") %||% theme_el("panel.grid") 
      stripText <- theme_el("strip.text")
      
      axisName <- lay[, paste0(xy, "axis")]
      anchor <- lay[, paste0(xy, "anchor")]
      rng <- layout$panel_params[[i]]
      
      # panel_params is quite different for "CoordSf"
      if ("CoordSf" %in% class(p$coordinates)) {
        # see CoordSf$render_axis_v
        direction <- if (xy == "x") "E" else "N"
        idx <- rng$graticule$type == direction & 
          !is.na(rng$graticule$degree_label) &
          # Respect the logical 'plot12' column which sf constructs for 
          # determining which tick labels should be drawn
          # https://github.com/r-spatial/sf/blob/b49d37/R/graticule.R#L199
          # https://github.com/r-spatial/sf/blob/52a8351/R/plot.R#L580
          (rng$graticule$plot12 %||% TRUE)
        tickData <- rng$graticule[idx, ]
        # TODO: how to convert a language object to unicode character string?
        rng[[paste0(xy, ".labels")]] <- sub(
          "\\*\\s+degree[ ]?[\\*]?", "&#176;", 
          gsub("\"", "", tickData[["degree_label"]])
        )
        rng[[paste0(xy, ".major")]] <- tickData[[paste0(xy, "_start")]]
        
        # If it doesn't already exist (for this panel), 
        # generate graticule (as done in, CoordSf$render_bg)
        isGrill <- vapply(traces, function(tr) {
          identical(tr$xaxis, lay$xaxis) && 
            identical(tr$yaxis, lay$yaxis) &&
            isTRUE(tr$`_isGraticule`)
        }, logical(1))
        
        if (sum(isGrill) == 0) {
          # TODO: reduce the number of points (via coord_munch?)
          d <- fortify_sf(rng$graticule)
          d$x <- scales::rescale(d$x, rng$x_range, from = c(0, 1))
          d$y <- scales::rescale(d$y, rng$y_range, from = c(0, 1))
          params <- list(
            colour = panelGrid$colour, 
            linetype = panelGrid$linetype
          )
          nm <- linewidth_or_size(panelGrid)
          params[[nm]] <- panelGrid[[nm]]
          grill <- geom2trace.GeomPath(d, params)
          grill$hoverinfo <- "none"
          grill$showlegend <- FALSE
          grill$`_isGraticule` <- TRUE
          grill$xaxis <- sub("axis", "", lay$xaxis)
          grill$yaxis <- sub("axis", "", lay$yaxis)
          
          traces <- c(list(grill), traces)
        }
        
        # if labels are empty, don't show axis ticks
        tickExists <- with(rng$graticule, sapply(degree_label, is.language))
        if (sum(tickExists) == 0) {
          theme$axis.ticks.length <- 0
        }
      }
      
      # stuff like layout$panel_params is already flipped, but scales aren't
      sc <- if (inherits(plot$coordinates, "CoordFlip")) {
        scales$get_scales(setdiff(c("x", "y"), xy))
      } else {
        scales$get_scales(xy)
      }
      # type of unit conversion
      type <- if (xy == "x") "height" else "width"
      # get axis title
      axisTitleText <- sc$name %||% plot$labels[[xy]] %||% ""
      if (is_blank(axisTitle)) axisTitleText <- ""
      
      # is this axis dynamic?
      isDynamic <- isTRUE(dynamicTicks) || identical(dynamicTicks, xy)
      if (isDynamic && !p$coordinates$is_linear()) {
        warning(
          "`dynamicTicks` is only supported for linear (i.e., cartesian) coordinates", 
          call. = FALSE
        )
      }
      
      # determine axis types
      isDate <- inherits(sc, c("ScaleContinuousDatetime", "ScaleContinuousDate"))
      isDateType <- isDynamic && isDate
      isDiscrete <- inherits(sc, "ScaleDiscretePosition")
      isDiscreteType <- isDynamic && isDiscrete
      
      # In 3.2.x .major disappeared in favor of break_positions()
      # (tidyverse/ggplot2#3436), but with 3.4.x break_positions() no longer
      # yields the actual final positions on a 0-1 scale, but .major does
      # (tidyverse/ggplot2#5029)
      ticktext <- rng[[paste0(xy, ".labels")]] %||% rng[[xy]]$get_labels()
      tickvals <- rng[[paste0(xy, ".major")]] %||% rng[[xy]]$break_positions()
      
      # https://github.com/tidyverse/ggplot2/pull/3566#issuecomment-565085809
      hasTickText <- !(is.na(ticktext) | is.na(tickvals))
      ticktext <- ticktext[hasTickText]
      tickvals <- tickvals[hasTickText]
      
      axisObj <- list(
        # TODO: log type?
        type = if (isDateType) "date" else if (isDiscreteType) "category" else "linear",
        autorange = isDynamic,
        range = rng[[xy]]$dimension %()% rng[[paste0(xy, ".range")]] %||% rng[[paste0(xy, "_range")]],
        tickmode = if (isDynamic) "auto" else "array",
        ticktext = ticktext,
        tickvals = tickvals,
        categoryorder = "array",
        categoryarray = ticktext,
        nticks = nrow(rng),
        ticks = if (is_blank(axisTicks)) "" else "outside",
        tickcolor = toRGB(axisTicks$colour),
        ticklen = unitConvert(theme$axis.ticks.length, "pixels", type),
        tickwidth = unitConvert(axisTicks, "pixels", type),
        showticklabels = !is_blank(axisText),
        tickfont = text2font(axisText, type),
        tickangle = - (axisText$angle %||% 0),
        showline = !is_blank(axisLine),
        linecolor = toRGB(axisLine$colour),
        linewidth = unitConvert(axisLine, "pixels", type),
        # TODO: always `showgrid=FALSE` and implement our own using traces
        showgrid = !is_blank(panelGrid) && !"CoordSf" %in% class(p$coordinates),
        domain = sort(as.numeric(doms[i, paste0(xy, c("start", "end"))])),
        gridcolor = toRGB(panelGrid$colour),
        gridwidth = unitConvert(panelGrid, "pixels", type),
        zeroline = FALSE,
        anchor = anchor,
        # layout.axisid.title don't yet support alignment :(
        title = list(
          text = faced(axisTitleText, axisTitle$face),
          font = text2font(axisTitle)
        )
      )
      
      # set scaleanchor/scaleratio if these are fixed coordinates
      # the logic here is similar to what p$coordinates$aspect() does,
      # but the ratio is scaled to the data range by plotly.js 
      fixed_coords <- c("CoordSf", "CoordFixed", "CoordMap", "CoordQuickmap")
      if (inherits(p$coordinates, fixed_coords)) {
        axisObj$scaleanchor <- anchor
        ratio <- p$coordinates$ratio %||% 1
        axisObj$scaleratio <- if (xy == "y") ratio else 1 / ratio
        
        if (inherits(p$coordinates, "CoordSf")) {
          if (isTRUE(sf::st_is_longlat(rng$crs))) {
            ratio <- cos(mean(rng$y_range) * pi/180)
          }
          # note how ratio is flipped in CoordSf$aspect() vs CoordFixed$aspect()
          axisObj$scaleratio <- if (xy == "y") 1 / ratio else ratio
        }
      }
      
      # TODO: seems like we _could_ support this with scaleanchors, 
      # but inverse transform by the panel ranges?
      # also, note how aspect.ratio overwrites fixed coordinates:
      # ggplot(mtcars, aes(wt, mpg)) + geom_point() + coord_fixed(0.5)
      # ggplot(mtcars, aes(wt, mpg)) + geom_point() + coord_fixed(0.5) + theme(aspect.ratio = 1)
      if (!is.null(theme$aspect.ratio)) {
        warning(
          "Aspect ratios aren't yet implemented, but you can manually set", 
          " a suitable height/width", call. = FALSE
        )
      }
      
      # tickvals are currently on 0-1 scale, but we want them on data scale
      axisObj$tickvals <- scales::rescale(
        axisObj$tickvals, to = axisObj$range, from = c(0, 1)
      )
      
      # inverse transform date data based on tickvals/ticktext
      invert_date <- function(x, scale) {
        if (inherits(scale, "ScaleContinuousDatetime")) {
          as.POSIXct(x, origin = "1970-01-01", tz = scale$timezone)
        } else {
          as.Date(x, origin = "1970-01-01", tz = scale$timezone)
        }
      }
      
      if (isDateType) {
        axisObj$range <- invert_date(axisObj$range, sc)
        traces <- lapply(traces, function(tr) {
          tr[[xy]] <- invert_date(tr[[xy]], sc)
          # TODO: are there other similar cases we need to handle?
          if (identical("bar", tr$type)) {
            tr[["width"]] <- invert_date(tr[["width"]], sc)
          }
          tr
        })
      }
      
      # inverse transform categorical data based on tickvals/ticktext
      if (isDiscreteType) {
        traces <- lapply(traces, function(tr) { 
          # map x/y trace data back to the 'closest' ticktext label
          # http://r.789695.n4.nabble.com/check-for-nearest-value-in-a-vector-td4369339.html
          tr[[xy]]<- vapply(tr[[xy]], function(val) {
            with(axisObj, ticktext[[which.min(abs(tickvals - val))]])
          }, character(1))
          tr
        })
        if ("dodge" %in% sapply(layers, ggtype, "position")) gglayout$barmode <- "dodge"
      }
      
      # attach axis object to the layout
      gglayout[[axisName]] <- axisObj
      
      # do some stuff that should be done once for the entire plot
      is_x <- xy == "x"
      if (i == 1) {
        # Split ticktext elements by "\n"  to account for linebreaks
        axisTickText <- strsplit(as.character(axisObj$ticktext), split = "\n", fixed = TRUE)
        axisTickText <- longest_element(unlist(axisTickText))
        side <- if (is_x) "b" else "l"
        # account for axis ticks, ticks text, and titles in plot margins
        # (apparently ggplot2 doesn't support axis.title/axis.text margins)
        gglayout$margin[[side]] <- gglayout$margin[[side]] + axisObj$ticklen +
          bbox(axisTickText, axisObj$tickangle, axisObj$tickfont$size)[[type]] +
          bbox(axisTitleText, axisTitle$angle, unitConvert(axisTitle, "pixels", type))[[type]]
        
        if (robust_nchar(axisTitleText) > 0) {
          axisTextSize <- unitConvert(axisText, "npc", type)
          axisTitleSize <- unitConvert(axisTitle, "npc", type)
        }
        
        # add space for exterior facet strips in `layout.margin`
        
        if (has_facet(plot)) {
          stripSize <- unitConvert(stripText, "pixels", type)
          if (is_x) {
            gglayout$margin$t <- gglayout$margin$t + stripSize
          }
          if (is_x && inherits(plot$facet, "FacetGrid")) {
            gglayout$margin$r <- gglayout$margin$r + stripSize
          }
          # facets have multiple axis objects, but only one title for the plot,
          # so we empty the titles and try to draw the title as an annotation
          if (robust_nchar(axisTitleText) > 0) {
            axisAnn <- make_label(
              faced(axisTitleText, axisTitle$face), 
              el = axisTitle,
              x = if (is_x) 0.5 else 0,
              y = if (is_x) 0 else 0.5,
              xanchor = if (is_x) "center" else "right", 
              yanchor = if (is_x) "top" else "center", 
              annotationType = "axis"
            )
            
            textMargin <- sum(axisText$margin[if (is_x) c(1, 3) else c(2, 4)])
            class(textMargin) <- setdiff(class(textMargin), "margin")
            titleMargin <- axisTitle$margin[if (is_x) 1 else 2]
            class(titleMargin) <- setdiff(class(titleMargin), "margin")
            offset <- bbox(axisTickText, axisText$angle, axisTextSize)[[type]] +
                 unitConvert(theme$axis.ticks.length, "npc", type) +
                 unitConvert(textMargin, "npc", type) +
                 unitConvert(titleMargin, "npc", type)
            
            offset <- unitConvert(grid::unit(offset, "npc"), "pixels", type)
            
            shift <- if (is_x) "yshift" else "xshift"
            axisAnn[[1]][[shift]] <- -1 * offset
            gglayout$annotations <- c(gglayout$annotations, axisAnn)
          }
        }
      }
      if (has_facet(plot)) gglayout[[axisName]]$title <- ""
    } # end of axis loop
    
    # theme(panel.border = ) -> plotly rect shape
    xdom <- gglayout[[lay[, "xaxis"]]]$domain
    ydom <- gglayout[[lay[, "yaxis"]]]$domain
    border <- make_panel_border(xdom, ydom, theme)
    gglayout$shapes <- c(gglayout$shapes, border)
    
    # facet strips -> plotly annotations
    if (has_facet(plot)) {
      col_vars <- ifelse(inherits(plot$facet, "FacetWrap"), "facets", "cols")
      col_txt <- paste(
        plot$facet$params$labeller(
          lay[names(plot$facet$params[[col_vars]])]
        ), collapse = br()
      )
      if (is_blank(theme[["strip.text.x"]])) col_txt <- ""
      if (inherits(plot$facet, "FacetGrid") && lay$ROW != 1) col_txt <- ""
      if (robust_nchar(col_txt) > 0) {
        col_lab <- make_label(
          col_txt, x = mean(xdom), y = max(ydom),
          el = theme[["strip.text.x"]] %||% theme[["strip.text"]],
          xanchor = "center", yanchor = "bottom"
        )
        gglayout$annotations <- c(gglayout$annotations, col_lab)
        strip <- make_strip_rect(xdom, ydom, theme, "top")
        gglayout$shapes <- c(gglayout$shapes, strip)
      }
      row_txt <- paste(
        plot$facet$params$labeller(
          lay[names(plot$facet$params$rows)]
        ), collapse = br()
      )
      if (is_blank(theme[["strip.text.y"]])) row_txt <- ""
      if (inherits(plot$facet, "FacetGrid") && lay$COL != nCols) row_txt <- ""
      if (robust_nchar(row_txt) > 0) {
        row_lab <- make_label(
          row_txt, x = max(xdom), y = mean(ydom),
          el = theme[["strip.text.y"]] %||% theme[["strip.text"]],
          xanchor = "left", yanchor = "middle"
        )
        gglayout$annotations <- c(gglayout$annotations, row_lab)
        strip <- make_strip_rect(xdom, ydom, theme, "right")
        gglayout$shapes <- c(gglayout$shapes, strip)
      }
    }
  } # end of panel loop
  
  
  # ------------------------------------------------------------------------
  # guide conversion
  #   Strategy: Obtain and translate the output of ggplot2:::guides_train().
  #   To do so, we borrow some of the body of ggplot2:::guides_build().
  # ------------------------------------------------------------------------
  # will there be a legend?
  gglayout$showlegend <- sum(unlist(lapply(traces, "[[", "showlegend"))) >= 1
  
  # legend styling
  gglayout$legend <- list(
    bgcolor = toRGB(theme$legend.background$fill),
    bordercolor = toRGB(theme$legend.background$colour),
    borderwidth = unitConvert(
      theme$legend.background[[linewidth_or_size(theme$legend.background)]], 
      "pixels", "width"
    ),
    font = text2font(theme$legend.text)
  )
  
  # if theme(legend.position = "none") is used, don't show a legend _or_ guide
  if (npscales$n() == 0 || identical(theme$legend.position, "none")) {
    gglayout$showlegend <- FALSE
  } else {
    # by default, guide boxes are vertically aligned
    theme$legend.box <- theme$legend.box %||% "vertical"
    
    # size of key (also used for bar in colorbar guide)
    theme$legend.key.width <- theme$legend.key.width %||% theme$legend.key.size
    theme$legend.key.height <- theme$legend.key.height %||% theme$legend.key.size
    
    # legend direction must be vertical
    theme$legend.direction <- theme$legend.direction %||% "vertical"
    if (!identical(theme$legend.direction, "vertical")) {
      warning(
        "plotly.js does not (yet) support horizontal legend items \n",
        "You can track progress here: \n",
        "https://github.com/plotly/plotly.js/issues/53 \n",
        call. = FALSE
      )
      theme$legend.direction <- "vertical"
    }
    
    # justification of legend boxes
    theme$legend.box.just <- theme$legend.box.just %||% c("center", "center")
    # scales -> data for guides
    gdefs <- if (inherits(plot$guides, "ggproto")) {
      get_gdefs_ggproto(npscales$scales, theme, plot, layers, layer_data)
    } else {
      get_gdefs(scales, theme, plot, layers)
    }

    # colourbar -> plotly.js colorbar
    colorbar <- compact(lapply(gdefs, gdef2trace, theme, gglayout))
    nguides <- length(colorbar) + gglayout$showlegend
    # If we have 2 or more guides, set x/y positions accordingly
    if (nguides >= 2) {
      # place legend at the bottom
      gglayout$legend$y <- 1 / nguides
      gglayout$legend$yanchor <- "top"
      # adjust colorbar position(s)
      for (i in seq_along(colorbar)) {
        colorbar[[i]]$marker$colorbar$yanchor <- "top"
        colorbar[[i]]$marker$colorbar$len <- 1 / nguides
        colorbar[[i]]$marker$colorbar$y <- 1 - (i - 1) * (1 / nguides)
      }
    }
    traces <- c(traces, colorbar)
    
    legendTitles <- compact(lapply(gdefs, function(g) if (inherits(g, "legend")) g$title else NULL))
    legendTitle <- paste(legendTitles, collapse = br())
    gglayout$legend$title <- list(
      text = legendTitle,
      font = text2font(theme$legend.title)
    )
  }
  
  # flip x/y in traces for flipped coordinates
  # (we've already done appropriate flipping for axis objects)
  if (inherits(plot$coordinates, "CoordFlip")) {
    for (i in seq_along(traces)) {
      tr <- traces[[i]]
      # flipping logic for bar positioning is in geom2trace.GeomBar
      if (!identical(tr$type, "bar")) traces[[i]][c("x", "y")] <- tr[c("y", "x")]
      if (identical(tr$type, "box")) {
        traces[[i]]$orientation <- "h"
        traces[[i]]$hoverinfo <- "x"
      }
      names(traces[[i]])[grepl("^error_y$", names(tr))] <- "error_x"
      names(traces[[i]])[grepl("^error_x$", names(tr))] <- "error_y"
    }
  }
  
  # Error bar widths in ggplot2 are on the range of the x/y scale,
  # but plotly wants them in pixels:
  for (xy in c("x", "y")) {
    type <- if (xy == "x") "width" else "height"
    err <- if (xy == "x") "error_y" else "error_x"
    for (i in seq_along(traces)) {
      e <- traces[[i]][[err]]
      if (!is.null(e)) {
        # TODO: again, "npc" is on device scale...we really want plot scale
        w <- grid::unit(e$width %||% 0, "npc")
        traces[[i]][[err]]$width <- unitConvert(w, "pixels", type)
      }
    }
  }
  
  # try to merge marker/line traces that have the same values for these props
  props <- c("x", "y", "text", "type", "xaxis", "yaxis", "name")
  hashes <- vapply(traces, function(x) digest::digest(x[names(x) %in% props]), character(1))
  modes <- vapply(traces, function(x) x$mode %||% "", character(1))
  nhashes <- length(unique(hashes))
  if (nhashes < length(traces)) {
    mergedTraces <- vector("list", nhashes)
    for (i in unique(hashes)) {
      idx <- which(hashes %in% i)
      mergedTraces[[i]] <- Reduce(modify_list, traces[idx])
      mergedTraces[[i]]$mode <- paste(
        unique(unlist(lapply(traces[idx], "[[", "mode"))), 
        collapse = "+"
      )
      # show one, show all
      show <- vapply(traces[idx], function(tr) tr$showlegend %||% TRUE, logical(1))
      if (any(show)) {
        mergedTraces[[i]]$showlegend <- TRUE
      }
    }
    traces <- mergedTraces
  }
  
  # better layout defaults (TODO: provide a mechanism for templating defaults)
  gglayout$hovermode <- "closest"
  ax <- grep("^[x-y]axis", names(gglayout))
  for (i in ax) {
    gglayout[[i]]$hoverformat <- ".2f"
  }
  # If a trace isn't named, it shouldn't have additional hoverinfo
  traces <- lapply(compact(traces), function(x) { x$name <- x$name %||% ""; x })
  
  gglayout$width <- width %|D|% NULL
  gglayout$height <- height %|D|% NULL
  gglayout$barmode <- gglayout$barmode %||% "relative"
  
  l <- list(
    data = setNames(traces, NULL),
    layout = compact(gglayout),
    # prevent autosize on doubleClick which clears ggplot2 margins
    config = list(doubleClick = "reset"),
    source = source
  )
  # strip any existing 'AsIs' list elements of their 'AsIs' status.
  # this is necessary since ggplot_build(qplot(1:10, fill = I("red")))
  # returns list element with their 'AsIs' class,
  # which conflicts with our JSON unboxing strategy.
  l <- rm_asis(l)
  
  # start build a plotly object with meta information about the ggplot
  # first, translate layer mappings -> plotly attrs
  mappingFormulas <- lapply(layers, function(x) {
    mappings <- getAesMap(plot, x)
    if (originalData) {
      lapply(mappings, lazyeval::f_new)
    } else {
      nms <- names(mappings)
      setNames(lapply(nms, function(x) lazyeval::f_new(as.name(x))), nms)
    }
  })
  
  return_dat <- if (originalData) layer_data else data
  
  # translate group aesthetics to data attributes
  return_dat <- Map(function(x, y) {
    if (is.null(y[["group"]])) return(x)
    dplyr::group_by(x, !!rlang::as_quosure(y[["group"]]))
  }, return_dat, mappingFormulas)
  
  # don't need to add group as an attribute anymore
  mappingFormulas <- lapply(mappingFormulas, function(x) x[!grepl("^group$", names(x))])
  
  ids <- lapply(seq_along(data), function(x) new_id())
  l$attrs <- setNames(mappingFormulas, ids)
  l$attrs <- lapply(l$attrs, function(x) structure(x, class = "plotly_eval"))
  # the build step removes the first attrs if no type exists
  l$attrs[[1]][["type"]] <- l$data[[1]][["type"]] %||% "scatter"
  
  l$cur_data <- ids[[layerData]]
  l$visdat <- setNames(lapply(return_dat, function(x) function(y) x), ids)

  l
}


# Due to the non-standard use of assign() in g2list() (above)
utils::globalVariables(c("groupDomains", "layers", "prestats_data", "scales", "sets"))

# Get the "complete" set of theme elements and their calculated values
calculated_theme_elements <- function(plot) {
  if (is.function(asNamespace("ggplot2")$complete_theme)) {
    theme <- ggplot2::complete_theme(plot$theme)
    elements <- names(theme)
  } else {
    theme <- ggfun("plot_theme")(plot)
    elements <- names(which(sapply(theme, inherits, "element")))
  }
  
  for (i in elements) {
    theme[[i]] <- ggplot2::calc_element(i, theme)
  }
  
  theme
}


#-----------------------------------------------------------------------------
# ggplotly 'utility' functions
#-----------------------------------------------------------------------------

# convert ggplot2 sizes and grid unit(s) to pixels or normalized point coordinates
unitConvert <- function(u, to = c("npc", "pixels"), type = c("x", "y", "height", "width")) {
  u <- verifyUnit(u)
  
  convert <- switch(
    type[1],
    x = grid::convertX,
    y = grid::convertY,
    width = grid::convertWidth,
    height = grid::convertHeight
  )
  # convert everything to npc first
  if (inherits(u, "margin")) {
    # margins consist of 4 parts: top, right, bottom, and left
    uh <- grid::convertHeight(u, "npc")
    uw <- grid::convertWidth(u, "npc")
    u <- grid::unit(c(uh[1], uw[2], uh[3], uw[4]), "npc")
  } else {
    u <- convert(u, "npc")
  }
  if (to[1] == "pixels") {
    if (inherits(u, "margin")) {
      uh <- mm2pixels(grid::convertHeight(uh, "mm"))
      uw <- mm2pixels(grid::convertWidth(uw, "mm"))
      u <- c(uh[1], uw[2], uh[3], uw[4])
    } else {
      u <- mm2pixels(convert(u, "mm"))
    }
  }
  as.numeric(u)
}

# ggplot2 size is in millimeters. plotly is in pixels. To do this correctly,
# we need to know PPI/DPI of the display. I'm not sure of a decent way to do that
# from R, but it seems 96 is a reasonable assumption.
mm2pixels <- function(u) {
  u <- verifyUnit(u)
  if (any(getUnitType(u) != "mm")) {
    stop("All units must be in millimeters")
  }
  (as.numeric(u) * 96) / 25.4
}
  
verifyUnit <- function(u) {
  if (grid::is.unit(u)) return(u)
  
  ## the default unit in ggplot2 is millimeters (unless it's element_text())
  if (inherits(u, "element")) {
    grid::unit(u[[linewidth_or_size(u)]] %||% 0, "points")
  } else {
    grid::unit(u %||% 0, "mm")
  }
}

# Use public API for getting the unit's type, if available
# https://github.com/ropensci/plotly/pull/1646#issue-331268260
getUnitType <- function(u) {
  tryNULL(get("unitType", envir = asNamespace("grid"))(u)) %||%
    attr(u, "unit")
}

# detect a blank theme element
is_blank <- function(x) {
  inherits(x, "element_blank") && inherits(x, "element")
}

# given text, and x/y coordinates on 0-1 scale,
# convert ggplot2::element_text() to plotly annotation
make_label <- function(txt = "", x, y, el = ggplot2::element_text(), ...) {
  if (is_blank(el) || is.null(txt) || robust_nchar(txt) == 0 || length(txt) == 0) {
    return(NULL)
  }
  angle <- el$angle %||% 0
  list(list(
    text = txt,
    x = x,
    y = y,
    showarrow = FALSE,
    # TODO: hjust/vjust?
    ax = 0,
    ay = 0,
    font = text2font(el),
    xref = "paper",
    yref = "paper",
    textangle = -angle,
    ...
  ))
}

has_facet <- function(x) {
  inherits(x$facet, c("FacetGrid", "FacetWrap"))
}

#' Estimate bounding box of a rotated string
#'
#' @param txt a character string of length 1
#' @param angle sets the angle of the tick labels with respect to the
#' horizontal (e.g., `tickangle` of -90 draws the tick labels vertically)
#' @param size vertical size of a character
#' @references
#' https://www.dropbox.com/s/nc6968prgw8ne4w/bbox.pdf?dl=0

bbox <- function(txt = "foo", angle = 0, size = 12) {
  # assuming the horizontal size of a character is roughly half of the vertical
  n <- robust_nchar(txt)
  if (sum(n) == 0) return(list(height = 0, width = 0))
  w <- size * (robust_nchar(txt) / 2)
  angle <- abs(angle %||% 0)
  # do the sensible thing in the majority of cases
  if (angle == 0) return(list(height = size, width = w))
  if (angle == 90) return(list(height = w, width = size))
  # first, compute the hypotenus
  hyp <- sqrt(size ^ 2 + w ^ 2)
  list(
    height = max(hyp * cos(90 - angle), size),
    width = max(hyp * sin(90 - angle), w)
  )
}

# create a plotly font object from ggplot2::element_text()
text2font <- function(x = ggplot2::element_text(), type = "height") {
  list(
    color = toRGB(x$colour),
    family = font_family(x$family),
    # TODO: what about the size of vertical text?
    size = unitConvert(grid::unit(x$size %||% 0, "points"), "pixels", type)
  )
}

# Replace a default font family, "", with thematic's font option (if set)
font_family <- function(family = "") {
  if (!identical(family, "")) {
    return(family)
  }
  if (!isNamespaceLoaded("thematic")) {
    return("")
  }
  font <- asNamespace("thematic")$thematic_get_option("font", resolve = TRUE)
  if (!length(font)) {
    return("")
  }
  # font$families is a vector of families, but font.family wants to be a 
  # string (like CSS font-family), so make sure the names are unquoted, 
  # then quote them
  families <- sub("'$", "", sub("^'", "", font$families))
  sprintf("'%s'", paste(families, collapse = "', '"))
}

# wrap text in bold/italics according to the text "face"
faced <- function(txt, face = "plain") {
  if (is.null(face)) face <- "plain"
  x <- switch(face,
              plain = txt,
              bold = bold(txt),
              italic = italic(txt),
              bold.italic = bold(italic(txt))
  )
  # if, for some reason, a face we don't support is used, return the text
  if (is.null(x)) txt else x
}
bold <- function(x) paste("<b>", x, "</b>")
italic <- function(x) paste("<i>", x, "</i>")

# if a vector that has one unique value (ignoring missings), return that value
uniq <- function(x) {
  u <- unique(x)
  if (identical(u, NA) || length(u) == 0) return(u)
  u <- u[!is.na(u)]
  if (length(u) == 1) u else x
}

# theme(strip.background) -> plotly.js rect shape
make_strip_rect <- function(xdom, ydom, theme, side = "top") {
  rekt <- rect2shape(theme[["strip.background"]])
  stripTextX <- theme[["strip.text.x"]] %||% theme[["strip.text"]]
  topSize <- 
    mm2pixels(grid::convertHeight(stripTextX$margin[1], "mm")) +
    mm2pixels(grid::convertHeight(stripTextX$margin[3], "mm")) +
    mm2pixels(grid::convertHeight(grid::unit(stripTextX$size, units = "points"), "mm"))
  stripTextY <- theme[["strip.text.y"]] %||% theme[["strip.text"]]
  rightSize <- 
    mm2pixels(grid::convertWidth(stripTextX$margin[2], "mm")) +
    mm2pixels(grid::convertWidth(stripTextX$margin[4], "mm")) +
    mm2pixels(grid::convertWidth(grid::unit(stripTextY$size, units = "points"), "mm"))
  if ("right" %in% side) {
    # x-padding should be accounted for in `layout.margin.r`
    rekt$y0 <- ydom[1]
    rekt$y1 <- ydom[2]
    rekt$x0 <- 0
    rekt$x1 <- rightSize
    rekt$xanchor <- xdom[2]
    rekt$xsizemode <- "pixel"
  }
  if ("top" %in% side) {
    rekt$x0 <- xdom[1]
    rekt$x1 <- xdom[2]
    rekt$y0 <- 0
    rekt$y1 <- topSize
    rekt$yanchor <- ydom[2]
    rekt$ysizemode <- "pixel"
  }
  list(rekt)
}

# theme(panel.border) -> plotly.js rect shape
make_panel_border <- function(xdom, ydom, theme) {
  rekt <- rect2shape(theme[["panel.border"]])
  rekt$x0 <- xdom[1]
  rekt$x1 <- xdom[2]
  rekt$y0 <- ydom[1]
  rekt$y1 <- ydom[2]
  list(rekt)
}

# element_rect -> plotly.js rect shape
rect2shape <- function(rekt = ggplot2::element_rect()) {
  list(
    type = "rect",
    fillcolor = toRGB(rekt$fill),
    line = list(
      color = toRGB(rekt$colour),
      width = unitConvert(rekt, "pixels", "width"),
      linetype = lty2dash(rekt$linetype)
    ),
    yref = "paper",
    xref = "paper",
    layer = "below"
  )
}

# We need access to internal ggplot2 functions in several places
# this helps us import functions in a way that R CMD check won't cry about
ggfun <- function(x) {
  tryCatch(getFromNamespace(x, "ggplot2"), error = function(e) NULL)
}

ggtype <- function(x, y = "geom") {
  sub(y, "", tolower(class(x[[y]])[1]))
}

# colourbar -> plotly.js colorbar
gdef2trace <- function(gdef, theme, gglayout) {
  if (inherits(gdef, "colorbar")) {
    # sometimes the key has missing values, which we can ignore
    gdef$key <- gdef$key[!is.na(gdef$key$.value), ]
    
    # Put values on a 0-1 scale
    # N.B. ggplot2 >v3.4.2 (specifically #4879) renamed bar to decor and also 
    # started returning normalized values for the key field
    decor <- gdef$decor %||% gdef$bar
    decor$value <- decor$value %||% decor$max
    rng <- range(decor$value)
    decor$value <- scales::rescale(decor$value, from = rng)
    if (!"decor" %in% names(gdef)) {
      gdef$key$.value <- scales::rescale(gdef$key$.value, from = rng)
    }
    
    vals <- lapply(gglayout[c("xaxis", "yaxis")], function(ax) {
      if (identical(ax$tickmode, "auto")) ax$ticktext else ax$tickvals
    })
    
    list(
      x = vals[[1]][[1]],
      y = vals[[2]][[1]],
      # essentially to prevent this getting merged at a later point
      name = gdef$hash,
      type = "scatter",
      mode = "markers",
      opacity = 0,
      hoverinfo = "skip",
      showlegend = FALSE,
      # do everything on a 0-1 scale
      marker = list(
        color = c(0, 1),
        colorscale = setNames(decor[c("value", "colour")], NULL),
        colorbar = list(
          bgcolor = toRGB(theme$legend.background$fill),
          bordercolor = toRGB(theme$legend.background$colour),
          borderwidth = unitConvert(
            theme$legend.background[[linewidth_or_size(theme$legend.background)]],
            "pixels", "width"
          ),
          thickness = unitConvert(
            theme$legend.key.width, "pixels", "width"
          ),
          title = gdef$title,
          titlefont = text2font(gdef$title.theme %||% theme$legend.title),
          tickmode = "array",
          ticktext = gdef$key$.label,
          tickvals = gdef$key$.value,
          tickfont = text2font(gdef$label.theme %||% theme$legend.text),
          ticklen = 2,
          len = 1/2
        )
      )
    )
  } else {
    # if plotly.js gets better support for multiple legends,
    # that conversion should go here
    NULL
  }
}


getAesMap <- function(plot, layer) {
  if (isTRUE(layer$inherit.aes)) {
    modify_list(plot$mapping, layer$mapping)
  } else {
    layer$mapping
  }
}

# ------------------------------------------------------------------
# Handle compatibility for changes in ggplot2 >v3.4.2 (specifically #5144),
# which moved away from scales_transform_df(), scales_train_df(), etc  
# towards ggproto methods attached to `scales`
# ------------------------------------------------------------------
scales_transform_df <- function(scales, df) {
  if (is.function(scales$transform_df)) {
    scales$transform_df(df)
  } else {
    ggfun("scales_transform_df")(df, scales = scales)
  }
}

scales_train_df <- function(scales, df) {
  if (is.function(scales$train_df)) {
    scales$train_df(df)
  } else {
    ggfun("scales_train_df")(df, scales = scales)
  }
}

scales_map_df <- function(scales, df) {
  if (is.function(scales$map_df)) {
    scales$map_df(df)
  } else {
    ggfun("scales_map_df")(df, scales = scales)
  }
}

scales_add_missing <- function(plot, aesthetics) {
  if (is.function(plot$scales$add_missing)) {
    plot$scales$add_missing(c("x", "y"), plot$plot_env)
  } else {
    ggfun("scales_add_missing")(plot, aesthetics, plot$plot_env)
  }
}

# -------------------------------------------------------------------------
# Handle compatibility for changes in ggplot2 >v3.4.2 (specifically #4879),
# which away from guides_train(), guides_merge(), guides_geom() 
# towards ggproto methods attached to `plot$guides`
# -------------------------------------------------------------------------
get_gdefs_ggproto <- function(scales, theme, plot, layers, layer_data) {
  
  # Unfortunate duplication of logic in tidyverse/ggplot2#5428
  # which ensures a 1:1 mapping between aesthetics and scales
  aesthetics <- lapply(scales, `[[`, "aesthetics")
  scales     <- rep.int(scales, lengths(aesthetics))
  aesthetics <- unlist(aesthetics, recursive = FALSE, use.names = FALSE)
  
  guides <- plot$guides$setup(scales, aesthetics = aesthetics)
  guides$train(scales, plot$labels)
  if (length(guides$guides) > 0) {
    guides$merge()
    guides$process_layers(layers, layer_data)
  }
  # Add old legend/colorbar classes to guide params so that ggplotly() code
  # can continue to work the same way it always has
  for (i in which(vapply(guides$guides, inherits, logical(1), "GuideColourbar"))) {
    guides$params[[i]] <- prefix_class(guides$params[[i]], "colorbar")
  }
  for (i in which(vapply(guides$guides, inherits, logical(1), "GuideLegend"))) {
    guides$params[[i]] <- prefix_class(guides$params[[i]], "legend")
  }
  guides$params
}

get_gdefs <- function(scales, theme, plot, layers) {
  gdefs <- ggfun("guides_train")(scales, theme, plot$guides, plot$labels)
  if (length(gdefs) > 0) {
    gdefs <- ggfun("guides_merge")(gdefs)
    gdefs <- ggfun("guides_geom")(gdefs, layers, plot$mapping)
  }
  gdefs
}
