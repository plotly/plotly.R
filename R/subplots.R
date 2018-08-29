#' View multiple plots in a single view
#' 
#' @param ... One of the following 
#' \itemize{
#'  \item any number of plotly/ggplot2 objects.
#'  \item a list of plotly/ggplot2 objects.
#'  \item a tibble with one list-column of plotly/ggplot2 objects.
#' }
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
#' @param widths relative width of each column on a 0-1 scale. By default all
#' columns have an equal relative width.
#' @param heights relative height of each row on a 0-1 scale. By default all
#' rows have an equal relative height.
#' @param margin either a single value or four values (all between 0 and 1).
#' If four values are provided, the first is used as the left margin, the second
#' is used as the right margin, the third is used as the top margin, and the
#' fourth is used as the bottom margin.
#' If a single value is provided, it will be used as all four margins. 
#' @param shareX should the x-axis be shared amongst the subplots?
#' @param shareY should the y-axis be shared amongst the subplots?
#' @param titleX should x-axis titles be retained?
#' @param titleY should y-axis titles be retained?
#' @param which_layout adopt the layout of which plot? If the default value of 
#' "merge" is used, layout options found later in the sequence of plots will 
#' override options found earlier in the sequence. This argument also accepts a 
#' numeric vector specifying which plots to consider when merging.
#' @return A plotly object
#' @export
#' @author Carson Sievert
#' @examples 
#' 
#' # pass any number of plotly objects to subplot()
#' p1 <- plot_ly(economics, x = ~date, y = ~uempmed)
#' p2 <- plot_ly(economics, x = ~date, y = ~unemploy)
#' subplot(p1, p2, p1, p2, nrows = 2, margin = 0.05)
#' 
#' #'  # anchor multiple traces on the same legend entry
#'  p1 <- add_lines(p1, color = I("black"), name = "1st", legendgroup = "1st")
#'  p2 <- add_lines(p2, color = I("red"), name = "2nd", legendgroup = "2nd")
#'  
#'  subplot(
#'    p1, style(p1, showlegend = FALSE),
#'    p2, style(p2, showlegend = FALSE),
#'    nrows = 2, margin = 0.05
#'  )
#' 
#' # or pass a list
#' economics_long %>%
#'   split(.$variable) %>%
#'   lapply(function(d) plot_ly(d, x = ~date, y = ~value)) %>%
#'   subplot(nrows = NROW(.), shareX = TRUE)
#'   
#' # or pass a tibble with a list-column of plotly objects
#' economics_long %>%
#'   group_by(variable) %>%
#'   do(p = plot_ly(., x = ~date, y = ~value)) %>%
#'   subplot(nrows = NROW(.), shareX = TRUE)
#'   
#' # learn more at https://cpsievert.github.io/plotly_book/subplot.html
#' 

subplot <- function(..., nrows = 1, widths = NULL, heights = NULL, margin = 0.02, 
                    shareX = FALSE, shareY = FALSE, titleX = shareX, 
                    titleY = shareY, which_layout = "merge") {
 
  
  plots <- dots2plots(...)
  
  # some plotly functions call plotly_build()...subplot() doesn't like that
  for (i in seq_along(plots)) {
    if (!is.null(plots[[i]][["frames"]])) {
      warning(
        sprintf("`subplot()` detected plot #%s was 'pre-built' and already has registered\n", i),
        "animation frames. This can cause problems and may happen by calling a \n", 
        "function like `animation_opts()` or `highlight()` (which returns a 'built' plot)\n",
        "_before_ `subplot()`. Consider using such functions _after_ `subplot()`.",
        call. = FALSE
      )
    }
  }
  
  # build all the plots without registering frames
  plotz <- lapply(plots, function(d) plotly_build(d, registerFrames = FALSE)[["x"]])
  
  # Are any traces referencing "axislike" layout attributes that are missing?
  # If so, move those traces to a "new plot", and inherit layout attributes,
  # which makes this sort of thing possible:
  # https://plot.ly/r/map-subplots-and-small-multiples/
  plots <- list()
  for (i in seq_along(plotz)) {
    p <- plots[[i]] <- plotz[[i]]
    layoutAttrs <- c(names(p$layout), c("mapbox", "geo", "xaxis", "yaxis"))
    xTraceAttrs <- sub("^x", "xaxis", sapply(p$data, function(tr) tr[["subplot"]] %||% tr[["geo"]] %||% tr[["xaxis"]] %||% "x"))
    yTraceAttrs <- sub("^y", "yaxis", sapply(p$data, function(tr) tr[["subplot"]] %||% tr[["geo"]] %||% tr[["yaxis"]] %||% "y"))
    missingAttrs <- setdiff(c(xTraceAttrs, yTraceAttrs), layoutAttrs)
    # move to next iteration if trace references are complete
    if (!length(missingAttrs)) next
    # remove each "missing" trace from this plot
    missingTraces <- xTraceAttrs %in% missingAttrs | yTraceAttrs %in% missingAttrs
    plots[[i]]$data[missingTraces] <- NULL
    # move traces with "similar missingness" to a new plot
    for (j in missingAttrs) {
      newPlot <- list(
        data = p$data[xTraceAttrs %in% j | yTraceAttrs %in% j],
        layout = p$layout
      )
      # reset the anchors
      newPlot$data <- lapply(newPlot$data, function(tr) {
        for (k in c("mapbox", "geo", "xaxis", "yaxis")) {
          tr[[k]] <- sub("[0-9]+", "", tr[[k]]) %||% NULL
        }
        tr
      })
      plots <- c(plots, list(newPlot))
    }
  }
  # set the domain(position) of each subplot in a grid layout
  subplots_info <- get_grid_layout(
    length(plots), nrows, margin, widths = widths, heights = heights
  )
  subplots_info$subplot_index <- 1L
  # bind all shapes to the only subplot of each plot
  subplots_info$shape_indices <- sapply(plots, function(p) paste0(seq_along(p$shapes), collapse=" "))
  # bind all non-axis title annotations to the only subplot of each plot
  subplots_info$annotation_indices <- sapply(plots, function(p) paste0(which(vapply(p$annotations,
                                                                                    function(a) identical(a$annotationType, "axis"), logical(1))),
                                                                       collapse=" "))

  # collect subplots axes information
  axes_info <- dplyr::bind_rows(lapply(seq_along(plots), function(i){
    ax_info <- get_axes_info(plots[[i]])
    ax_info$plot_index <- i
    return(ax_info)
  }))

  # get the plot position in the grid layout and the new domain(s) for each axis
  axes_info <- axes_info %>%
    dplyr::left_join(dplyr::select(subplots_info, plot_index, plot_col=col, plot_row=row,
                                   new_xstart=xstart, new_xend=xend,
                                   new_ystart=ystart, new_yend=yend),
                     by="plot_index") %>%
    dplyr::mutate(new_xstart = dplyr::if_else(dim == "y", NA_real_, new_xstart),
                  new_xend = dplyr::if_else(dim == "y", NA_real_, new_xend),
                  new_ystart = dplyr::if_else(dim == "x", NA_real_, new_ystart),
                  new_yend = dplyr::if_else(dim == "x", NA_real_, new_yend))
  # strip domain information from the subplots
  subplots_info <- dplyr::select(subplots_info, -xstart, -xend, -ystart, -yend)

  # number of x/y axes per plot
  # note: a _single_ geo/mapbox object counts a both an x and y
  xAxisN <- table(subset(axes_info, dim!="y")$plot_index)
  yAxisN <- table(subset(axes_info, dim!="x")$plot_index)

  # Set the new axes properties
  # assign new axes indexes (1..N) for each axis type(dim)
  axes_info <- dplyr::group_by(axes_info, dim) %>%
    dplyr::mutate(new_dim_index = row_number()) %>%
    dplyr::ungroup()
  # correcct the new axes indices if they are shared by the subplots
  if (shareX) {
    if (length(unique(xAxisN)) > 1L) {
      warning("Must have a consistent number of axes per 'subplot' to share them.")
    } else {
      axes_mask <- axes_info$dim != "y"
      axes_info[axes_mask, "new_dim_index"] <- (axes_info$plot_col[axes_mask]-1)*unique(xAxisN) +
                                                rep.int(seq(unique(xAxisN)), length(plots))
      #xAxisID <- rep(rep(seq_len(ncols * unique(xAxisN)), length.out = length(plots)), unique(xAxisN))
    }
  }
  if (shareY) {
    if (length(unique(yAxisN)) > 1L) {
      warning("Must have a consistent number of axes per 'subplot' to share them.")
    } else {
      axes_mask <- axes_info$dim != "x"
      axes_info[axes_mask, "new_dim_index"] <- (axes_info$plot_row[axes_mask]-1)*unique(yAxisN) +
                                                rep.int(seq(unique(yAxisN)), length(plots))
      #yAxisID <- rep(rep(seq_len(nrows * unique(xAxisN)), each = ncols, length.out = length(plots)), unique(yAxisN))
    }
  }
  # remove axis titles, if specified
  axes_info$new_title <- axes_info$title
  if (!titleX) {
    axes_info[axes_info$dim == "x", "new_title"] <- NA_character_
  }
  if (!titleY) {
    axes_info[axes_info$dim == "y", "new_title"] <- NA_character_
  }
  # exclude all but one shared axes (the one that is closer to the bottom-left corner)
  axes_info <- dplyr::group_by(axes_info, dim, new_dim_index) %>%
    dplyr::mutate(is_preserved = pmax(dplyr::min_rank(plot_col), dplyr::min_rank(-plot_row)) == 1L) %>%
    dplyr::ungroup()
  # add axes references to subplots, one axis reference per plot
  xaxis_refs <- dplyr::filter(axes_info, dim == "x" | !is.na(xstart)) %>%
                dplyr::group_by(plot_index) %>% dplyr::filter(row_number()==1L) %>%
                dplyr::ungroup() %>% dplyr::select(plot_index, xref = ref)
  yaxis_refs <- dplyr::filter(axes_info, dim == "y" | !is.na(ystart)) %>%
                dplyr::group_by(plot_index) %>% dplyr::filter(row_number()==1L) %>%
                dplyr::ungroup() %>% dplyr::select(plot_index, yref = ref)
  subplots_info <- dplyr::left_join(dplyr::left_join(subplots_info,
                                    xaxis_refs, by="plot_index"),
                                    yaxis_refs, by="plot_index")

  merge_plots(plots, subplots_info, axes_info, which_layout = which_layout)
}

# merge plotly "plots" using the new layout provided by "subplots_info" frame and
# updated axes properties from "axes_info" frame
merge_plots <- function(plots, subplots_info, axes_info, which_layout = "merge") {
  # set the new axis names, if not set
  if (!("new_name" %in% colnames(axes_info))) {
    axes_info$new_name <- paste0(sub("[0-9]+$", "", axes_info$name), sub("^1$", "", axes_info$new_dim_index))
  }
  if (!("new_ref" %in% colnames(axes_info))) {
    axes_info$new_ref <- paste0(axes_info$dim, sub("^1$", "", axes_info$new_dim_index))
  }
  # add the new axis anchor reference for cartesian axes
  axes_info <- dplyr::left_join(dplyr::mutate(axes_info,
                                              eff_anchor=dplyr::if_else(!is.na(anchor), anchor,
                                                                        dplyr::case_when(axes_info$dim=="x" ~ "y",
                                                                                         axes_info$dim=="y" ~ "x",
                                                                                         TRUE ~ NA_character_))),
                                              dplyr::select(axes_info, plot_index, eff_anchor=ref, new_anchor=new_ref),
                                by=c("plot_index", "eff_anchor")) %>%
              dplyr::select(-eff_anchor)
  # get subplots domains from their axes references
  subplots_info <- dplyr::left_join(subplots_info,
        dplyr::select(axes_info, plot_index, xref=ref, xstart, xend, new_xstart, new_xend),
        by=c("plot_index", "xref")) %>%
    dplyr::left_join(
        dplyr::select(axes_info, plot_index, yref=ref, ystart, yend, new_ystart, new_yend),
        by=c("plot_index", "yref"))

  # grab main plot objects
  traces <- lapply(plots, "[[", "data")
  layouts <- lapply(plots, "[[", "layout")
  axes <- lapply(layouts, function(lay) lay[grepl("^geo|^mapbox|^[xy]axis", names(lay))])
  shapes <- lapply(layouts, function(l) return(NULL))
  annotations <- lapply(layouts, function(l) return(NULL))

  # compose transform for repositioning subplots shapes/annotations
  get_transform <- function(subplot_info, dim) {
    start <- subplot_info[[paste0(dim,"start")]] %||% 0.0
    end <- subplot_info[[paste0(dim,"end")]] %||% 1.0
    new_start <- subplot_info[[paste0("new_",dim,"start")]] %||% 0.0
    new_end <- subplot_info[[paste0("new_",dim,"end")]] %||% 1.0
    d <- end - start
    return(c((new_end  - new_start)/d,
             (new_start*end  - new_end*start)/d))
  }

  for (i in seq_along(plots)) {
    # update preserved plot axes
    plot_axes_info <- dplyr::arrange(dplyr::filter(axes_info, plot_index==i), axis_index)
    plot_axes <- setNames(axes[[i]], plot_axes_info$new_name)
    for (j in dplyr::filter(plot_axes_info, is_preserved)$axis_index) {
      axis_info <- subset(plot_axes_info, axis_index==j)
      ax <- plot_axes[[j]]
      # update the anchor
      if (!is.na(axis_info$new_anchor)) {
        ax$anchor <- axis_info$new_anchor
      }
      # update domains
      if (all(c("x", "y") %in% names(ax$domain))) {
        # geo domains are different from cartesian
        ax$domain$x <- c(axis_info$new_xstart, axis_info$new_xend)
        ax$domain$y <- c(axis_info$new_ystart, axis_info$new_yend)
      } else if (axis_info$dim == "x") {
        ax$domain <- c(axis_info$new_xstart, axis_info$new_xend)
      } else if (axis_info$dim == "y") {
        ax$domain <- c(axis_info$new_ystart, axis_info$new_yend)
      }
      # update the title
      if (is.na(axis_info$new_title) && !is.na(axis_info$title)) {
        ax$title <- NULL
	    } else if (!is.na(axis_info$new_title)) {
        ax$title <- axis_info$new_title
      }
      plot_axes[[j]] <- ax
    }
    # update merged plot axes excluding the unpreserved ones
    axes[[i]] <- plot_axes[plot_axes_info$is_preserved]

    # map old trace anchor names to the new ones
    anchorMap <- setNames(plot_axes_info$new_ref, plot_axes_info$ref)

    # map trace xaxis/yaxis/geo attributes
    for (key in c("geo", "subplot", "xaxis", "yaxis")) {
      oldAnchors <- unlist(lapply(traces[[i]], "[[", key))
      if (!length(oldAnchors)) next
      newAnchors <- anchorMap[oldAnchors]
      traces[[i]] <- Map(function(tr, a) { tr[[key]] <- a; tr }, traces[[i]], newAnchors)
    }
    # reposition plot shapes and annotations
    plot_shapes <- layouts[[i]]$shapes
    plot_anns <- layouts[[i]]$annotations
    plot_subplots_info <- dplyr::filter(subplots_info, plot_index==i)

    for (j in plot_subplots_info$subplot_index) {
      subplot_info <- subset(plot_subplots_info, subplot_index==j)
      xTrf <- get_transform(subplot_info, "x")
      yTrf <- get_transform(subplot_info, "y")
      shape_ixs <- as.integer(strsplit(subplot_info$shape_indices, " ", fixed=TRUE)[[1]])
      plot_shapes[shape_ixs] <- lapply(plot_shapes[shape_ixs], reposition, xTrf, yTrf)
      ann_ixs <- as.integer(strsplit(subplot_info$annotation_indices, " ", fixed=TRUE)[[1]])
      plot_anns[ann_ixs] <- lapply(plot_anns[ann_ixs], reposition, xTrf, yTrf)
    }
    shapes[[i]] <- plot_shapes
    annotations[[i]] <- plot_anns
  }

  p <- list(
    data = Reduce(c, traces),
    layout = Reduce(modify_list, axes)
  )
  
  
  # add repositioned shapes and annotations
  p$layout$shapes <- Reduce(c, shapes)
  p$layout$annotations <- Reduce(c, annotations)

  # merge non-axis layout stuff
  layouts <- lapply(layouts, function(x) {
    x[!grepl("^[x-y]axis|^geo|^mapbox|annotations|shapes", names(x))] %||% list()
  })
  if (which_layout != "merge") {
    if (!is.numeric(which_layout)) warning("which_layout must be numeric")
    if (!all(idx <- which_layout %in% seq_along(plots))) {
      warning("which_layout is referencing non-existant layouts")
      which_layout <- which_layout[idx]
    }
    layouts <- layouts[which_layout]
  }
  p$attrs <- Reduce(c, lapply(plots, "[[", "attrs"))
  p$layout <- modify_list(p$layout, Reduce(modify_list, layouts))
  p$source <- ensure_one(plots, "source")
  p$config <- ensure_one(plots, "config")
  p$highlight <- ensure_one(plots, "highlight")
  
  # retrain default coloring
  p$data <- colorway_retrain(p$data, p$layout$colorway %||% colorway())
  
  p$subplot <- TRUE
  as_widget(p)
}

# ----------------------------------------------------------------
# Functions used solely within subplot()
# ----------------------------------------------------------------

# take a "collection" of plots and 
dots2plots <- function(...) {
  dotz <- list(...)
  
  # if ... is a list (or a tibble), list(...) is a (length 1) list 
  # containing a list of plotly objects
  if (length(dotz) == 1 && is.list(dotz[[1]]) && !is.plotly(dotz[[1]])) {
    dotz <- dotz[[1]]
  }
  
  if (tibble::is_tibble(dotz)) {
    # if dots is a tibble, search for one column with a list of plotly objects
    idx <- which(vapply(dotz, function(x) is.plotly(x[[1]]), logical(1)))
    if (length(idx) != 1) {
      stop(
        "If you supply a tibble to subplot(), \n", 
        "it must have _one_ column with a list of plotly objects",
        call. = FALSE
      )
    }
    dotz <- dotz[[idx]]
  }
  
  dotz
}



# helper function that warns if more than one plot-level attribute 
# has been specified in a list of plots (and returning that attribute)
ensure_one <- function(plots, attr) {
  attrs <- lapply(plots, "[", attr)
  for (i in seq_along(attrs)) {
    if (!identical(attrs[[1]], attrs[[i]])) {
      warning("Can only have one: ", attr, call. = FALSE)
      break
    }
  }
  attrs[[length(attrs)]][[1]]
}

# helper function returning the data frame with the axes information
# for the plotly object "p"
get_axes_info <- function(p) {
  axes <- p$layout[grepl("^geo|^mapbox|^[xy]axis", names(p$layout))]
  res <- data.frame(
    name = names(axes),
    type = sapply(axes, function(ax) ax$type %||% NA_character_),
    axis_index = seq(axes), # position in the axes list
    title = sapply(axes, function(ax) ax$title %||% NA_character_),
    anchor = sapply(axes, function(ax) ax$anchor %||% NA_character_),
    range_start = sapply(axes, function(ax) ax$range[[1]] %||% NA_real_),
    range_end = sapply(axes, function(ax) ax$range[[2]] %||% NA_real_),
    stringsAsFactors = FALSE
  )
  res$dim <- sub("(axis)?[0-9]*$", "", res$name)
  # axis index within its dimension
  res$dim_index <- as.integer(sub("^[^0-9]*", "", res$name))
  res$dim_index <- dplyr::if_else(is.na(res$dim_index), 1L, res$dim_index)
  # how the axis is referenced by the trace or as another axis anchor
  # (index=1) is omitted
  res$ref <- paste0(res$dim, sub("^1$", "", res$dim_index))
  # axis domain(s)
  dom0 <- sapply(axes, function(ax) ax$domain[[1]] %||% NA_real_)
  dom1 <- sapply(axes, function(ax) ax$domain[[2]] %||% NA_real_)
  xdom0 <- sapply(axes, function(ax) ax$x$domain[[1]] %||% NA_real_)
  xdom1 <- sapply(axes, function(ax) ax$x$domain[[2]] %||% NA_real_)
  ydom0 <- sapply(axes, function(ax) ax$y$domain[[1]] %||% NA_real_)
  ydom1 <- sapply(axes, function(ax) ax$y$domain[[2]] %||% NA_real_)
  res$xstart <- dplyr::if_else(res$dim == "x", dom0, xdom0)
  res$xend <- dplyr::if_else(res$dim == "x", dom1, xdom1)
  res$ystart <- dplyr::if_else(res$dim == "y", dom0, ydom0)
  res$yend <- dplyr::if_else(res$dim == "y", dom1, ydom1)

  return(res)
}

# helper function returning the domains (positions) for the subplots
# in the grid layout
get_grid_layout <- function(nplots = 1, nrows = 1, margins = 0.01, 
                            widths = NULL, heights = NULL) {
  if (length(margins) == 1) margins <- rep(margins, 4)
  if (length(margins) != 4) stop("margins must be length 1 or 4", call. = FALSE)
  ncols <- ceiling(nplots / nrows)
  widths <- widths %||% rep(1 / ncols, ncols)
  heights <- heights %||% rep(1 / nrows, nrows)
  if (length(widths) != ncols) {
    stop("The length of the widths argument must be equal ",
         "to the number of columns", call. = FALSE)
  }
  if (length(heights) != nrows) {
    stop("The length of the heights argument is ", length(heights),
         ", but the number of rows is ", nrows, call. = FALSE)
  }
  if (any(widths < 0) | any(heights < 0)) {
    stop("The widths and heights arguments must contain positive values")
  }
  if (sum(widths) > 1 | sum(heights) > 1) {
    stop("The sum of the widths and heights arguments must be less than 1")
  }
  
  widths <- cumsum(c(0, widths))
  heights <- cumsum(c(0, heights))
  # 'center' these values if there is still room left 
  widths <- widths + (1 - max(widths)) / 2
  heights <- heights + (1 - max(heights)) / 2
  
  xs <- vector("list", ncols)
  for (i in seq_len(ncols)) {
    xs[[i]] <- c(
      xstart = widths[i] + if (i == 1) 0 else margins[1],
      xend = widths[i + 1] - if (i == ncols) 0 else margins[2]
    )
  }
  xz <- rep_len(xs, nplots)
  
  ys <- vector("list", nrows)
  for (i in seq_len(nplots)) {
    j <- ceiling(i / ncols)
    ys[[i]] <- c(
      ystart = 1 - (heights[j]) - if (j == 1) 0 else margins[3],
      yend = 1 - (heights[j + 1]) + if (j == nrows) 0 else margins[4]
    )
  }
  res <- list2df(Map(c, xz, ys))
  res$plot_index <- seq(nplots)
  res$col <- (res$plot_index-1L) %% ncols + 1L
  res$row <- (res$plot_index-1L) %/% ncols + 1L

  return(res)
}

list2df <- function(x, nms) {
  #stopifnot(length(unique(sapply(x, length))) == 1)
  m <- if (length(x) == 1) t(x[[1]]) else Reduce(rbind, x)
  row.names(m) <- NULL
  df <- data.frame(m)
  if (!missing(nms)) setNames(df, nms) else df
}

# translate x/y positions according to domain objects
# (useful mostly for repositioning annotations/shapes in subplots)
reposition <- function(obj, xTrf, yTrf) {
  # we need x and y in order to rescale them!
  # TODO: this implementation currently assumes xref/yref == "paper"
  # should we support references to axis objects as well?
  for (j in c("x", "x0", "x1")) {
    if (is.numeric(obj[[j]])) {
      obj[[j]] <- sapply(obj[[j]], function(x) x*xTrf[[1]] + xTrf[[2]])#scales::rescale(o[[j]], domX, from = c(0, 1))
    }
  }
  for (j in c("y", "y0", "y1")) {
    if (is.numeric(obj[[j]])) {
      obj[[j]] <- sapply(obj[[j]], function(y) y*yTrf[[1]] + yTrf[[2]])#scales::rescale(o[[j]], domY, from = c(0, 1))
    }
  }
  obj
}


colorway_retrain <- function(traces, colorway = colorway()) {
  colorway <- rep(colorway, length.out = length(traces))
  for (i in seq_along(traces)) {
    col <- prefix_class(default(colorway[[i]]), "colorway")
    traces[[i]] <- rapply(traces[[i]], function(x) { if (inherits(x, "colorway")) alpha_inherit(col, x) else x }, how = "replace")
  }
  traces 
}


# retrieve the alpha of an 'old' color code and apply it to a new color code
alpha_inherit <- function(new, old) {
  # should return the alpha in a rgba() code
  alphas <- as.numeric(col2rgb(rgb2hex(old), alpha = TRUE)["alpha", ] / 255)
  prefix_class(default(toRGB(new, alphas)), "colorway")
}
