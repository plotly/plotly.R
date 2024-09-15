#' Obtain data associated with a plotly graph
#'
#' `plotly_data()` returns data associated with 
#' a plotly visualization (if there are multiple data frames, by default, 
#' it returns the most recent one). 
#' 
#' @param p a plotly visualization.
#' @param id a character string or number referencing an "attribute layer".
#' 
#' @param .data a plotly visualization.
#' @param x a plotly visualization.
#' @param ... arguments passed onto the relevant method.
#' 
#' @name plotly_data
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' # use group_by() to define groups of visual markings
#' p <- txhousing %>%
#'   group_by(city) %>%
#'   plot_ly(x = ~date, y = ~sales)
#' p
#' 
#' # plotly objects preserve data groupings 
#' groups(p)
#' plotly_data(p)
#' 
#' # dplyr verbs operate on plotly objects as if they were data frames
#' p <- economics %>%
#'   plot_ly(x = ~date, y = ~unemploy / pop) %>%
#'   add_lines() %>%
#'   mutate(rate = unemploy / pop) %>% 
#'   filter(rate == max(rate))
#' plotly_data(p)
#' add_markers(p)
#' layout(p, annotations = list(x = ~date, y = ~rate, text = "peak"))
#' 
#' # use group_by() + do() + subplot() for trellis displays 
#' d <- group_by(mpg, drv)
#' plots <- do(d, p = plot_ly(., x = ~cty, name = ~drv))
#' subplot(plots[["p"]], nrows = 3, shareX = TRUE)
#'
#' # arrange displays by their mean
#' means <- summarise(d, mn = mean(cty, na.rm = TRUE))
#' means %>%
#'   dplyr::left_join(plots) %>%
#'   arrange(mn) %>%
#'   subplot(nrows = NROW(.), shareX = TRUE)
#'   
#' # more dplyr verbs applied to plotly objects
#' p <- mtcars %>%
#'   plot_ly(x = ~wt, y = ~mpg, name = "scatter trace") %>%
#'   add_markers()
#' p %>% slice(1) %>% plotly_data()
#' p %>% slice(1) %>% add_markers(name = "first observation")
#' p %>% filter(cyl == 4) %>% plotly_data()
#' p %>% filter(cyl == 4) %>% add_markers(name = "four cylinders")
#' 
#' 
plotly_data <- function(p, id = p$x$cur_data) {
  if (!is.plotly(p)) {
    stop("`plotly_data()` expects a plotly object as it's first argument.", call. = FALSE)
  }
  f <- p$x$visdat[[id]]
  # if data has been specified, f should be a closure that, when called,
  # returns data
  if (is.null(f)) return(f)
  if (!is.function(f)) stop("Expected a closure", call. = FALSE)
  dat <- f()
  if (crosstalk::is.SharedData(dat)) {
    key <- dat$key()
    set <- dat$groupName()
    dat <- dat$origData()
    dat[[crosstalk_key()]] <- key
    # not allowed for list-columns!
    #dat <- dplyr::group_by_(dat, crosstalk_key(), add = TRUE)
    dat <- structure(dat, set = set)
  }
  prefix_class(dat, "plotly_data")
}

#' @export
print.plotly_data <- function(x, ...) {
  print(remove_class(tibble::as_tibble(x, ...), "plotly_data"))
  x
}

#' Highlight/query data based on primary key
#' 
#' This function simply creates an object of class [`crosstalk::SharedData`].
#' The reason it exists is to make it easier to teach others how to leverage its
#' functionality in plotly. It also makes it more discoverable if one is already
#' aware of [highlight()].
#' 
#' @param x a plotly visualization or a `data.frame`.
#' @param ... arguments passed to
#' [`crosstalk::SharedData$new()`][crosstalk::SharedData].
#' @export
#' @author Carson Sievert
#' @return An object of class [`crosstalk::SharedData`].
#' @seealso [highlight]
highlight_key <- function(x, ...) {
  UseMethod("highlight_key")
}

#' @export
highlight_key.plotly <- function(x, ...) {
  d <- plotly_data(x)
  add_data(x, crosstalk::SharedData$new(d, ...))
}

#' @export
highlight_key.default <- function(x, ...) {
  crosstalk::SharedData$new(x, ...)
}


# ---------------------------------------------------------------------------
# dplyr methods
# ---------------------------------------------------------------------------

#' @rdname plotly_data
groups.plotly <- function(x) {
  groups(plotly_data(x))
}

#' @rdname plotly_data
ungroup.plotly <- function(x, ...) {
  d <- ungroup(plotly_data(x), ...)
  add_data(x, d)
}

#' @rdname plotly_data
group_by.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), group_by, ...)
  if (crosstalk_key() %in% names(d)) {
    d <- preserve_set(d, group_by_add, !!rlang::sym(crosstalk_key()))
  }
  add_data(.data, d)
}

#' @rdname plotly_data
mutate.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), mutate, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
do.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), do, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
summarise.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), summarise, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
arrange.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), arrange, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
select.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), select, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
filter.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), filter, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
distinct.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), distinct, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
slice.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), slice, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
rename.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), rename, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
transmute.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), transmute, ...)
  add_data(.data, d)
}

# Apply a dplyr generic to a dataset while preserving the crosstalk 'set' attribute
preserve_set <- function(.data, func, ...) {
  structure(func(.data, ...), set = attr(.data, "set"))
}

# ------------------------------------------------------------
# Deprecated dplyr non-nse generics
# ------------------------------------------------------------

#' @rdname plotly_data
group_by_.plotly <- function(.data, ...) {
  d <- group_by_(plotly_data(.data), ...)
  # add crosstalk key as a group (to enable examples like demos/highlight-pipeline.R)
  if (crosstalk_key() %in% names(d)) {
    d <- group_by_add(d, !!rlang::sym(crosstalk_key()), add = TRUE)
  }
  add_data(.data, d)
}

#' @rdname plotly_data
mutate_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), mutate_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
do_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), do_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
summarise_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), summarise_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
arrange_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), arrange_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
select_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), select_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
filter_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), filter_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
distinct_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), distinct_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
slice_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), slice_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
rename_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), rename_, ...)
  add_data(.data, d)
}

#' @rdname plotly_data
transmute_.plotly <- function(.data, ...) {
  d <- preserve_set(plotly_data(.data), transmute_, ...)
  add_data(.data, d)
}

# ---------------------------------------------------------------------------
# tidyr methods
# waiting on https://github.com/tidyverse/tidyr/pull/229
# ---------------------------------------------------------------------------

# #' @rdname plotly_data
# #' @export
# gather_.plotly <- function(data, key_col, value_col, gather_cols, na.rm = FALSE,
#                            convert = FALSE, factor_key = FALSE) {
#   d <- plotly_data(data)
#   set <- attr(d, "set")
#   d <- tidyr::gather_(
#     d, key_col = key_col, value_col = value_col, gather_cols = gather_cols, 
#     na.rm = na.rm, convert = convert, factor_key = factor_key
#   )
#   add_data(data, structure(d, set = set))
# }
# 
# #' @importFrom dplyr select_vars
# #' @rdname plotly_data
# #' @export
# gather_vars.plotly <- function(data, key_col, value_col, ...) {
#   d <- plotly_data(data)
#   if (n_dots(...) == 0) {
#     setdiff(colnames(d), c(key_col, value_col))
#   } else {
#     unname(dplyr::select_vars(colnames(d), ...))
#   }
# }
# 
# n_dots <- function(...) nargs()


# ---------------------------------------------------------------------------
# miscellanous methods
# ---------------------------------------------------------------------------

# Avoid errors when passing a shared data to ggplot2
# qplot(data = crosstalk::SharedData$new(mtcars), mpg, wt)

#' @export
fortify.SharedData <- function(model, data, ...) {
  key <- model$key()
  set <- model$groupName()
  data <- model$origData()
  # need a consistent name so we know how to access it ggplotly()
  data[[crosstalk_key()]] <- key
  structure(data, set = set)
}

# yes, you can feed a plotly object into ggplot %^)
#' @export
ggplot.plotly <- function(data, mapping = aes(), ...,
                          environment = parent.frame()) {
  ggplot(plotly_data(data), mapping = mapping, ..., environment = environment)
}
