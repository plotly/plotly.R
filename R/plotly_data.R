#' Obtain data associated with a plotly graph
#'
#' \code{plotly_data()} returns data associated with 
#' a plotly visualization (if there are multiple data frames, by default, 
#' it returns the most recent one). 
#' 
#' @param p a plotly visualization
#' @param id a character string or number referencing an "attribute layer".
#' @name plotly_data
#' 
#' @param .data a plotly visualization
#' @param x a plotly visualization
#' @param ... stuff passed onto the relevant method
#' @param add By default, when add = FALSE, group_by will override existing groups. 
#' To instead add to the existing groups, use add = TRUE
#' @param .dots Used to work around non-standard evaluation. See vignette("nse") for details
#' 
#' @export
#' @examples
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
  f <- p$x$visdat[[id]]
  # if data has been specified, f should be a closure that, when called,
  # returns data
  if (is.function(f)) return(tibble::as_tibble(f()))
  data.frame()
}

#' @rdname plotly_data
#' @export
groups.plotly <- function(x) {
  dplyr::groups(plotly_data(x))
}

#' @rdname plotly_data
#' @export
ungroup.plotly <- function(x, ...) {
  dplyr::ungroup(plotly_data(x))
}

#' @rdname plotly_data
#' @export
group_by_.plotly <- function(.data, ..., .dots, add = FALSE) {
  d <- plotly_data(.data)
  d <- dplyr::group_by_(d, .dots = lazyeval::all_dots(.dots, ...), add = add)
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
summarise_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::summarise_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
mutate_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::mutate_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
arrange_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::arrange_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
select_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::select_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
filter_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::filter_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
distinct_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::distinct_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
slice_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::slice_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
rename_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::rename_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rdname plotly_data
#' @export
transmute_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::transmute_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}
