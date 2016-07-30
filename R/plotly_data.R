#' Obtain data associated with a plotly graph
#'
#' \code{plotly_data()} returns data associated with 
#' a plotly visualization (if there are multiple data frames, by default, 
#' it returns the most recent one). 
#' 
#' @param p a plotly visualization
#' @param id a character string or number referencing an "attribute layer".
#' @return returns a data frame
#' @examples
#' @export
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
#'   left_join(plots) %>%
#'   arrange(mn) %>%
#'   .[["p"]] %>%
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

#' @rawNamespace export(groups.plotly)
groups.plotly <- function(x) {
  dplyr::groups(plotly_data(x))
}

#' @rawNamespace export(ungroup.plotly)
ungroup.plotly <- function(x) {
  dplyr::ungroup(plotly_data(x))
}

#' @rawNamespace export(group_by_.plotly)
group_by_.plotly <- function(.data, ..., .dots, add = FALSE) {
  d <- plotly_data(.data)
  d <- dplyr::group_by_(d, .dots = lazyeval::all_dots(.dots, ...), add = add)
  add_data(.data, d)
}

#' @rawNamespace export(summarise_.plotly)
summarise_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::summarise_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(mutate_.plotly)
mutate_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::mutate_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(arrange_.plotly)
arrange_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::arrange_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(select_.plotly)
select_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::select_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(filter_.plotly)
filter_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::filter_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(distinct_.plotly)
distinct_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::distinct_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(slice_.plotly)
slice_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::slice_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(rename_.plotly)
rename_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::rename_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}

#' @rawNamespace export(transmute_.plotly)
transmute_.plotly <- function(.data, ..., .dots) {
  d <- plotly_data(.data)
  d <- dplyr::transmute_(d, .dots = lazyeval::all_dots(.dots, ...))
  add_data(.data, d)
}


# Avoid errors when passing a shared data to ggplot2
# qplot(data = crosstalk::SharedData$new(mtcars), mpg, wt)

#' @rawNamespace export(fortify.SharedData)
fortify.SharedData <- function(model, data, ...) {
  key <- model$key()
  set <- model$groupName()
  data <- model$origData()
  # need a consistent name for the keyso we know how to access it ggplotly()
  data[[".crossTalkKey"]] <- key
  structure(data, set = set)
}
