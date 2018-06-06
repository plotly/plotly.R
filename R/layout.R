#' Modify the layout of a plotly visualization
#' 
#' @param p A plotly object.
#' @param ... Arguments to the layout object. For documentation,
#' see \url{https://plot.ly/r/reference/#Layout_and_layout_style_objects}
#' @param data A data frame to associate with this layout (optional). If not 
#' provided, arguments are evaluated using the data frame in [plot_ly()].
#' @author Carson Sievert
#' @export
layout <- function(p, ..., data = NULL) {
  UseMethod("layout")
}

#' @export
layout.matrix <- function(p, ..., data = NULL) {
  # workaround for the popular graphics::layout() function
  # https://github.com/ropensci/plotly/issues/464
  graphics::layout(p, ...)
}

#' @export
layout.shiny.tag.list <- function(p, ..., data = NULL) {
  idx <- which(vapply(p, is.plotly, logical(1)))
  for (i in idx) {
    p[[i]] <- layout.plotly(p[[i]], ..., data = NULL)
  }
  p
}

#' @export
layout.plotly <- function(p, ..., data = NULL) {
  p <- add_data(p, data)
  attrs <- list(...)
  if (!is.null(attrs[["height"]]) || !is.null(attrs[["width"]])) {
    warning("Specifying width/height in layout() is now deprecated.\n", 
            "Please specify in ggplotly() or plot_ly()", call. = FALSE)
  }
  # similar to add_trace()
  p$x$layoutAttrs <- c(
    p$x$layoutAttrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  p
}

#' Add a range slider to the x-axis
#'
#' @param p plotly object.
#' @param start a start date/value.
#' @param end an end date/value.
#' @param ... these arguments are documented here 
#' \url{https://plot.ly/r/reference/#layout-xaxis-rangeslider}
#' @export
#' @author Carson Sievert
#' @examples 
#' 
#' plot_ly(x = time(USAccDeaths), y = USAccDeaths) %>% 
#'   add_lines() %>%
#'   rangeslider()
#'   
#' d <- tibble::tibble(
#'   time = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by = "days"),
#'   y = rnorm(seq_along(time))
#'  )
#'  
#' plot_ly(d, x = ~time, y = ~y) %>%
#'   add_lines() %>%
#'   rangeslider(d$time[5], d$time[50])
#'   
#' 
rangeslider <- function(p, start = NULL, end = NULL, ...) {
  if (sum(grepl("^xaxis", names(p$x$layout))) > 1) {
    stop("Can only add a rangeslider to a plot with one x-axis", call. = FALSE)
  }
  
  p$x$layout$xaxis$range <- c(
    to_milliseconds(start),
    to_milliseconds(end)
  )
  
  p$x$layout$xaxis$rangeslider <- list(visible = TRUE, ...)
  p
}


#' Set the default configuration for plotly
#' 
#' @param p a plotly object
#' @param ... these arguments are documented at 
#' \url{https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js}
#' @param collaborate include the collaborate mode bar button (unique to the R pkg)?
#' @param cloud include the send data to cloud button?
#' @param locale locale to use. See [here](https://github.com/plotly/plotly.js/tree/master/dist#to-include-localization) for more info.
#' @param mathjax whether or not to add [MathJax rendering support](https://github.com/plotly/plotly.js/tree/master/dist#to-support-mathjax).
#' Note that plotly uses SVG-based mathjax rendering which won't play nicely with 
#' HTML-based rendering (e.g., rmarkdown documents). In this case, you may want to 
#' consider `<iframe>`-ing your plotly graph(s) into the larger document.
#' Currently mathjax is loaded externally (meaning an internet
#' connection is needed to view the graph). Future versions may not have this requirement.
#' @author Carson Sievert
#' @export
#' @examples
#' 
#' today <- Sys.Date()
#' x <- seq.Date(today, today + 360, by = "day")
#' p <- plot_ly(x = x, y = rnorm(length(x))) %>%
#'   add_lines()
#' 
#' # remove the plotly logo and collaborate button from modebar
#' config(p, displaylogo = FALSE, collaborate = FALSE)
#' 
#' # enable mathjax
#' # see more examples at https://plot.ly/r/LaTeX/
#' plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16)) %>%
#'   layout(title = "$\\text{Some mathjax: }\\alpha+\\beta x$") %>%
#'   config(mathjax = TRUE)
#' 
#' # japanese
#' config(p, locale = "ja")
#' # german
#' config(p, locale = "de")
#' # spanish
#' config(p, locale = "es")
#' # chinese
#' config(p, locale = "zh-CN")
#' 

config <- function(p, ..., collaborate = TRUE, cloud = FALSE, locale = NULL, mathjax = FALSE) {
  
  if (!is.null(locale)) {
    p$dependencies <- c(
      p$dependencies,
      list(locale_dependency(locale))
    )
    p$x$config$locale <- locale
  }
  
  if (!identical(mathjax, FALSE)) {
    p$dependencies <- c(
      list(mathjax_cdn()),
      p$dependencies
    )
  }
  
  p$x$config <- modify_list(p$x$config, list(...))
  
  nms <- sapply(p$x$config[["modeBarButtonsToAdd"]], "[[", "name")
  hasCollab <- sharingButton()[["name"]] %in% nms
  
  if (collaborate && !hasCollab) {
    nAdd <- length(p$x$config[["modeBarButtonsToAdd"]])
    p$x$config[["modeBarButtonsToAdd"]][[nAdd + 1]] <- sharingButton()
  }
  if (!collaborate) {
    p$x$config[["modeBarButtonsToAdd"]][nms %in% sharingButton()[["name"]]] <- NULL
  }

  p$x$config$cloud <- cloud

  p
}
