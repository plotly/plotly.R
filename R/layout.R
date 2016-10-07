#' Modify the layout of a plotly visualization
#' 
#' @param p A plotly object.
#' @param ... Arguments to the layout object. For documentation,
#' see \url{https://plot.ly/r/reference/#Layout_and_layout_style_objects}
#' @param data A data frame to associate with this layout (optional). If not 
#' provided, arguments are evaluated using the data frame in \code{\link{plot_ly}()}.
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
layout.plotly <- function(p, ..., data = NULL) {
  p <- add_data(p, data)
  attrs <- list(...)
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
rangeslider <- function(p, ...) {
  if (sum(grepl("^xaxis", names(p$x$layout))) > 1) {
    stop("Can only add a rangeslider to a plot with one x-axis", call. = FALSE)
  }
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
#' @author Carson Sievert
#' @export
#' @examples
#' 
#' config(plot_ly(), displaylogo = FALSE, collaborate = FALSE)
#' 

config <- function(p, ..., collaborate = TRUE, cloud = FALSE) {
  
  # make sure config is a list
  p$x$config <- p$x$config %||% list()
  
  nms <- sapply(p$x$config[["modeBarButtonsToAdd"]], "[[", "name")
  hasCollab <- sharingButton()[["name"]] %in% nms
  
  if (collaborate && !hasCollab) {
    nAdd <- length(p$x$config[["modeBarButtonsToAdd"]])
    p$x$config[["modeBarButtonsToAdd"]][[nAdd + 1]] <- sharingButton()
  }
  if (!collaborate) {
    p$x$config[["modeBarButtonsToAdd"]][nms %in% sharingButton()[["name"]]] <- NULL
  }

  hasCloud <- !'sendDataToCloud' %in% p$x$config[["modeBarButtonsToRemove"]]
  if (!cloud) {
    p$x$config[["modeBarButtonsToRemove"]] <- c(
      p$x$config[["modeBarButtonsToRemove"]], 'sendDataToCloud'
    )
  }
  if (cloud) {
    b <- p$x$config[["modeBarButtonsToRemove"]]
    p$x$config[["modeBarButtonsToRemove"]] <- b[!b %in% 'sendDataToCloud']
  }
  
  # ensure array
  if (length(p$x$config[["modeBarButtonsToRemove"]]) == 1) {
    p$x$config[["modeBarButtonsToRemove"]] <- list(p$x$config[["modeBarButtonsToRemove"]])
  }
  
  p$x$config <- modify_list(p$x$config, list(...))
  p
}
