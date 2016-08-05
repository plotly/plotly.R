#' Initiate a plotly visualization
#'
#' Transform data into a plotly visualization.
#'
#' There are a number of "visual properties" that aren't included in the official
#' Reference section (see below).
#'
#' @param data A data frame (optional) or \code{\link[crosstalk]{SharedData}} object.
#' @param ... These arguments are documented at \url{https://plot.ly/r/reference/}
#' Note that acceptable arguments depend on the value of \code{type}.
#' @param type A character string describing the type of trace. If \code{NULL}
#' (the default), the initial trace type is determined by \code{add_trace}
#' @param color A formula containing a name or expression. 
#' Values are scaled and mapped to color codes based on the value of 
#' \code{colors} and \code{alpha}. To avoid scaling, wrap with \code{\link{I}()},
#' and provide value(s) that can be converted to rgb color codes by 
#' \code{\link[grDevices]{col2rgb}()}.
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like \code{colorRamp()}.
#' @param alpha A number between 0 and 1 specifying the alpha channel applied to color.
#' @param symbol A formula containing a name or expression. 
#' Values are scaled and mapped to symbols based on the value of \code{symbols}.
#' To avoid scaling, wrap with \code{\link{I}()}, and provide valid 
#' \code{\link{pch}()} values and/or valid plotly symbol(s) as a string
#' \url{}
#' @param symbols A character vector of symbol types. For possible values, see \link{schema}.
#' @param linetype Either a variable name or a (discrete) vector to use for linetype encoding.
#' @param linetypes A character vector of line types. For possible values, see \link{schema}.
#' @param size A variable name or numeric vector to encode the size of markers.
#' @param sizes A numeric vector of length 2 used to scale sizes to pixels.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param crosstalkOpts a list of options for controlling the type and 
#' appearance of client-side interaction. For details, see \code{\link{ct_opts}()}.
#' @seealso \code{\link{ggplotly}()}
#' @author Carson Sievert
#' @export
#' @examples
#' \dontrun{
#' 
#' # plot_ly() tries to create a sensible plot based on the information you 
#' # give it. If you don't provide a trace type, plot_ly() will infer one.
#' plot_ly(economics, x = ~pop)
#' plot_ly(economics, x = ~date, y = ~pop)
#' # plot_ly() doesn't require data frame(s), which allows one to take 
#' # advantage of trace type(s) designed specifically for numeric matrices
#' plot_ly(z = ~volcano)
#' plot_ly(z = ~volcano, type = "surface")
#' 
#' # plotly has a functional interface: every plotly function takes a plotly
#' # object as it's first input argument and returns a modified plotly object
#' add_lines(plot_ly(economics, x = ~date, y = ~unemploy/pop))
#' 
#' # To make code more readable, plotly imports the pipe operator from magrittr
#' economics %>% plot_ly(x = ~date, y = ~unemploy/pop) %>% add_lines()
#' 
#' # Attributes defined via plot_ly() set 'global' attributes that 
#' # are carried onto subsequent traces
#' plot_ly(economics, x = ~date, line = list(color = "black")) %>%
#'  add_trace(y = ~uempmed, mode = "markers+lines") %>%
#'  add_lines(y = ~psavert) %>%
#'  layout(title = "Setting global trace attributes")
#' 
#' # Attributes are documented in the figure reference -> https://plot.ly/r/reference
#' # You might notice plot_ly() has named arguments that aren't in the figure
#' # reference. These arguments make it easier to map abstract data values to
#' # visual attributes.
#' p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length) 
#' add_markers(p, color = ~Petal.Length, size = ~Petal.Length)
#' add_markers(p, color = ~Species)
#' add_markers(p, color = ~Species, colors = "Set1")
#' add_markers(p, symbol = ~Species)
#' add_paths(p, linetype = ~Species)
#' 
#' # client-side linked brushing
#' library(crosstalk)
#' sd <- SharedData$new(mtcars)
#' o <- ct_opts(color = "red")
#' subplot(
#'   plot_ly(sd, x = ~wt, y = ~mpg, color = I("black"), crosstalkOpts = o),
#'   plot_ly(sd, x = ~wt, y = ~disp, color = I("black"), crosstalkOpts = o)
#' ) %>% hide_legend() %>% plotly_json()
#' 
#' # client-side highlighting
#' d <- SharedData$new(txhousing, ~city)
#' ct <- ct_opts(color = "red")
#' plot_ly(d, x = ~date, y = ~median, color = I("black"), crosstalkOpts = ct) %>%
#'   group_by(city) %>%
#'   add_lines() %>% 
#'   add_markers(size = I(0.01))
#' 
#' }
#'

plot_ly <- function(data = data.frame(), ..., type = NULL, group,
                    color, colors = NULL, alpha = 1, symbol, symbols = NULL, 
                    size, sizes = c(10, 100), linetype, linetypes = NULL,
                    width = NULL, height = NULL, crosstalkOpts = ct_opts()) {
  
  is_sd <- crosstalk::is.SharedData(data)
  if (is_sd) {
    key <- data$key()
    set <- data$groupName()
    data <- data$origData()
  } else {
    key <- NULL
    set <- NULL
  }
  
  if (!is.data.frame(data)) {
    stop("First argument, `data`, must be a data frame or shared data.", call. = FALSE)
  }
  
  # "native" plotly arguments
  attrs <- list(...)
  # warn about old arguments that are no longer supported
  for (i in c("filename", "fileopt", "world_readable")) {
    if (is.null(attrs[[i]])) next
    warning("Ignoring ", i, ". Use `plotly_POST()` if you want to post figures to plotly.")
    attrs[[i]] <- NULL
  }
  if (!is.null(attrs[["group"]])) {
    warning(
      "The group argument has been deprecated. Use `group_by()` instead.\n",
      "See `help('plotly-data')` for examples"
    )
    attrs[["group"]] <- NULL
  }
  if (!is.null(attrs[["inherit"]])) {
    warning("The inherit argument has been deprecated.")
    attrs[["inherit"]] <- NULL
  }
  
  # tack on variable mappings
  attrs$color <- if (!missing(color)) color
  attrs$symbol <- if (!missing(symbol)) symbol
  attrs$linetype <- if (!missing(linetype)) linetype
  attrs$size <- if (!missing(size)) size
  
  # tack on scale ranges
  attrs$colors <- colors
  attrs$alpha <- alpha
  attrs$symbols <- symbols
  attrs$linetypes <- linetypes
  attrs$sizes <- sizes
  attrs$type <- type
  
  # tack on crosstalk stuffs
  attrs$key <- key
  attrs$set <- set
  attrs$crosstalk <- crosstalkOpts
  
  # id for tracking attribute mappings and finding the most current data
  id <- new_id()
  # avoid weird naming clashes
  plotlyVisDat <- data
  p <- list(
    visdat = setNames(list(function() plotlyVisDat), id),
    cur_data = id,
    attrs = setNames(list(attrs), id),
    # we always deal with a _list_ of traces and _list_ of layouts 
    # since they can each have different data
    layout = list(
      width = width, 
      height = height,
      # sane margin defaults (mainly for RStudio)
      margin = list(b = 40, l = 60, t = 25, r = 10),
      dragmode = if (is_sd) "lasso",
      hovermode = if (is_sd) "closest"
    ),
    config = list(modeBarButtonsToRemove = I("sendDataToCloud")),
    base_url = get_domain()
  )
  
  as_widget(p)
}


# Convert a list to a plotly htmlwidget object
# 
# @param x a plotly object.
# @param ... other options passed onto \code{htmlwidgets::createWidget}
# 

as_widget <- function(x, ...) {
  if (inherits(x, "htmlwidget")) return(x)
  # add plotly class mainly for printing method
  # customize the JSON serializer (for htmlwidgets)
  attr(x, 'TOJSON_FUNC') <- to_JSON
  htmlwidgets::createWidget(
    name = "plotly",
    x = x,
    width = x$layout$width,
    height = x$layout$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE,
      defaultWidth = '100%',
      defaultHeight = 400
    ),
    preRenderHook = plotly_build,
    dependencies = crosstalk::crosstalkLibs()
  )
}
