#' Initiate a plotly visualization
#'
#' Transform data into a plotly visualization.
#' 
#' There are a number of "visual properties" that aren't included in the officical 
#' Reference section (see below). 
#' 
#' @param data A data frame (optional).
#' @param ... These arguments are documented at \url{https://plot.ly/r/reference/}
#' Note that acceptable arguments depend on the value of \code{type}.
#' @param type A character string describing the type of trace.
#' @param group Either a variable name or a vector to use for grouping. If used, 
#' a different trace will be created for each unique value.
#' @param color Either a variable name or a vector to use for color mapping.
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like \code{colorRamp()}.
#' @param symbol Either a variable name or a (discrete) vector to use for symbol encoding.
#' @param symbols A character vector of symbol types. Possible values:
#' 'dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up' 
#' @param size A variable name or numeric vector to encode the size of markers.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param inherit logical. Should future traces inherit properties from this initial trace?
#' @param source Only relevant for \link{event_data}.
#' @seealso \code{\link{layout}()}, \code{\link{add_trace}()}, \code{\link{style}()}
#' @author Carson Sievert
#' @export
#' @examples
#' \dontrun{
#' data(economics, package = "ggplot2")
#' # basic time-series plot
#' p <- plot_ly(economics, x = ~date, y = ~uempmed, inherit = TRUE)
#' # add a loess smoother
#' p2 <- add_trace(p, y = ~fitted(loess(uempmed ~ as.numeric(date))))
#' # add a title
#' p3 <- layout(p2, title = "Median duration of unemployment (in weeks)")
#' # change the font
#' layout(p3, font = list(family = "Courier New, monospace"))
#' 
#' # using the color argument
#' plot_ly(economics, x = ~date, y = ~unemploy / pop, color = ~pop, mode = "markers")
#' plot_ly(economics, x = ~date, y = ~unemploy / pop, color = ~pop, 
#'   colors = terrain.colors(5), mode = "markers")
#'   
#' # function to extract the decade of a given date
#' decade <- function(x) {
#'   factor(floor(as.numeric(format(x, "%Y")) / 10) * 10)
#' }
#' plot_ly(economics, x = ~unemploy / pop, color = ~decade(date), type = "box")
#' 
#' # sometimes, a data frame isn't fit for the use case...
#' # for 3D surface plots, a numeric matrix is more natural
#' plot_ly(z = volcano, type = "surface")
#' 
#' }
#' 
plot_ly <- function(data = data.frame(), ..., type = NULL,
                    group, color, colors, symbol, symbols, size,
                    width = NULL, height = NULL, inherit = FALSE,
                    source = "A") {
  # "native" plotly arguments
  argz <- list(...)
  # old arguments to this function that are no longer supported
  if (!is.null(argz$filename)) 
    warning("Ignoring filename. Use plotly_POST() if you want to post figures to plotly.")
  if (!is.null(argz$fileopt)) 
    warning("Ignoring fileopt. Use plotly_POST() if you want to post figures to plotly.")
  if (!is.null(argz$world_readable)) 
    warning("Ignoring world_readable. Use plotly_POST() if you want to post figures to plotly.")
  # tack on "special" arguments
  argz$group <- verify_arg(group)
  argz$color <- verify_arg(color)
  argz$symbol <- verify_arg(symbol)
  argz$size <- verify_arg(size)
  # TODO: do we really need to verify these?
  argz$colors <- verify_arg(colors)
  argz$symbols <- verify_arg(symbols)
  argz$type <- verify_type(type)
  
  # id for tracking attribute mappings and finding the most current data
  id <- new_id()
  p <- list(
    visdat = setNames(list(function() data), id),
    cur_data = id,
    attrs = setNames(list(argz), id),
    # we always deal with a _list_ of traces and _list_ of layouts 
    # since they can each have different data
    layout = list(
        width = width, 
        height = height,
        # sane margin defaults (mainly for RStudio)
        margin = list(b = 40, l = 60, t = 25, r = 10)
    ),
    config = list(modeBarButtonsToRemove = I("sendDataToCloud")),
    inherit = inherit,
    base_url = get_domain()
  )
  
  as.widget(p)
}
