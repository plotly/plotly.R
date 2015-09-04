#' Create/Modify plotly images
#'
#' The images endpoint turn a plot (which may be given in multiple forms) 
#' into an image of the desired format.
#'
#' @param x either a plotly object or a list.
#' @param width Image width in pixels
#' @param height Image height in pixels
#' @param format The desired image format 'png', 'jpeg', 'svg', 'pdf', 'eps', or 'webp'
#' @param scale Both png and jpeg formats will be scaled beyond the specified width and height by this number.
#' @param encoded A boolean flag for base64 encoding of bytes
#' @export
#' @examples
#' 
#' plotly_IMAGE(plot_ly(x = 1:10))
#' 
#' 

plotly_IMAGE <- function(x, width = 1000, height = 500, format = "png", 
                         scale = 4, encoded = FALSE) {
  x <- plotly_build(x)
  
  bod <- list(
    figure = to_JSON(x[c("data", "layout")]),
    width = width,
    height = height,
    format = format,
    scale = scale,
    encoded = encoded
  )
  
  base_url <- file.path(get_domain("v2"), "images")
  resp <- httr::POST(base_url, plotly_headers("v2"), body = bod)
  con <- process(struct(resp, "image"))
  
}
