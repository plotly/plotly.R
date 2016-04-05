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
#' @param out_file A filename for writing the image to a file.
#' @param ... arguments passed onto \code{httr::POST}
#' @export
#' @examples \dontrun{
#' p <- plot_ly(x = 1:10)
#' Png <- plotly_IMAGE(p, out_file = "plotly-test-image.png")
#' Jpeg <- plotly_IMAGE(p, format = "jpeg", out_file = "plotly-test-image.jpeg")
#' Svg <- plotly_IMAGE(p, format = "svg",  out_file = "plotly-test-image.svg")
#' Pdf <- plotly_IMAGE(p, format = "pdf",  out_file = "plotly-test-image.pdf")
#' }
#' 

plotly_IMAGE <- function(x, width = 1000, height = 500, format = "png", 
                         scale = 1, out_file, ...) {
  x <- plotly_build(x)
  
  bod <- list(
    figure = x[c("data", "layout")],
    width = width,
    height = height,
    format = format,
    scale = scale,
    encoded = FALSE
  )
  base_url <- file.path(get_domain("api"), "v2", "images")
  resp <- httr::POST(base_url, plotly_headers("v2"), body = to_JSON(bod), 
                     if (!missing(out_file)) httr::write_disk(out_file, overwrite = TRUE), 
                     ...)
  con <- process(append_class(resp, "image"))
  invisible(con)
}
