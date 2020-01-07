#' Create a static image
#'
#' The images endpoint turns a plot (which may be given in multiple forms)
#' into an image of the desired format.
#'
#' @param x either a plotly object or a list.
#' @param width Image width in pixels
#' @param height Image height in pixels
#' @param format The desired image format 'png', 'jpeg', 'svg', 'pdf', 'eps', or 'webp'
#' @param scale Both png and jpeg formats will be scaled beyond the specified width and height by this number.
#' @param out_file A filename for writing the image to a file.
#' @param ...  arguments passed onto `httr::RETRY`
#' @importFrom httr RETRY write_disk
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
  x <- plotly_build(x)[["x"]]

  bod <- list(
    figure = x[c("data", "layout")],
    width = width,
    height = height,
    format = format,
    scale = scale,
    encoded = FALSE,
    filename = Sys.time()
  )
  base_url <- file.path(get_domain("api"), "v2", "images")
  resp <- httr::RETRY(
    verb = "POST"
    , url = base_url
    , body = to_JSON(bod)
    , times = 5
    , terminate_on = c(400, 401, 403, 404)
    , terminate_on_success = TRUE
    , api_headers()
    , api_auth()
    , if (!missing(out_file)) httr::write_disk(out_file, overwrite = TRUE)
    , ...
  )
  con <- process(append_class(resp, "api_image"))
  invisible(con)
}
