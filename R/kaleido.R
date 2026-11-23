#' Static image exporter (deprecated)
#'
#' Static images are now made by [save_image()] (from plotly.core) with
#' headless Chrome, without Python or the kaleido Python package. `kaleido()`
#' is kept for existing code: it returns [plotly.core::chrome_exporter()],
#' which has the same `transform()` and `shutdown()` functions.
#'
#' @param ... Passed to [plotly.core::chrome_exporter()].
#' @return An environment with `transform()`, a function to convert plot
#'   objects into static images with the same arguments as [save_image()],
#'   and `shutdown()`, which stops the browser.
#' @export
#' @keywords internal
kaleido <- function(...) {
  .Deprecated(
    "save_image",
    package = "plotly",
    msg = paste(
      "kaleido() is deprecated: static images are now made with headless",
      "Chrome, without Python. Use save_image(), or",
      "plotly.core::chrome_exporter() for a reusable exporter."
    )
  )
  plotly.core::chrome_exporter(...)
}
