#' Modify trace styling
#'
#' @param data A data frame with a class of plotly.
#' @param traces numeric vector. Which traces should be modified?
#' @param ... arguments coerced to a list and used to modify trace(s)
#' @author Carson Sievert
#' @export
#'

# TODO: should it be possible to do NSE with style()?
style <- function(data = NULL, traces = 1, ...) {
  df <- if (is.null(data)) data.frame() else data
  p <- get_plot(df)
  ntraces <- length(p$data)
  idx <- traces %in% seq_len(ntraces)
  if (any(!idx)) warning("You referenced some traces that don't exist")
  p$data[traces] <- modifyList(p$data[traces], list(...))
  hash_plot(df, p)
}
