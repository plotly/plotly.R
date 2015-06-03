is.style <- function(x) inherits(x, "style")

#' Modify trace styling
#'
#' @param traces numeric vector. Which traces should be modified?
#' @param ... arguments coerced to a list and used to modify trace(s)
#' @author Carson Sievert
#'

style <- function(traces = 1, ...) {
  structure(
    c(traces = traces, list(...)), 
    class = "style"
  )
}
