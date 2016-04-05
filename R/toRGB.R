#' Convert R colours to RGBA hexadecimal colour values
#' @param x see the \code{col} argument in \code{col2rgb} for valid specifications
#' @param alpha alpha channel on 0-1 scale
#' @return hexadecimal colour value (if is.na(x), return "transparent" for compatibility with Plotly)
#' @export
toRGB <- function(x, alpha = 1) {
  if (is.null(x)) return(x)
  # as of ggplot2 version 1.1, an NA alpha is treated as though it's 1
  alpha[is.na(alpha)] <- 1
  # if we've already made the proper conversion, return the input
  if (inherits(x, "plotly_rgba")) return(x)
  if (inherits(x, "plotly_rgb")) {
    if (all(alpha == 1)) return(x)
    # all alpha channel
    x <- sub("^rgb", "rgba", sub("\\)", paste0(",", alpha, ")"), x))
    return(prefix_class(x, "plotly_rgba"))
  }
  # for some reason ggplot2 has "NA" in some place (instead of NA)
  if (is.character(x)) {
    x[x == "NA"] <- NA
  }
  has_alpha <- all(0 <= alpha & alpha < 1)
  rgb_matrix <- col2rgb(x, alpha = has_alpha)
  # rescale alpha 
  # TODO: what if x already has an alpha channel???
  if (has_alpha) rgb_matrix["alpha", ] <- alpha
  container <- if (has_alpha) "rgba(%s)" else "rgb(%s)"
  rgb_a <- sprintf(container, apply(rgb_matrix, 2, paste, collapse = ","))
  rgb_a[is.na(x)] <- "transparent"
  structure(rgb_a, class = if (has_alpha) "plotly_rgba" else "plotly_rgb")
}
