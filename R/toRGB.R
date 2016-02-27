#' Convert R colours to RGBA hexadecimal colour values
#' @param x character for colour, for example: "white"
#' @param alpha transparency alpha
#' @return hexadecimal colour value (if is.na(x), return "transparent" for compatibility with Plotly)
#' @export
toRGB <- function(x, alpha = 1) {
  if (is.null(x)) return(x)
  if (all(grepl("rgb\\(", x))) return(x)
  # for some reason ggplot2 has "NA" in some place (instead of NA)
  if (is.character(x)) {
    x[x == "NA"] <- NA
  }
  # as of ggplot2 version 1.1, an NA alpha is treated as though it's 1
  alpha[is.na(alpha)] <- 1
  if (any(alpha != 1)) {
    rgb.matrix <- col2rgb(x, TRUE)
    rgb.matrix["alpha", ] <- alpha
    ch.vector <- "rgba(%s)"
  } else {
    rgb.matrix <- col2rgb(x)
    ch.vector <- "rgb(%s)"
  }
  rgb.text <- apply(rgb.matrix, 2, paste, collapse=",")
  rgb.css <- sprintf(ch.vector, rgb.text)
  ifelse(is.na(x), "transparent", rgb.css)
}
