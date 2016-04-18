#' Convert R colours to RGBA hexadecimal colour values
#' @param x see the \code{col} argument in \code{col2rgb} for valid specifications
#' @param alpha alpha channel on 0-1 scale
#' @return hexadecimal colour value (if is.na(x), return "transparent" for compatibility with Plotly)
#' @export
toRGB <- function(x, alpha = 1) {
  # add alpha to already converted "rgb(x,y,z)" codes
  idx <- grepl("^rgb\\(", x) & alpha < 1 & 0 < alpha
  if (any(idx)) {
    x[idx] <- sub("^rgb", "rgba", x[idx])
    x[idx] <- paste0(sub("\\)", ",", x[idx]), alpha, ")")
  }
  # return code if 
  if (any(is.null(x) || grepl("^rgb[a]?\\(", x))) return(x)
  # for some reason ggplot2 has "NA" in some place (instead of NA)
  if (is.character(x)) {
    x[x == "NA"] <- NA
  }
  # as of ggplot2 version 1.1, an NA alpha is treated as though it's 1
  alpha[is.na(alpha)] <- 1
  rgb_matrix <- col2rgb(x, alpha = TRUE)
  # multiply the existing alpha with specified alpha (both on 0-1 scale)
  rgb_matrix["alpha", ] <- alpha * scales::rescale(
    rgb_matrix["alpha", ], from = c(0, 255)
  )
  container <- ifelse(rgb_matrix["alpha", ] == 1, "rgb(%s)", "rgba(%s)")
  rgba <- sprintf(container, apply(rgb_matrix, 2, paste, collapse = ","))
  rgba <- sub(",1\\)", ")", rgba)
  rgba[is.na(x)] <- "transparent"
  rgba
}
