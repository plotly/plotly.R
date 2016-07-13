#' Convert R colours to RGBA hexadecimal colour values
#' @param x see the \code{col} argument in \code{col2rgb} for valid specifications
#' @param alpha alpha channel on 0-1 scale
#' @return hexadecimal colour value (if is.na(x), return "transparent" for compatibility with Plotly)
#' @export
toRGB <- function(x, alpha = 1) {
  if (length(x) == 0) return(x)
  if (any(x %in% "transparent")) return(x)
  # add alpha to already converted "rgb(x,y,z)" codes
  idx <- grepl("^rgba\\(", x) & alpha <= 1 & 0 <= alpha
  if (any(idx)) {
    x[idx] <- rgb2hex(x[idx])
  }
  # for some reason ggplot2 has "NA" in some place (instead of NA)
  if (is.character(x)) {
    x[x == "NA"] <- NA
  }
  # as of ggplot2 version 1.1, an NA alpha is treated as though it's 1
  alpha[is.na(alpha)] <- 1
  rgb_matrix <- grDevices::col2rgb(x, alpha = TRUE)
  # multiply the existing alpha with specified alpha (both on 0-1 scale)
  rgb_matrix["alpha", ] <- alpha * scales::rescale(
    rgb_matrix["alpha", ], from = c(0, 255)
  )
  rgb_matrix["alpha", ] <- round(rgb_matrix["alpha", ], 4)
  rgba <- sprintf("rgba(%s)", apply(rgb_matrix, 2, paste, collapse = ","))
  
  rgba[is.na(x)] <- "transparent"
  rgba
}

# take a 'plotly color' and produce a hex code
rgb2hex <- function(string = "rgba(255,255,255,1)") {
  vals <- sub("rgba\\(", "", sub("\\)", "", string))
  valz <- strsplit(vals, ",")
  sapply(valz, function(x) {
    x <- setNames(as.numeric(x), c("red", "green", "blue", "alpha"))
    x[["alpha"]] <- x[["alpha"]] * 255
    do.call(grDevices::rgb, c(x, list(maxColorValue = 255)))
  })
}
