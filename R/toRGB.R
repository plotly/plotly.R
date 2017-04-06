#' Convert R colours to RGBA hexadecimal colour values
#' @param x see the \code{col} argument in \code{col2rgb} for valid specifications
#' @param alpha alpha channel on 0-1 scale
#' @return hexadecimal colour value (if is.na(x), return "transparent" for compatibility with Plotly)
#' @export
#' @seealso \code{\link{showRGB}()}
#' @examples 
#' 
#' toRGB("steelblue") 
#' # [1] "rgba(70,130,180,1)"
#' 
#' m <- list(
#'   color = toRGB("red"),
#'   line = list(
#'     color = toRGB("black"), 
#'     width = 19
#'   )
#' )
#' 
#' plot_ly(x = 1, y = 1, marker = m)
#' 
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


#' View colors already formatted by toRGB()
#' 
#' Useful for viewing colors after they've been converted to plotly.js'
#' color format -- "rgba(255, 255, 255, 1)"
#' 
#' @param A character string specifying color(s).
#' @param ... arguments passed along to \code{scales::show_col}.
#' @export
#' @author Carson Sievert
#' @examples
#' 
#' showRGB(toRGB(colors()), labels = FALSE)
showRGB <- function(x, ...) {
  if (!all(grepl("^rgba", x))) stop("All values must begin with rgba")
  cols <- lapply(strsplit(x, ","), function(y) {
    vals <- as.numeric(gsub("rgba\\(|\\)", "", y))
    vals[1:3] <- vals[1:3] / 255
    setNames(as.list(vals), c("red", "green", "blue", "alpha"))
  })
  rgbs <- sapply(cols, function(x) do.call(rgb, x))
  scales::show_col(rgbs, ...)
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

#convert rgba() to rgb()
strip_alpha <- function(string = "rgba(255,255,255,1)", warn = TRUE) {
  if (length(string) == 0) return(string)
  if (warn) {
    if (any(!grepl(",1\\)$", string))) {
      warning("Removing an alpha value lower than 1")
    }
  }
  string <- sub(",1\\)$", ")", string)
  sub("^rgba", "rgb", string)
}
