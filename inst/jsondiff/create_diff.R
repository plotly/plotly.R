#' Create an HTML page showing difference between two plotlys
#' 
#' @param Old a plotly figure url
#' @param New a plotly figure url
#' @param Dir a directory to place the result
#' @example
#' create_diff(
#'  "https://plot.ly/~sfg/2141.png",
#'  "https://plot.ly/~sfg/2143.png"
#' )
create_diff <- function(Old, New, Dir = ".") {
  if (!dir.exists(Dir)) dir.create(Dir, showWarnings = FALSE, recursive = TRUE)
  Dir <- normalizePath(Dir, mustWork = TRUE)
  curl::curl_download(New, file.path(Dir, "New.png"))
  curl::curl_download(Old, file.path(Dir, "Old.png"))
  get_id <- function(x) sub(".*/([0-9]+).*", "\\1", x)
  get_usr <- function(x) sub(".*/~(.*)/.*", "\\1", x)
  p_old <- plotly_build(
    get_figure(get_usr(Old), get_id(Old))
  )
  p_new <- plotly_build(
    get_figure(get_usr(New), get_id(New))
  )
  json_old <- plotly:::to_JSON(p_old)
  writeLines(paste("Old =", json_old), file.path(Dir, "Old.json"))
  json_new <- plotly:::to_JSON(p_new)
  writeLines(paste("New =", json_new), file.path(Dir, "New.json"))
  invisible(NULL)
}
