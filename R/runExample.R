#' Run a plotly example(s)
#' 
#' Provides a unified interface for running demos, shiny apps, and Rmd documents
#' which are bundled with the package.
#' 
#' @param type the type of example
#' @param name the name of the example (valid names depend on \code{type}).
#' @param ... arguments passed onto the suitable method.
#' @export
#' @author Carson Sievert

runExample <- function(type = c("demo", "shiny", "rmd"), name, ...) {
  if (missing(name)) {
    stop("Must provide an example name", call. = FALSE)
  }
  
  type <- match.arg(type, type)
  
  if (type == "demo") {
    utils::demo(topic = name, package = "plotly")
  }
  
  # check to make sure the example exists
  exampleDir <- system.file("examples", type, package = "plotly")
  nms <- basename(list.dirs(exampleDir, recursive = FALSE))
  if (!isTRUE(name %in% nms)) {
    stop(
      sprintf(
        "Couldn't find that %s example. Valid examples include: '%s'", 
        type, paste(nms, collapse = "', '")
      ), 
      .call = FALSE
    )
  }
  
  finalDir <- system.file("examples", type, name, package = "plotly")
  
  if (type == "shiny") {
    try_library("shiny", "runExample")
    getFromNamespace("runApp", asNamespace("shiny"))(finalDir, ...)
  }
  
  if (type == "rmd") {
    try_library("rmarkdown", "runExample")
    input <- file.path(finalDir, "index.Rmd")
    output <- tempfile(fileext = ".html")
    getFromNamespace("render", asNamespace("rmarkdown"))(input, output_file = output, ...)
    browseURL(output)
  }
  
  invisible(NULL)
}
