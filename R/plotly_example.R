#' Run a plotly example(s)
#' 
#' Provides a unified interface for running demos, shiny apps, and Rmd documents
#' which are bundled with the package.
#' 
#' @param type the type of example
#' @param name the name of the example (valid names depend on `type`).
#' @param ... arguments passed onto the suitable method.
#' @export
#' @author Carson Sievert

plotly_example <- function(type = c("demo", "shiny", "rmd"), name, ...) {
  
  type <- match.arg(type)
  
  # demos don't necessarily need a name
  if (type == "demo") {
    if (missing(name)) {
      return(utils::demo(package = "plotly")) 
    } else  {
      return(utils::demo(topic = name, package = "plotly"))
    }
  }
  
  # check to make sure the example exists
  exampleDir <- system.file("examples", type, package = "plotly")
  nms <- basename(list.dirs(exampleDir, recursive = FALSE))
  if (missing(name) || !isTRUE(name %in% nms)) {
    message(
      sprintf(
        "Couldn't find that %s example. Valid examples include: '%s'", 
        type, paste(nms, collapse = "', '")
      )
    )
    return(invisible())
  }
  
  finalDir <- system.file("examples", type, name, package = "plotly")
  
  if (type == "shiny") {
    try_library("shiny", "plotly_example")
    getFromNamespace("runApp", asNamespace("shiny"))(finalDir, display.mode = "showcase", ...)
  }
  
  if (type == "rmd") {
    try_library("rmarkdown", "plotly_example")
    input <- file.path(finalDir, "index.Rmd")
    output <- tempfile(fileext = ".html")
    getFromNamespace("render", asNamespace("rmarkdown"))(input, output_file = output, ...)
    browseURL(output)
  }
  
  invisible(NULL)
}
