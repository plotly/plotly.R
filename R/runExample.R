#' Run a plotly example(s)
#' 
#' Provides a unified interface for running demos, shiny apps, and Rmd documents
#' which are bundled with the package.
#' 
#' @param type the type of example
#' @param name the name of the example (valid names depend on \code{type}).
#' @export
#' @author Carson Sievert
#' @examples 
#' 
#' \dontrun{
#' runExample("shiny", "Diamonds")
#' }

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
    shiny::runApp(finalDir, ...)
  }
  
  if (type == "rmd") {
    input <- file.path(finalDir, "index.Rmd")
    output <- tempfile(fileext = ".html")
    rmarkdown::render(input, output_file = output, ...)
    browseURL(output)
  }
  
  invisible(NULL)
}
