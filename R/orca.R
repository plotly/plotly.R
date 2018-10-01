#' Static image export via orca
#' 
#' The function makes a system call to the orca command-line utility, 
#' see the installation instructions [here](https://github.com/plotly/orca#installation)
#' 
#' @param p a plotly object.
#' @param file output filename.
#' @param format the output format (png, jpeg, webp, svg, pdf, eps).
#' @param scale Sets the image scale. Applies to all output images.
#' @param width Sets the image width. If not set, defaults to `layout.width` value. 
#' Applies to all output images.
#' @param height Sets the image height. If not set, defaults to `layout.height` value. 
#' Applies to all output images.
#' @param mathjax whether or not to include MathJax (required to render [TeX]).
#' If `TRUE`, the PLOTLY_MATHJAX_PATH environment variable must be set and point 
#' to the location of MathJax (this variable is also used to render [TeX] in 
#' interactive graphs, see [config]).
#' @param parallel_limit Sets the limit of parallel tasks run.
#' @param verbose Turn on verbose logging on stdout.
#' @param debug Starts app in debug mode and turn on verbose logs on stdout.
#' @param safe Turns on safe mode: where figures likely to make browser window 
#' hang during image generating are skipped.
#' @export
#' @author Carson Sievert
#' @seealso [orca_serve]
#' @examples
#' 
#' \dontrun{
#' p <- plot_ly(z = ~volcano) %>% add_surface()
#' orca(p, "surface-plot.svg")
#' }
#' 

orca <- function(p, file = "plot.png", format = tools::file_ext(file), 
                 scale = NULL, width = NULL, height = NULL, mathjax = FALSE,
                 parallel_limit = NULL, verbose = FALSE, debug = FALSE, 
                 safe = FALSE) {
  
  orca_available()
  
  b <- plotly_build(p)
  
  # find the relevant plotly.js bundle
  plotlyjs <- plotlyjsBundle(b)
  plotlyjs_file <- file.path(plotlyjs$src$file, plotlyjs$script)
  
  tmp <- tempfile(fileext = ".json")
  cat(to_JSON(b$x[c("data", "layout")]), file = tmp)
  
  args <- c(
    "graph", tmp, 
    "-o", file,
    "--format", format,
    "--plotlyjs", plotlyjs_file,
    if (debug) "--debug",
    if (verbose) "--verbose",
    if (safe) "--safe-mode"
  )
  
  if (!is.null(scale)) args <- c(args, "--scale", scale)
  if (!is.null(width)) args <- c(args, "--width", width)
  if (!is.null(height)) args <- c(args, "--height", height)
  if (!is.null(parallel_limit)) args <- c(args, "--parallel-limit", parallel_limit)
  if (!is.null(tryNULL(mapbox_token()))) args <- c(args, "--mapbox-access-token", mapbox_token())
  if (isTRUE(mathjax)) args <- c(args, "--mathjax", file.path(mathjax_path(), "MathJax.js"))
  
  # TODO: point to local topojson? Should this only work if plot_geo(standalone = TRUE)?
  try_library("processx", "orca")
  invisible(processx::run("orca", args, echo = TRUE, spinner = TRUE))
}

#' Orca image export server
#' 
#' Compared to [orca], [orca_serve] is more efficient at exporting many plotly 
#' graphs because the former must startup/shutdown an external process for every image. 
#' The server (background) process is launched upon initialization of a [orca_serve] class 
#' (i.e., when the `new()` method is called). The `export()` method accepts any valid plotly 
#' object as input and spits out an image file to disk. To kill the background server process, 
#' use `close()`.
#' 
#' @usage NULL
#' @format NULL
#' 
#' @section Initialization:
#' A new 'orcaServe'-object is initialized using the new() method on the generator:
#' 
#' \strong{Usage}
#' 
#' \code{
#'   orca_serve$new(
#'     port = 5151, mathjax = FALSE, safe = FALSE, request_limit = NULL, keep_alive = TRUE, 
#'     window_max_number = NULL, quiet = FALSE, debug = FALSE, ...
#'   )
#'  }
#' 
#' \strong{Arguments}
#' 
#' \describe{
#'   \item{\code{port}}{Sets the server's port number.}
#'   \item{\code{mathjax}}{
#'     whether or not to include MathJax (required to render [TeX]).
#'     If `TRUE`, the PLOTLY_MATHJAX_PATH environment variable must be set and point 
#'     to the location of MathJax (this variable is also used to render [TeX] in 
#'     interactive graphs, see [config]).
#'   }
#'   \item{\code{safe}}{
#'     Turns on safe mode: where figures likely to make browser window hang during image generating are skipped.
#'   }
#'   \item{\code{request_limit}}{
#'     Sets a request limit that makes orca exit when reached.
#'   }
#'   \item{\code{keep_alive}}{
#'     Turn on keep alive mode where orca will (try to) relaunch server if process unexpectedly exits.
#'   }
#'   \item{\code{window_max_number}}{
#'     Sets maximum number of browser windows the server can keep open at a given time.
#'   }
#'   \item{\code{quiet}}{Suppress all logging info.}
#'   \item{\code{debug}}{Starts app in debug mode.}
#'   \item{\code{xvfb}}{Whether to run orca via X virtual framebuffer. May be necessary in a headless environment}
#' }
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{export(p, file = "plot.png", format = tools::file_ext(file), scale = NULL, width = NULL, height = NULL)}}{
#'     Export a static image of a plotly graph. Arguments found here are the same as those found in [orca].
#'   }
#'   \item{\code{close()}}{Close down the orca server and kill the underlying node process.}
#' }
#' 
#' @section Fields:
#' 
#' \describe{
#'   \item{\code{port}}{The port number that the server is listening to.}
#'   \item{\code{process}}{An R6 class for controlling and querying the underlying node process.}
#' }
#' 
#' @export
#' @author Carson Sievert
#' @seealso [orca]
#' @examples 
#' 
#' \dontrun{
#' # launch the server
#' server <- orca_serve$new()
#' 
#' # export as many graphs as you'd like
#' server$export(qplot(1:10), "test1.pdf")
#' server$export(plot_ly(x = 1:10, y = 1:10), "test2.pdf")
#' 
#' # the underlying process is exposed as a field, so you
#' # have full control over the external process
#' server$process$is_alive()
#' 
#' # convenience method for closing down the server
#' server$close()
#' 
#' # remove the exported files from disk
#' unlink("test1.pdf")
#' unlink("test2.pdf")
#' }

orca_serve <- R6::R6Class(
  "orcaServe",
  public = list(
    process = NULL,
    port = NULL,
    initialize = function(port = 5151, mathjax = FALSE, safe = FALSE, request_limit = NULL,
                          keep_alive = TRUE, window_max_number = NULL, quiet = FALSE, 
                          debug = FALSE, xvfb = FALSE, ...) {
      
      # make sure we have the required infrastructure
      orca_available()
      try_library("processx", "orca_serve")
      
      # use main bundle since any plot can be thrown at the server
      plotlyjs <- plotlyMainBundle()
      plotlyjs_file <- file.path(plotlyjs$src$file, plotlyjs$script)
      
      args <- c(
        "serve",
        "-p", port,
        "--plotly", plotlyjs_file,
        if (safe) "--safe-mode",
        #if (orca_version() >= "1.1.1") "--graph-only",
        if (keep_alive) "--keep-alive",
        if (debug) "--debug",
        if (quiet) "--quiet"
      )
      
      if (!is.null(request_limit))
        args <- c(args, "--request-limit", request_limit)
      
      if (!is.null(window_max_number))
        args <- c(args, "--window-max-number", window_max_number)
      
      if (!is.null(tryNULL(mapbox_token()))) 
        args <- c(args, "--mapbox-access-token", mapbox_token())
      
      if (isTRUE(mathjax)) 
        args <- c(args, "--mathjax", file.path(mathjax_path(), "MathJax.js"))
      
      if (xvfb) {
        self$process <- processx::process$new("xvfb-run", c("orca", args), ...)
      } else {
        self$process <- processx::process$new("orca", args, ...)
      }
      
      self$port <- port
    },
    export = function(p, file = "plot.png", format = tools::file_ext(file), scale = NULL, width = NULL, height = NULL) {
      
      # request/response model works similarly to plotly_IMAGE()
      bod <- list(
        figure = plotly_build(p)$x[c("data", "layout")],
        format = format,
        width = width,
        height = height,
        scale = scale
      )
      res <- httr::POST(
        paste0("http://localhost:", self$port), 
        body = to_JSON(bod)
      )
      httr::stop_for_status(res)
      httr::warn_for_status(res)
      con <- httr::content(res, as = "raw")
      writeBin(con, file)
    },
    close = function() {
      self$process$kill()
    }
  )
)

orca_available <- function() {
  if (Sys.which("orca") == "") {
    stop(
      "The orca command-line utility is required for this functionality.\n\n",
      "Please follow the installation instructions here -- https://github.com/plotly/orca#installation",
      call. = FALSE
    )
  }
  
  TRUE
}

orca_version <- function() {
  orca_available()
  as.package_version(system("orca --version", intern = TRUE))
}
