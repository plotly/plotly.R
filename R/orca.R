#' Static image exporting via orca
#' 
#' This function is deprecated, use [save_image()] instead.
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
#' @param more_args additional arguments to pass along to system command. This is useful
#' for specifying display and/or electron options, such as `--enable-webgl` or `--disable-gpu`.
#' @param ... for `orca()`, additional arguments passed along to `processx::run`. For 
#' `orca_serve()`, additional arguments passed along to `processx::process`.
#' @export
#' @author Carson Sievert
#' @md
#' @rdname orca
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#' # NOTE: in a headless environment, you may need to set `more_args="--enable-webgl"`
#' # to export webgl correctly
#' p <- plot_ly(z = ~volcano) %>% add_surface()
#' orca(p, "surface-plot.svg")
#' 
#' #' # launch the server
#' server <- orca_serve()
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
#' 

orca <- function(p, file = "plot.png", format = tools::file_ext(file), 
                 scale = NULL, width = NULL, height = NULL, mathjax = FALSE,
                 parallel_limit = NULL, verbose = FALSE, debug = FALSE, 
                 safe = FALSE, more_args = NULL, ...) {
  
  .Deprecated("save_image")
  
  orca_available()
  
  b <- plotly_build(p)
  
  # find the relevant plotly.js bundle
  plotlyjs_path <- plotlyMainBundlePath()
  
  tmp <- tempfile(fileext = ".json")
  cat(to_JSON(b$x[c("data", "layout")]), file = tmp)
  
  args <- c(
    "graph", tmp, 
    "-o", file,
    "--format", format,
    "--plotlyjs", plotlyjs_path,
    if (debug) "--debug",
    if (verbose) "--verbose",
    if (safe) "--safe-mode",
    more_args
  )
  
  if (!is.null(scale)) args <- c(args, "--scale", scale)
  if (!is.null(width)) args <- c(args, "--width", width)
  if (!is.null(height)) args <- c(args, "--height", height)
  if (!is.null(parallel_limit)) args <- c(args, "--parallel-limit", parallel_limit)
  if (!is.null(tryNULL(mapbox_token()))) args <- c(args, "--mapbox-access-token", mapbox_token())
  if (isTRUE(mathjax)) args <- c(args, "--mathjax", file.path(mathjax_path(), "MathJax.js"))
  
  # TODO: point to local topojson? Should this only work if plot_geo(standalone = TRUE)?
  try_library("processx", "orca")
  invisible(processx::run("orca", args, echo = TRUE, spinner = TRUE, ...))
}

#' Orca image export server
#' 
#' @inheritParams orca
#' @param port Sets the server's port number.
#' @param keep_alive Turn on keep alive mode where orca will (try to) relaunch server if process unexpectedly exits.
#' @param window_max_number Sets maximum number of browser windows the server can keep open at a given time.
#' @param request_limit Sets a request limit that makes orca exit when reached.
#' @param quiet Suppress all logging info.
#' 
#' @section Methods:
#' 
#' The `orca_serve()` function returns an object with two methods:
#' 
#' \describe{
#'   \item{\code{export(p, file = "plot.png", format = tools::file_ext(file), scale = NULL, width = NULL, height = NULL)}}{
#'     Export a static image of a plotly graph. Arguments found here are the same as those found in `orca()`
#'   }
#'   \item{\code{close()}}{Close down the orca server and kill the underlying node process.}
#' }
#' 
#' @section Fields:
#' 
#' The `orca_serve()` function returns an object with two fields:
#' 
#' \describe{
#'   \item{\code{port}}{The port number that the server is listening to.}
#'   \item{\code{process}}{An R6 class for controlling and querying the underlying node process.}
#' }
#' 
#' @export
#' @rdname orca

orca_serve <- function(port = 5151, mathjax = FALSE, safe = FALSE, request_limit = NULL,
                       keep_alive = TRUE, window_max_number = NULL, quiet = FALSE, 
                       debug = FALSE, more_args = NULL, ...) {
  
  .Deprecated("kaleido")
  
  # make sure we have the required infrastructure
  orca_available()
  try_library("processx", "orca_serve")
  
  # use main bundle since any plot can be thrown at the server
  plotlyjs_path <- plotlyMainBundlePath()
  
  args <- c(
    "serve",
    "-p", port,
    "--plotly", plotlyjs_path,
    if (safe) "--safe-mode",
    if (orca_version() >= "1.1.1") "--graph-only",
    if (keep_alive) "--keep-alive",
    if (debug) "--debug",
    if (quiet) "--quiet",
    more_args
  )
  
  if (!is.null(request_limit))
    args <- c(args, "--request-limit", request_limit)
  
  if (!is.null(window_max_number))
    args <- c(args, "--window-max-number", window_max_number)
  
  if (!is.null(tryNULL(mapbox_token()))) 
    args <- c(args, "--mapbox-access-token", mapbox_token())
  
  if (isTRUE(mathjax)) 
    args <- c(args, "--mathjax", file.path(mathjax_path(), "MathJax.js"))
  
  process <- processx::process$new("orca", args, ...)
  
  list(
    port = port,
    process = process,
    close = function() process$kill(),
    export = function(p, file = "plot.png", format = tools::file_ext(file), scale = NULL, width = NULL, height = NULL) {
      # request/response model works similarly to plotly_IMAGE()
      bod <- list(
        figure = plotly_build(p)$x[c("data", "layout")],
        format = format,
        width = width,
        height = height,
        scale = scale
      )
      res <- httr::RETRY(
        verb = "POST",
        url = paste0("http://127.0.0.1:", port),
        body = to_JSON(bod),
        times = 5,
        terminate_on = c(400, 401, 403, 404),
        terminate_on_success = TRUE
      )
      httr::stop_for_status(res)
      httr::warn_for_status(res)
      con <- httr::content(res, as = "raw")
      writeBin(con, file)
    }
  )
}

correct_orca <- function() {
  orca_help <- processx::run("orca", "-h")
  grepl("plotly", orca_help[["stdout"]], ignore.case = TRUE)
}


orca_available <- function() {
  if (Sys.which("orca") == "" || !correct_orca()) {
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
  # default to initial release if we can't correctly parse version
  tryCatch(
    as.package_version(system("orca --version", intern = TRUE)), 
    error = function(e) "1.0.0"
  )
}
