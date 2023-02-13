#' Save plot as a static image
#' 
#' Static image exporting via [the kaleido python
#' package](https://github.com/plotly/Kaleido/). `kaleido()` imports
#' kaleido into a \pkg{reticulate}d Python session and returns a `$transform()`
#' method for converting R plots into static images. `save_image()` provides a convenience wrapper around `kaleido()$transform()`. 
#' 
#' @section Installation:
#' 
#' `kaleido()` requires [the kaleido python
#' package](https://github.com/plotly/Kaleido/) to be usable via the \pkg{reticulate} package. Here is a recommended way to do the installation:
#' 
#' ```
#' install.packages('reticulate')
#' reticulate::install_miniconda()
#' reticulate::conda_install('r-reticulate', 'python-kaleido')
#' reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
#' reticulate::use_miniconda('r-reticulate')
#' ```
#' 
#' @param ... not currently used.
#' @param p a plot object.
#' @param file a file path with a suitable file extension (png, jpg, jpeg,
#'   webp, svg, or pdf).
#' @param width,height The width/height of the exported image in layout
#'   pixels. If `scale` is 1, this will also be the width/height of the exported
#'   image in physical pixels.
#' @param scale The scale factor to use when exporting
#'   the figure. A scale factor larger than 1.0 will increase the image
#'   resolution with respect to the figure's layout pixel dimensions. Whereas as
#'   scale factor of less than 1.0 will decrease the image resolution.
#' @export
#' @return For `save_image()`, the generated `file`. For `kaleido()`, an environment that contains:
#'   * `transform()`: a function to convert plots objects into static images. This function has the same signature (i.e., arguments) as `save_image()`
#'   * `shutdown()`: a function for shutting down any currently running subprocesses 
#'     that were launched via `transform()`
#'   * `scope`: a reference to the underlying `kaleido.scopes.plotly.PlotlyScope`
#'     python object. Modify this object to customize the underlying Chromium
#'     subprocess and/or configure other details such as URL to plotly.js, MathJax, etc. 
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#'   # Save a single image
#'   p <- plot_ly(x = 1:10)
#'   tmp <- tempfile(fileext = ".png")
#'   save_image(p, tmp)
#'   file.show(tmp)
#' 
#'   # Efficiently save multiple images
#'   scope <- kaleido()
#'   for (i in 1:5) {
#'     scope$transform(p, tmp)
#'   }
#'   # Remove and garbage collect to remove 
#'   # R/Python objects and shutdown subprocesses
#'   rm(scope); gc()
#' }
#' 
save_image <- function(p, file, ..., width = NULL, height = NULL, scale = NULL) {
  kaleido()$transform(
    p, file, ..., width = width, height = height, scale = scale
  )
}

#' @rdname save_image
#' @export
kaleido <- function(...) {
  if (!rlang::is_installed("reticulate")) {
    stop("`kaleido()` requires the reticulate package.")
  }
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("`kaleido()` requires `reticulate::py_available()`  to be `TRUE`. Do you need to install python?")
  }
  
  py <- reticulate::py
  scope_name <- paste0("scope_", new_id())
  py[[scope_name]] <- reticulate::import("kaleido")$scopes$plotly$PlotlyScope(
    plotlyjs = plotlyMainBundlePath()
  )
  
  scope <- py[[scope_name]]
  
  mapbox <- Sys.getenv("MAPBOX_TOKEN", NA)
  if (!is.na(mapbox)) {
    scope$mapbox_access_token <- mapbox
  }
  
  res <- list2env(list(
    scope = scope,
    # https://github.com/plotly/Kaleido/blob/6a46ecae/repos/kaleido/py/kaleido/scopes/plotly.py#L78-L106
    transform = function(p, file = "figure.png", ..., width = NULL, height = NULL, scale = NULL) {
      # Perform JSON conversion exactly how the R package would do it
      # (this is essentially plotly_json(), without the additional unneeded info)
      # and attach as an attribute on the python scope object
      scope[["_last_plot"]] <- to_JSON(
        plotly_build(p)$x[c("data", "layout", "config")]
      )
      # On the python side, _last_plot is a string, so use json.loads() to 
      # convert to dict(). This should be fine since json is a dependency of the
      # BaseScope() https://github.com/plotly/Kaleido/blob/586be5/repos/kaleido/py/kaleido/scopes/base.py#L2
      transform_cmd <- sprintf(
        "%s.transform(sys.modules['json'].loads(%s._last_plot), format='%s', width=%s, height=%s, scale=%s)",
        scope_name, scope_name, tools::file_ext(file), 
        reticulate::r_to_py(width), reticulate::r_to_py(height), 
        reticulate::r_to_py(scale)
      )
      # Write the base64 encoded string that transform() returns to disk
      # https://github.com/plotly/Kaleido/blame/master/README.md#L52
      reticulate::py_run_string(
        sprintf("import sys; open('%s', 'wb').write(%s)", file, transform_cmd)
      )
      
      invisible(file)
    },
    # Shutdown the kaleido subprocesses
    # https://github.com/plotly/Kaleido/blob/586be5c/repos/kaleido/py/kaleido/scopes/base.py#L71-L72
    shutdown = function() {
      reticulate::py_run_string(paste0(scope_name, ".__del__()"))
    }
  ))
  
  # Shutdown subprocesses and delete python scope when 
  # this object is garbage collected by R
  reg.finalizer(res, onexit = TRUE, function(x) {
    x$shutdown()
    reticulate::py_run_string(paste("del", scope_name))
  })
  
  class(res) <- "kaleidoScope"
  res
}


#' Print method for kaleido
#' 
#' S3 method for [kaleido()].
#' 
#' @param x a [kaleido()] object.
#' @param ... currently unused.
#' @export
#' @importFrom utils capture.output
#' @keywords internal
print.kaleidoScope <- function(x, ...) {
  args <- formals(x$transform)
  cat("$transform: function(", paste(names(args), collapse = ", "), ")\n", sep = "")
  cat("$shutdown: function()\n")
  cat("$scope: ", utils::capture.output(x$scope))
}
