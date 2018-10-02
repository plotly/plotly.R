# default is equal to whether NOT_CRAN is true or not
enable_vdiffr <- identical(Sys.getenv("NOT_CRAN"), "true")

# disable or enable vdiffr based on the state of USE_VDIFFR, if set
if (identical(Sys.getenv("USE_VDIFFR"), "true")) {
  enable_vdiffr <- TRUE
} else if (identical(Sys.getenv("USE_VDIFFR"), "false")) {
  enable_vdiffr <- FALSE
}

# disable vdiffr if version is old
if (!requireNamespace("vdiffr", quietly = TRUE) ||
    utils::packageVersion("vdiffr") < "0.2.3.9001") {
  enable_vdiffr <- FALSE
}

# start up the image server and let vdiffr's svg writing method know about it
if (enable_vdiffr) {
  # generate random (port) number between 3000 & 8000
  port <- floor(runif(1, 3001, 8000))
  # make sure orca cli is available
  orca_available()
  # try and start up the node process
  orcaImageServer <- try(orca_serve(port, xvfb = as.logical(Sys.getenv("USE_XVFB", FALSE))), silent = TRUE)
  if (inherits(orcaImageServer, 'try-error')) {
    stop(
      "Tried to open orca server on port '", port, "', but it's not available. ", 
      "Try (possibly restarting R) and running `test()` again"
    )
  }
  
  # define logic for writing svg in vdiffr
  write_svg.plotly <- function(p, file, title, user_fonts = NULL) {
    # before exporting, specify trace[i].uid so resulting svg is deterministic
    # https://github.com/plotly/orca/issues/133
    p <- plotly::plotly_build(p)
    uid_data <- paste0("-vdiffr-plotly-", seq_along(p$x$data))
    p$x$data <- Map(function(tr, id) { tr$uid <- id; tr }, p$x$data, uid_data)
    
    # write svg to disk
    owd <- setwd(dirname(file))
    on.exit(setwd(owd))
    orcaImageServer$export(p, basename(file))
    
    # strip out non-deterministic fullLayout.uid
    # TODO: if and when plotly provides an API to pre-specify, use it!
    svg_txt <- readLines(file, warn = FALSE)
    strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
    def <- strextract(svg_txt, 'defs id=\\"defs-[[:alnum:]]+\\"')
    uid <- sub("defs-", "", strextract(def, "defs-[[:alnum:]]+"))
    svg_txt <- gsub(uid, "", svg_txt, fixed = TRUE)
    writeLines(svg_txt, file)
  }
  
  # force the vdiffr shiny app to open in a real browser 
  # (some svg files seem to not render properly in RStudio)
  options(shiny.launch.browser = TRUE)
  
  message("Visual testing is enabled.")
} else {
  
  message("Visual testing is not enabled.")
  
}


expect_doppelganger <- function(p, name, ...) {
  
  if (enable_vdiffr) {
    # some plots have random characteristics, so make sure we always have the same seed,
    # otherwise comparing svg produces false positives
    set.seed(555)
    if (ggplot2::is.ggplot(p)) p <- ggplotly(p)
    vdiffr::expect_doppelganger(name, p, ...)
  } else {
    invisible(NULL)
  }
  
}

# run visual test and return 'built' data/layout
expect_doppelganger_built <- function(p, name, ...) {
  expect_doppelganger(p, name, ...)
  plotly_build(p)$x[c("data", "layout")]
}
