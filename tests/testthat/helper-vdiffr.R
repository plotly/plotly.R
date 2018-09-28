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

# start up the image server and inform vdiffr how to write plotly svg
if (enable_vdiffr) {
  # generate random (port) number between 3000 & 8000
  port <- floor(runif(1, 3001, 8000))
  
  # check to see if this port is usable
  # TODO: this is how shiny does -- is there a lighter-weight solution?
  tmp <- try(httpuv::startServer("0.0.0.0", port, list()), silent = TRUE)
  if (inherits(tmp, 'try-error')) {
    stop(
      "Tried to open orca server on port '", port, "', but it's not available. ", 
      "Try running `test()` again"
    )
  } else {
    httpuv::stopServer(tmp)
  }
  
  # start up the node process
  orcaImageServer <- orca_serve$new(port)
  
  strextract <- function(str, pattern) {
    regmatches(str, regexpr(pattern, str))
  }
  
  # define logic for writing svg in vdiffr
  assignInNamespace(
    "write_svg.plotly",
    function(p, file, title, user_fonts = NULL) {
      
      # before image export, specify trace[i].uid so it's deterministic in the resulting svg
      # https://github.com/plotly/orca/issues/133
      p <- plotly::plotly_build(p)
      uid_data <- paste0("-vdiffr-plotly-", seq_along(p$x$data))
      p$x$data <- Map(function(tr, id) { tr$uid <- id; tr }, p$x$data, uid_data)
      
      # write svg to disk
      withr::with_dir(
        dirname(file), orcaImageServer$export(p, basename(file))
      )
      
      # strip out non-deterministic fullLayout.uid
      # TODO: if and when plotly provides an API to pre-specify, use it!
      svg_txt <- readLines(file)
      def <- strextract(svg_txt, 'defs id=\\"defs-[[:alnum:]]+\\"')
      uid <- sub("defs-", "", strextract(def, "defs-[[:alnum:]]+"))
      svg_txt <- gsub(uid, "", svg_txt, fixed = TRUE)
      writeLines(svg_txt, file)
    },
    asNamespace("vdiffr")
  )
}


expect_doppelganger <- function(p, name, ...) {
  
  if (enable_vdiffr) {
    if (ggplot2::is.ggplot(p)) p <- ggplotly(p)
    ignore <- vdiffr::expect_doppelganger(name, p, ...)
  }
  
  # return 'built' data/layout
  plotly_build(p)$x[c("data", "layout")]
}
