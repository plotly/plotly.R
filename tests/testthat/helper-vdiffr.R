# If VDIFFR is TRUE, enable visual testing
enable_vdiffr <- as.logical(Sys.getenv("VDIFFR", FALSE))

# start up the image server and let vdiffr's svg writing method know about it
if (enable_vdiffr) {
  
  args <- Sys.getenv("VDIFFR_ARGS", NA)
  args <- if (is.na(args)) NULL else args
  orcaServer <- orca_serve(more_args = args)
  
  # define logic for writing svg in vdiffr
  write_svg.plotly <- function(p, file, title, user_fonts = NULL) {
    # before exporting, specify trace[i].uid so resulting svg is deterministic
    # https://github.com/plotly/orca/issues/133
    p <- plotly_build(p)
    uid_data <- paste0("-vdiffr-plotly-", seq_along(p$x$data))
    p$x$data <- Map(function(tr, id) { tr$uid <- id; tr }, p$x$data, uid_data)
    
    # write svg to disk
    # NOTE TO SELF: yes, it would be great to use `orca_serve()` here, but it gives 
    # slightly different results from `orca()` (ordering of attributes are different)
    # and `orca_serve()` doesn't seem to run reliably everywhere
    owd <- setwd(dirname(file))
    on.exit(setwd(owd))
    # NOTE: the dimensions here should match the server args part of xvfb-run
    orcaServer$export(p, file = basename(file), width = 640, height = 480)
    
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
  options(shiny.launch.browser = interactive())
  
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
