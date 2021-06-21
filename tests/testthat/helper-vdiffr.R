visual_testing <- grepl("true", Sys.getenv("VISUAL_TESTS"), fixed = TRUE)

message("Visual testing is ", if (!visual_testing) "not ", "enabled.")

# start up the orca image server
if (visual_testing) {
  # try 20 random ports
  for (vdiff_port_tries in 1:20) {
    port <- floor(runif(1, 3001, 8000))
    success <- tryFALSE({
      # init image server with webgl enabled
      # maybe someday this won't be necessary 
      # https://github.com/plotly/orca/issues/127
      orcaServer <- orca_serve(port = port, more_args = "--enable-webgl")
      orcaServer$process$is_alive() && is.null(orcaServer$process$get_exit_status())
    })
    if (success) break
  }
} 

expect_doppelganger <- function(p, name, ...) {
  
  if (!visual_testing) {
    return(invisible(NULL))
  }
  
  testthat::local_edition(3)
  
  # some plots have random characteristics, so make sure we always have the same seed,
  # otherwise comparing svg produces false positives
  set.seed(555)
  
  name <- str_standardise(name)
  file <- paste0(name, ".svg")
  path <- tempfile(file, fileext = ".svg")
  write_plotly_svg(p, path)
  testthat::expect_snapshot_file(path = path, name = file, cran = FALSE)
}

# run visual test and return 'built' data/layout
expect_doppelganger_built <- function(p, name, ...) {
  expect_doppelganger(p, name, ...)
  plotly_build(p)$x[c("data", "layout")]
}


# define logic for writing svg
write_plotly_svg <- function(p, file) {
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

# copied from vdiffr
str_standardise <- function(s, sep = "-") {
  stopifnot(rlang::is_scalar_character(s))
  s <- gsub("[^a-z0-9]", sep, tolower(s))
  s <- gsub(paste0(sep, sep, "+"), sep, s)
  s <- gsub(paste0("^", sep, "|", sep, "$"), "", s)
  s
}
