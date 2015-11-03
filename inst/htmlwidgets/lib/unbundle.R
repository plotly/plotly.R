# need creds to get source right now (so manually download by clicking "raw")
src <- "~/Downloads/plotly_20150819a_advanced.zip"
target <- "inst/htmlwidgets/lib/"
unzip(src, exdir = target)

d <- file.path(
  target,
  sub("\\.zip$", "", basename(src))
)

plotlyjs <- file.path(target, "plotlyjs")
dir.create(plotlyjs, recursive = TRUE)
file.copy(
  file.path(d, "plotly.min.js"),
  file.path(plotlyjs, "plotly.min.js"),
  overwrite = TRUE
)

jquery <- file.path(target, "jquery")
dir.create(jquery)
file.copy(
  file.path(d, "dependencies", "jquery-latest.js"),
  file.path(jquery, "jquery.js"),
  overwrite = TRUE
)

typedArray <- file.path(target, "typedArray")
dir.create(typedArray)
file.copy(
  file.path(d, "dependencies", "typedarray.js"),
  file.path(typedArray, "typedArray.js"),
  overwrite = TRUE
)

d3 <- file.path(target, "d3")
dir.create(d3)
file.copy(
  file.path(d, "dependencies", "d3.v3.min.js"),
  file.path(d3, "d3.v3.min.js"),
  overwrite = TRUE
)

# cleanup
unlink(d, recursive = TRUE)
