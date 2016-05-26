library(httr)
x <- GET('https://api.github.com/repos/plotly/plotly.js/releases/latest')
zip <- content(x)$zipball_url
tmp <- tempfile(fileext = ".zip")
download.file(zip, tmp)
unzip(tmp)
# save trace types (mostly for error checking on the R side)
traces <- basename(Sys.glob("plotly-plotly.js*/src/traces/*"))
devtools::use_data(traces, overwrite = TRUE, internal = TRUE)
# TODO: figure out the _required_ properties of each trace type, store them 
# as internal R data, and use them to provide error checking

# update the plotly.js bundle
p <- Sys.glob("plotly-plotly.js*/dist/plotly.min.js")
file.copy(p, "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js", overwrite = T)
l <- Sys.glob("plotly-plotly.js*/LICENSE")
file.copy(l, "inst/htmlwidgets/lib/plotlyjs/LICENSE", overwrite = T)
unlink("plotly-plotly.js*", recursive = T)
message("Manually update plotly.yaml with this version")
basename(zip)
