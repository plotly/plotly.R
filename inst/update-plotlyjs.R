library(httr)
x <- GET('https://api.github.com/repos/plotly/plotly.js/releases/latest')
zip <- content(x)$zipball_url
tmp <- tempfile(fileext = ".zip")
download.file(zip, tmp)
unzip(tmp)
p <- Sys.glob("plotly-plotly.js*/dist/plotly.min.js")
file.copy(p, "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js", overwrite = T)
g <- Sys.glob("plotly-plotly.js*/dist/plotly-geo-assets.js")
file.copy(g, "inst/htmlwidgets/lib/plotlyjs/plotly-geo-assets.js", overwrite = T)
unlink("plotly-plotly.js*", recursive = T)
# update plotly.yml with version
basename(zip)
