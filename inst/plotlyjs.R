library(httr)
# download latest GitHub release
# for a particular version: `zip <- "https://github.com/plotly/plotly.js/archive/v1.33.1.zip"`
x <- GET('https://api.github.com/repos/plotly/plotly.js/releases/latest')
zip <- content(x)$zipball_url
tmp <- tempfile(fileext = ".zip")
download.file(zip, tmp)
unzip(tmp)

# update the default bundle
file.copy(
  Sys.glob("*plotly.js*/dist/plotly.min.js"), 
  "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js", 
  overwrite = TRUE
)
# update the plotly.js LICENSE
file.copy(
  Sys.glob("*plotly.js*/LICENSE"), 
  "inst/htmlwidgets/lib/plotlyjs/LICENSE", 
  overwrite = TRUE
)
# update the locale files
locales <- Sys.glob("*plotly.js*/dist/plotly-locale-*.js")
file.copy(
  locales,
  file.path("inst/htmlwidgets/lib/plotlyjs/locales", sub("^plotly-locale-", "", basename(locales))),
  overwrite = TRUE
)
# update typed array polyfill
download.file(
  "https://raw.githubusercontent.com/plotly/plotly.js/master/dist/extras/typedarray.min.js",
  "inst/htmlwidgets/lib/typedarray/typedarray.min.js"
)

# update the plot schema
Schema <- jsonlite::fromJSON(Sys.glob("*plotly.js*/dist/plot-schema.json"))
usethis::use_data(Schema, overwrite = T, internal = T)

# clean-up and bump HTML dependency version
unlink("*plotly.js*", recursive = T)
message("Manually update plotly.R with this version")




# download latest build from master 
#download.file(
#  "https://raw.githubusercontent.com/plotly/plotly.js/master/dist/plotly.min.js", 
#  destfile = "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js"
#)
