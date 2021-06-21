library(httr)
# download latest GitHub release
# for a particular version: `zip <- "https://github.com/plotly/plotly.js/archive/v1.33.1.zip"`
x <- httr::RETRY(
  verb = "GET",
  url = 'https://api.github.com/repos/plotly/plotly.js/releases/latest',
  times = 5,
  terminate_on = c(400, 401, 403, 404),
  terminate_on_success = TRUE
)
zip <- content(x)$zipball_url
tmp <- tempfile(fileext = ".zip")
download.file(zip, tmp)
unzip(tmp)

# TODO: patch source to add back in phantomjs support
# https://github.com/plotly/plotly.js/pull/5380/files#diff-459a89970d7ac2d1323f78c770d768ecbbfb9f698b0d1e25e8010e0eb73f56edR12-R14

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
# plotly.js used to bundle a typedarray polyfill to support older browsers,
# but v2 drops support for them, so it no longer includes this polyfill
# Hopefully, we can continue to get away with using the pre-v2 polyfill
# to support shinytest/phantomjs
#download.file(
#  "https://raw.githubusercontent.com/plotly/plotly.js/master/dist/extras/typedarray.min.js",
#  "inst/htmlwidgets/lib/typedarray/typedarray.min.js"
#)

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
