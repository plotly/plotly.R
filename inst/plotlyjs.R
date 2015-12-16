download.file(
  "http://cdn.plot.ly/plotly-latest.min.js",
  "inst/htmlwidgets/lib/plotlyjs/plotly-latest.min.js"
)

download.file(
  "https://raw.githubusercontent.com/plotly/plotly.js/master/LICENSE",
  "inst/htmlwidgets/lib/plotlyjs/LICENSE"
)


# find plotlyjs dependencies and write yaml file
library(jsonlite)
pkg <- fromJSON("https://raw.githubusercontent.com/plotly/plotly.js/master/package.json")
deps <- pkg$dependencies
nms <- c(pkg$name, names(deps))
deps <- c(pkg$version, sub("^\\^", "", as.character(deps)))

f <- function(x, y) {
  list(name = x, 
       version = y,
       src = "htmlwidgets/lib/plotlyjs",
       script = "plotly-latest.min.js"
  )
}



depz <- list(dependencies = Map(f, nms, deps))
writeLines(yaml::as.yaml(depz), "inst/htmlwidgets/plotly.yaml")

  
  
