# requires my fork
# devtools::install_github("cpsievert/staticdocs") 
# $ nohup R CMD BATCH ./build_site.R & 

library(grid)
library(ggplot2)
printly <- function(x) {
  if (ggplot2::is.ggplot(x)) {
    u <- plotly::plotly_POST(x, filename = Sys.time())
    id <- sub("/", ":", strsplit(u$url, "~")[[1]][2])
    src <- sprintf("https://api.plot.ly/v2/files/%s/image?image_name=default", id)
    a <- sprintf(
      '<a href="%s"> <img src="%s" width="540" height="400" /> </a>', 
      u$url, src
    )
    structure(a, class = "html")
  } else {
    x
  }
}

assignInNamespace("print.ggplot", printly, asNamespace("ggplot2"))

# WARNING: this takes close to an hour and was giving me memory leaks in RStudio
# (I ended up running this on my remote machine to avoid memory issues)
staticdocs::build_site("../ggplot2")

# push changes to gh-pages
# system("git checkout gh-pages")
# system("git checkout master -- inst/web")
# system("cp -r inst/web/* ggplot2")


# this is essentially what staticdocs is doing
#x <- "p <- ggplot(mtcars, aes(wt, mpg)); p + geom_point()"
#y <- evaluate::evaluate(x, envir = new.env(parent = globalenv()), new_device = TRUE)
#str(y)


# old non-local version
# printly <- function(x) {
#   if (ggplot2::is.ggplot(x)) {
#     p <- plotly:::plotly_iframe(plotly::plotly_POST(x)$url)
#     structure(p, class = "html")
#   } else {
#     x
#   }
# }
# assignInNamespace("print.ggplot", printly, asNamespace("ggplot2"))
# # should be printly
# # getS3method("print", "ggplot")
