# requires my fork
# $ nohup R CMD BATCH ./build_site.R & 

# Make sure this script is being run in the right place
repo_head <- git2r::head(git2r::repository())
is_gh_pgs <- identical(repo_head@name, "gh-pages") &&
  grepl("plotly", repo_head@repo@path)
if (!is_gh_pgs) stop("Must be run on the 'gh-pages' branch of ropensci/plotly.")

# download a fresh version of tidyverse/ggplot2@master
unlink("ggplot2-master", recursive = TRUE)
curl::curl_download(
  "https://github.com/tidyverse/ggplot2/archive/master.zip",
  "ggplot2-master.zip"
)
unzip("ggplot2-master.zip")

# install tidyverse/ggplot2@master
devtools::install("ggplot2-master")

# override print.ggplot with our own custom function that sends ggplot objects
# to plotly's cloud service
printly <- function(x) {
  stop("wtf")
  if (!ggplot2::is.ggplot(x)) return(x)
  u <- plotly::api_create(x, filename = as.character(Sys.time()))
  message("Click on the png below to view the interactive version")
  a <- sprintf(
    '<a href="%s"> <img src="%s" width="540" height="400" /> </a>', 
    u$web_url, u$image_urls$default
  )
  structure(a, class = "html")
}
assignInNamespace("print.ggplot", printly, asNamespace("ggplot2"))
# verify with `getFromNamespace("print.ggplot", asNamespace("ggplot2"))`

render_site <- function() {
  owd <- setwd("ggplot2-master")
  on.exit(setwd(owd), add = TRUE)
  # requires my fork of pkgdown `devtools::install_github('cpsievert/pkgdown')`
  pkgdown::build_site()
}
render_site()


# update the target directory, so site is available at http://ropensci.github.io/plotly/ggplot2/
unlink("ggplot2", recursive = TRUE)
file.rename("ggplot2-master/docs", "ggplot2")

# clean-up
unlink("ggplot2-master")


# this is essentially what staticdocs is doing
#x <- "p <- ggplot(mtcars, aes(wt, mpg)); p + geom_point()"
#y <- evaluate::evaluate(x, envir = new.env(parent = globalenv()), new_device = TRUE)
#str(y)


