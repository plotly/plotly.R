sharingButton <- function() {
  url <- 'https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html'
  list(
    name = "Collaborate",
    icon = list(
      width = 1000,
      ascent = 850,
      descent = -150,
      path = sharingPath
    ),
    click = htmlwidgets::JS(sprintf(
      "function(gd) { 
        // is this being viewed in RStudio?
        if (location.search == '?viewer_pane=1') {
          alert('To learn about sharing options, visit:\\n %s');
        } else {
          window.open('%s', '_blank');
        }
      }", url, url))
  )
}

sharingPath <- 'm214-7h429v214h-429v-214z m500 0h72v500q0 8-6 21t-11 20l-157 156q-5 6-19 12t-22 5v-232q0-22-15-38t-38-16h-322q-22 0-37 16t-16 38v232h-72v-714h72v232q0 22 16 38t37 16h465q22 0 38-16t15-38v-232z m-214 518v178q0 8-5 13t-13 5h-107q-7 0-13-5t-5-13v-178q0-8 5-13t13-5h107q7 0 13 5t5 13z m357-18v-518q0-22-15-38t-38-16h-750q-23 0-38 16t-16 38v750q0 22 16 38t38 16h517q23 0 50-12t42-26l156-157q16-15 27-42t11-49z'
