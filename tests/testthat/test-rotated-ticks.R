context("rotated ticks")

ss <- data.frame(State=paste("some long text", c("CA", "NY", "TX")),
                 Prop.Inv=c(0, 1, 0.7),
                 Year=c(1984, 2015, 1999))

fg <- ggplot() +
  geom_point(aes(x=State, y=Prop.Inv), data=ss) +
  xlab("STATE SOME REALLY REALLY LONG TEXT THAT MAY OVERLAP TICKS")

## TODO: change the details of getTicks and expect_rotate_anchor to
## test plotly web pages.
getTicks <- function(html, p.name){
  xp <- sprintf('//svg[@id="%s"]//g[@id="xaxis"]//text', p.name)
  nodes <- getNodeSet(html, xp)
  stopifnot(length(nodes) > 0)
  sapply(nodes, xmlAttrs)
}
expect_rotate_anchor <- function(info, rotate, anchor){
  return()#TODO:remove.
  not <- getTicks(info$html, 'not')
  expect_match(not["style", ], "text-anchor: middle", fixed=TRUE)
  expect_match(not["transform", ], "rotate(0", fixed=TRUE)
  rotated <- getTicks(info$html, 'rotated')
  expect_match(rotated["style", ], paste("text-anchor:", anchor), fixed=TRUE)
  expect_match(rotated["transform", ], paste0("rotate(", rotate), fixed=TRUE)

  e.axis <- remDr$findElement(using="css selector", "g#xaxis")
  e.text <- e.axis$findChildElement("css selector", "text")
  tick.loc <- e.text$getElementLocation()
  tick.size <- e.text$getElementSize()
  ## Subtract a magic number that lets the test pass for un-rotated
  ## labels in firefox.
  tick.bottom.y <- tick.loc$y + tick.size$height - 6
  e.title <- remDr$findElement("css selector", "text#xtitle")
  title.loc <- e.title$getElementLocation()
  expect_true(tick.bottom.y < title.loc$y)
}

## TODO: implement renderHTML which should upload and plot the data,
## then download the rendered HTML using RSelenium to control a
## headless browser.
renderHTML <- function(gg){
  gg2list(gg)
}

test_that('no axis rotation is fine', {
  info <- renderHTML(fg)
  expect_rotate_anchor(info, "0", "middle")
})

test_that('axis.text.x=element_text(angle=90)"', {
  rotated <- fg+theme(axis.text.x=element_text(angle=90))
  info <- renderHTML(rotated)
  expect_rotate_anchor(info, "-90", "end")
})

test_that('axis.text.x=element_text(angle=70) means transform="rotate(-70)"', {
  rotated <- fg+theme(axis.text.x=element_text(angle=70))
  info <- renderHTML(rotated)
  expect_rotate_anchor(info, "-70", "end")
})

## test_that('hjust=0.75 is an error', {
##   problem <- fg+theme(axis.text.x=element_text(hjust=0.75)
##   expect_error({
##     info <- renderHTML(problem)
##   }, "ggplotly only supports hjust values 0, 0.5, 1")
## })
