context("means and error bars")

library(ggplot2)

one.line.df <-
  data.frame(
    x = c(1, 2, 3, 4), 
    y = c(2, 1, 3, 4), 
    array = c(0.1, 0.2, 0.1, 0.1), 
    arrayminus = c(0.2, 0.4, 1, 0.2))

none.json <- list(
  list(
    x = c(1, 2, 3, 4), 
    y = c(2, 1, 3, 4), 
    error_y = list(
      type = "data", 
      symmetric = FALSE, 
      array = c(0.1, 0.2, 0.1, 0.1), 
      arrayminus = c(0.2, 0.4, 1, 0.2)
    ), 
    type = "scatter",
    mode = "none"
  )
)

test_that("only asymmetric error bars", {
  error.gg <- ggplot(one.line.df, aes(x, y))+
    geom_errorbar(aes(ymin=y-arrayminus, ymax=y+array))
  generated.json <- gg2list(error.gg)
  is.trace <- names(generated.json) == ""
  traces <- generated.json[is.trace]
  expect_identical(length(traces), 1L)
  tr <- traces[[1]]
  expect_identical(tr$mode, "none")
  expect_identical(tr$type, "scatter")
  ey <- tr$error_y
  expect_identical(ey$type, "data")
  expect_identical(ey$symmetric, FALSE)
  expect_equal(ey$array, c(0.1, 0.2, 0.1, 0.1))
  expect_equal(ey$arrayminus, c(0.2, 0.4, 1, 0.2))
})

one.line.json <- list(
  list(
    x = c(1, 2, 3, 4), 
    y = c(2, 1, 3, 4), 
    error_y = list(
      type = "data", 
      symmetric = FALSE, 
      array = c(0.1, 0.2, 0.1, 0.1), 
      arrayminus = c(0.2, 0.4, 1, 0.2)
    ), 
    type = "scatter"
  )
)

test_that("asymmetric error bars, geom_errorbar last", {
  one.line.gg <- ggplot(one.line.df, aes(x, y))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=y-arrayminus, ymax=y+array))
  generated.json <- gg2list(one.line.gg)
  ## BUG: lines do not appear in plotly.
  ##generated.json[[2]]$mode <- "lines+markers"
  
  ## when there is 1 trace with error bars, lines, and markers, plotly
  ## shows error bars in the background, lines in the middle and
  ## markers in front.
  is.trace <- names(generated.json) == ""
  traces <- generated.json[is.trace]
  expect_identical(length(traces), 1L)
})

test_that("asymmetric error bars, geom_errorbar first", {
  one.line.gg <- ggplot(one.line.df, aes(x, y))+
    geom_errorbar(aes(ymin=y-arrayminus, ymax=y+array))+
    geom_line()+
    geom_point()
  generated.json <- gg2list(one.line.gg)
  is.trace <- names(generated.json) == ""
  traces <- generated.json[is.trace]
  expect_identical(length(traces), 1L)
  tr <- traces[[1]]
  expect_identical(tr$mode, "lines+markers")
  expect_identical(tr$type, "scatter")
  ey <- tr$error_y
  expect_identical(ey$type, "data")
  expect_identical(ey$symmetric, FALSE)
  expect_equal(ey$array, c(0.1, 0.2, 0.1, 0.1))
  expect_equal(ey$arrayminus, c(0.2, 0.4, 1, 0.2))
})

colors.json <- list(
  list(
    x = c(1, 2, 3, 4), 
    y = c(2, 1, 3, 4), 
    error_y = list(
      type = "data", 
      symmetric = FALSE, 
      array = c(0.1, 0.2, 0.1, 0.1), 
      arrayminus = c(0.2, 0.4, 1, 0.2),
      color="red"
    ), 
    type = "scatter",
    marker=list(color="blue", size=14),
    line=list(color="black")
  )
)

test_that("different colors for error bars, points, and lines", {
  one.line.gg <- ggplot(one.line.df, aes(x, y))+
    geom_errorbar(aes(ymin=y-arrayminus, ymax=y+array), color="red")+
    geom_line(color="black")+
    geom_point(color="blue", size=14)
  generated.json <- gg2list(one.line.gg)
  is.trace <- names(generated.json) == ""
  traces <- generated.json[is.trace]
  expect_identical(length(traces), 1L)
  ## TODO: check that colors agree in colors.json and generated.json.
})

if(FALSE){
## from https://github.com/chriddyp/ggplot2-plotly-cookbook/blob/a45f2c70b7adf484e0b0eb8810a1e59e018adbb8/means_and_error_bars.R#L162-L191
df <- ToothGrowth

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## data: a data frame.
## measurevar: the name of a column that contains the variable to be summariezed
## groupvars: a vector containing names of columns that contain grouping variables
## na.rm: a boolean that indicates whether to ignore NA's
## conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N = length2(xx[[col]], na.rm=na.rm),
                     mean = mean (xx[[col]], na.rm=na.rm),
                     sd = sd (xx[[col]], na.rm=na.rm)
                     )
                 },
                 measurevar
                 )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean   
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
dfc <- summarySE(df, measurevar="len", groupvars=c("supp","dose"))
bad <- ggplot(dfc, aes(x=dose, y=len, colour=supp)) +
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_line() +
  geom_point()

good <- ggplot(dfc, aes(x=dose, y=len, colour=supp)) +
  geom_line() +
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_point()
good.json <- gg2list(good)
py$ggplotly(good, kwargs=list(fileopt='overwrite', filename='R-Cookbook/means-and-error-bars/basic-error-bars'))

## The conversion from geom_errorbar to plotly error bars is not
## straightforward.

## we need to make a plotly trace with list(error_y=list(array=c(1, 2,
## 3), symmetric=FALSE, arrayminus=c(4.45, 3.91, 2.65))) -- and maybe
## traceref=2 and tracerefminus=1, which are indices of other traces
## that contain the same error bar data in the master trace list.

## https://plot.ly/~tdhock/184 has some plotly error bars.

good.json[[1]]$error_y <-
  list(array=c(1, 2, 3), arrayminus=c(4, 5, 6),
       symmetric=FALSE,
       traceref=1, tracerefminus=1,
       visible=TRUE,
       type="data")

}
