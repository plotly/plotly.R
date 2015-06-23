# install the new/experimental plotly R package
# devtools::install_github("ropensci/plotly@carson-dsl")

# ----------------------------------------------------------------------
# https://plot.ly/r/3d-line-plots/
# ----------------------------------------------------------------------

library(plotly)

# initiate a 100 x 3 matrix filled with zeros
m <- matrix(numeric(300), ncol = 3)

# simulate a 3D random-walk
for (i in 2:100) m[i, ] <- m[i-1, ] + rnorm(3)

# collect everything in a data-frame
df <- setNames(
  data.frame(m, seq(1, 100)), 
  c("x", "y", "z", "time")
)

# create the plotly
plot_ly(df, x = x, y = y, z = z, color = time, type = "scatter3d")

# result -> https://plot.ly/~botty/1973

# ----------------------------------------------------------------------
# https://plot.ly/r/3d-scatter-plots/
# ----------------------------------------------------------------------

library(plotly)

# variance-covariance matrix for a multivariate normal distribution
s <- matrix(c(1, .5, .5, 
              .5, 1, .5, 
              .5, .5, 1), ncol = 3)

# use the mvtnorm package to sample 200 observations
obs <- mvtnorm::rmvnorm(200, sigma = s)

# collect everything in a data-frame
df <- setNames(data.frame(obs), c("x", "y", "z"))

plot_ly(df, x = x, y = y, z = z, type = "scatter3d", mode = "markers")

# result -> https://plot.ly/~botty/1975


# ----------------------------------------------------------------------
# https://plot.ly/r/3d-surface-plots/
# ----------------------------------------------------------------------

library(plotly)
# Note that volcano is a numeric matrix that ships with R
plot_ly(z = volcano, type = "surface")
# result -> https://plot.ly/~botty/1979

# 2D kernel density estimation
kd <- with(geyser, MASS::kde2d(duration, waiting, n = 50))
with(kd, plot_ly(x = x, y = y, z = z, type = "surface"))
# result -> https://plot.ly/~botty/3275

# ----------------------------------------------------------------------
# https://plot.ly/r/filled-area-plots/
# ----------------------------------------------------------------------

library(plotly)
p <- plot_ly(x = c(1, 2, 3, 4), y = c(0, 2, 3, 5), fill = "tozeroy")
add_trace(p, x = c(1, 2, 3, 4), y = c(3, 5, 1, 7), fill = "tonexty")
# result -> https://plot.ly/~botty/1981

# ----------------------------------------------------------------------
# https://plot.ly/r/bar-charts/
# ----------------------------------------------------------------------

library(plotly)
p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)
p
# result -> https://plot.ly/~botty/1983

p2 <- add_trace(p, 
                x = c("giraffes", "orangutans", "monkeys"),
                y = c(12, 18, 29),
                name = "LA Zoo",
)

p2

# result -> https://plot.ly/~botty/1987

layout(p2, barmode = "stack")

# result -> https://plot.ly/~botty/1989


## customizing colors

library(dplyr)
diamonds %>% count(cut) %>%
  plot_ly(x = cut, y = n, type = "bar", marker = list(color = toRGB("black")))

# mapping a color variable
diamonds %>% count(cut, clarity) %>%
  plot_ly(x = cut, y = n, type = "bar", color = clarity)

# TODO: fix legend title
# https://plot.ly/~botty/1994


# ----------------------------------------------------------------------
# https://plot.ly/r/box-plots/
# ----------------------------------------------------------------------

library(plotly)

#' basic boxplot
plot_ly(y = rnorm(50), type = "box") %>%
  add_trace(y = rnorm(50, 1))
# result -> https://plot.ly/~botty/2000

#' adding jittered points
plot_ly(y = rnorm(50), type = "box", boxpoints = "all", jitter = 0.3,
        pointpos = -1.8)
# result -> https://plot.ly/~botty/2006


#' several box plots
data(diamonds, package = "ggplot2")
plot_ly(diamonds, y = price, color = cut, type = "box")
# result -> https://plot.ly/~botty/2004

#' grouped box plots
plot_ly(diamonds, x = cut, y = price, color = clarity, type = "box") %>%
  layout(boxmode = "group")
# result -> https://plot.ly/~botty/2012

# ----------------------------------------------------------------------
# https://plot.ly/r/bubble-charts/ 
# ----------------------------------------------------------------------

# why do we need a separate page from this?? -> https://plot.ly/r/line-and-scatter/
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", marker = list(size = depth))
# TODO: automatic scaling for marker size/opacity

# ----------------------------------------------------------------------
# https://plot.ly/r/contour-plots/
# ----------------------------------------------------------------------

#' Basic contour
library(plotly)
plot_ly(z = volcano, type = "contour")
# result -> https://plot.ly/~botty/2024

#' Advanced
x <- rnorm(200) 
y <- rnorm(200)
p1 <- plot_ly(x = x, type = "histogram")
p2 <- plot_ly(x = x, y = y, type = "histogram2dcontour")
p3 <- plot_ly(y = y, type = "histogram")
a1 <- list(domain = c(0, .85))
a2 <- list(domain = c(.85, 1))
subplot(
  layout(p1, xaxis = a1, yaxis = a2),
  layout(p2, xaxis = a1, yaxis = a1),
  layout(p3, xaxis = a2, yaxis = a1)
)
#TODO: fix this -> https://plot.ly/~botty/2038

# ----------------------------------------------------------------------
# https://plot.ly/r/error-bars/
# ----------------------------------------------------------------------

library(dplyr)
library(plotly)
data(mpg, package = "ggplot2")

p <- mpg %>% group_by(class) %>%
  summarise(mn = mean(hwy), sd = 1.96 * sd(hwy)) %>%
  arrange(desc(mn)) %>% 
  plot_ly(x = class, y = mn, error_y = list(value = sd), 
          mode = "markers", name = "Highway") %>%
  layout(yaxis = list(title = "Miles Per Gallon"))
p
# result -> https://plot.ly/~botty/2070

df2 <- mpg %>% group_by(class) %>%
  summarise(mn = mean(cty), sd = 1.96 * sd(cty))

add_trace(p, y = mn, error_y = list(value = sd), 
          name = "City", data = df2)
# result -> https://plot.ly/~botty/2072

# ----------------------------------------------------------------------
# https://plot.ly/r/heatmaps/
# ----------------------------------------------------------------------

library(plotly)
plot_ly(z = volcano, type = "heatmap")
# result -> https://plot.ly/~botty/3253

#' categorical x/y axis
m <- matrix(rnorm(9), nrow = 3, ncol = 3)
plot_ly(z = m, x = c("a", "b", "c"), y = c("d", "e", "f"), type = "heatmap")
# result -> https://plot.ly/~botty/3269

#' Sequential Colorscales (Hot)
plot_ly(z = volcano, colorscale = "Hot", type = "heatmap")
# result -> https://plot.ly/~botty/3255

#' Sequential Colorscales (Greys)
plot_ly(z = volcano, colorscale = "Greys", type = "heatmap")
# result -> https://plot.ly/~botty/3257

#' Sequential Colorscales (Greens)
plot_ly(z = volcano, colorscale = "Greens", type = "heatmap")
# result -> https://plot.ly/~botty/3259

#' Custom colorscale via scales package
vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
plot_ly(z = volcano, colorscale = colz, type = "heatmap")

# ----------------------------------------------------------------------
# https://plot.ly/r/2D-Histogram/
# ----------------------------------------------------------------------

library(plotly)
s <- matrix(c(1, -.75, -.75, 1), ncol = 2)
obs <- mvtnorm::rmvnorm(500, sigma = s)
plot_ly(x = obs[,1], y = obs[,2], type = "histogram2d")
# result -> https://plot.ly/~botty/3279

# ----------------------------------------------------------------------
# https://plot.ly/r/histograms/
# ----------------------------------------------------------------------

#' Basic histogram
plot_ly(x = rnorm(50), type = "histogram")

#' Vertical histogram
plot_ly(y = rnorm(50), type = "histogram")

#' Overlayed histograms
plot_ly(x = rnorm(500), opacity = 0.6, type = "histogram") %>%
  add_trace(x = rnorm(500)) 

# ----------------------------------------------------------------------
# https://plot.ly/r/line-and-scatter/
# ----------------------------------------------------------------------

#' Simple scatterplot
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers")
# result -> https://plot.ly/~botty/3419

#' Scatterplot with qualitative colorscale
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Species, mode = "markers")
# result -> https://plot.ly/~botty/3421

#' Scatterplot with sequential colorscale
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Petal.Width, mode = "markers")
# result -> https://plot.ly/~botty/3423

#' Scatterplot with custom colorscale (TODO: how to add legend entries?)
pal <- RColorBrewer::brewer.pal(3, "Set1")
names(pal) <- levels(iris$Species)
cols <- as.character(pal[iris$Species])
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, marker = list(color = cols),
        mode = "markers")
# result -> https://plot.ly/~botty/3427

#' Basic time-series (line) plot
plot_ly(economics, x = date, y = uempmed, name = "unemployment")
# result -> https://plot.ly/~botty/3319
add_trace(y = fitted(loess(uempmed ~ as.numeric(date))))
# result -> https://plot.ly/~botty/3331

#' Density plot
dens <- with(diamonds, tapply(price, INDEX = cut, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)
plot_ly(df, x = x, y = y, color = cut)
# result -> https://plot.ly/~botty/3325


#' Different line interpolation options
x <- 1:5
y <- c(1, 3, 2, 3, 1)
plot_ly(x = x, y = y, name = "linear", line = list(shape = "linear")) %>%
  add_trace(y = y + 5, name = "spline", line = list(shape = "spline")) %>%
  add_trace(y = y + 10, name = "vhv", line = list(shape = "vhv")) %>%
  add_trace(y = y + 15, name = "hvh", line = list(shape = "hvh")) %>%
  add_trace(y = y + 20, name = "vh", line = list(shape = "vh")) %>%
  add_trace(y = y + 25, name = "hv", line = list(shape = "hv"))
# result -> https://plot.ly/~botty/3297


# ----------------------------------------------------------------------
# https://plot.ly/r/log-plot/
# ----------------------------------------------------------------------

d <- diamonds[sample(nrow(diamonds), 1000), ]

#' Without log scales
(p <- plot_ly(d, x = carat, y = price, mode = "markers"))
# result -> https://plot.ly/~botty/3433

#' With log scales
layout(p, xaxis = list(type = "log", autorange = T),
       yaxis = list(type = "log", autorange = T))
# result -> https://plot.ly/~botty/3431

# ----------------------------------------------------------------------
# https://plot.ly/r/graphing-multiple-chart-types/
# ----------------------------------------------------------------------

# necessary?

# ----------------------------------------------------------------------
# https://plot.ly/r/polar-chart/
# ----------------------------------------------------------------------

data(mic, package = "plotly")
p <- plot_ly(mic, r = r, t = t, color = nms, mode = "lines")
layout(p, title = "Mic Patterns", orientation = -90)
# result ->  https://plot.ly/~botty/3417

data(hobbs, package = "plotly")
p <- plot_ly(hobbs, r = r, t = t, color = nms, opacity = 0.7, mode = "markers")
layout(p, title = "Hobbs-Pearson Trials", plot_bgcolor = toRGB("grey90"))
# result ->  https://plot.ly/~botty/3415

data(wind, package = "plotly")
p <- plot_ly(wind, r = r, t = t, color = nms, type = "area")
layout(p, radialaxis = list(ticksuffix = "%"), orientation = 270)
# result -> https://plot.ly/~botty/3413

# ----------------------------------------------------------------------
# https://plot.ly/r/time-series/
# ----------------------------------------------------------------------

#' POSIXlt date/time class
now_lt <- as.POSIXlt(Sys.time(), tz = "GMT")
tm <- seq(0, 600, by = 10)
x <- now_lt - tm
y <- rnorm(length(x))
plot_ly(x = x, y = y, text = paste(tm, "seconds from now in GMT"))
# result -> https://plot.ly/~botty/3361

#' POSIXct date/time class
now_ct <- as.POSIXct(Sys.time())
tm <- seq(0, 600, by = 10)
x <- now_ct - tm
y <- rnorm(length(x))
plot_ly(x = x, y = y, text = paste(tm, "seconds from now in", Sys.timezone()))
# result -> https://plot.ly/~botty/3363

#' Dates
today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))
plot_ly(x = x, y = y, text = paste(tm, "days from today"))
# result -> https://plot.ly/~botty/3355
