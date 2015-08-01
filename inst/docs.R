# install the new/experimental plotly R package
# devtools::install_github("ropensci/plotly@carson-dsl")

################################################################################
# Basic Charts (https://plot.ly/r/#basic-charts)
################################################################################

# ----------------------------------------------------------------------
# https://plot.ly/r/3d-line-plots/
# ----------------------------------------------------------------------

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
library(plotly)
plot_ly(df, x = x, y = y, z = z, color = time, type = "scatter3d")

# ----------------------------------------------------------------------
# https://plot.ly/r/3d-scatter-plots/
# ----------------------------------------------------------------------

# variance-covariance matrix for a multivariate normal distribution
s <- matrix(c(1, .5, .5, 
              .5, 1, .5, 
              .5, .5, 1), ncol = 3)
# use the mvtnorm package to sample 200 observations
obs <- mvtnorm::rmvnorm(200, sigma = s)
# collect everything in a data-frame
df <- setNames(data.frame(obs), c("x", "y", "z"))

library(plotly)
plot_ly(df, x = x, y = y, z = z, type = "scatter3d", mode = "markers")

# ----------------------------------------------------------------------
# https://plot.ly/r/3d-surface-plots/
# ----------------------------------------------------------------------

library(plotly)
# volcano is a numeric matrix that ships with R
plot_ly(z = volcano, type = "surface")

# 2D kernel density estimation
kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
with(kd, plot_ly(x = x, y = y, z = z, type = "surface"))


# ----------------------------------------------------------------------
# https://plot.ly/r/filled-area-plots/
# ----------------------------------------------------------------------

library(plotly)
p <- plot_ly(x = c(1, 2, 3, 4), y = c(0, 2, 3, 5), fill = "tozeroy")
add_trace(p, x = c(1, 2, 3, 4), y = c(3, 5, 1, 7), fill = "tonexty")


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

p2 <- add_trace(
  p, 
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(12, 18, 29),
  name = "LA Zoo"
)
p2

layout(p2, barmode = "stack")

## customizing colors

library(dplyr)
ggplot2::diamonds %>% count(cut) %>%
  plot_ly(x = cut, y = n, type = "bar", marker = list(color = toRGB("black")))

# mapping a color variable
ggplot2::diamonds %>% count(cut, clarity) %>%
  plot_ly(x = cut, y = n, type = "bar", color = clarity)

# ----------------------------------------------------------------------
# https://plot.ly/r/box-plots/
# ----------------------------------------------------------------------

library(plotly)
#' basic boxplot
plot_ly(y = rnorm(50), type = "box") %>%
  add_trace(y = rnorm(50, 1))
#' adding jittered points
plot_ly(y = rnorm(50), type = "box", boxpoints = "all", jitter = 0.3,
        pointpos = -1.8)
#' several box plots
plot_ly(ggplot2::diamonds, y = price, color = cut, type = "box")

#' grouped box plots
plot_ly(ggplot2::diamonds, x = cut, y = price, color = clarity, type = "box") %>%
  layout(boxmode = "group")

# ----------------------------------------------------------------------
# https://plot.ly/r/bubble-charts/ 
# ----------------------------------------------------------------------

# IMO, this page should be a part of this page -> https://plot.ly/r/line-and-scatter/
library(plotly)
d <- diamonds[sample(nrow(diamonds), 1000), ]
# note how size is automatically scaled and added as hover text
plot_ly(d, x = carat, y = price, size = carat, mode = "markers")

plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat, opacity = carat)

# ----------------------------------------------------------------------
# https://plot.ly/r/contour-plots/
# ----------------------------------------------------------------------

#' Basic contour
library(plotly)
plot_ly(z = volcano, type = "contour")

#' Advanced
x <- rnorm(200) 
y <- rnorm(200)
p1 <- plot_ly(x = x, type = "histogram", showlegend = FALSE)
p2 <- plot_ly(x = x, y = y, type = "histogram2dcontour")
p3 <- plot_ly(y = y, type = "histogram", showlegend = FALSE)
a1 <- list(domain = c(0, .85))
a2 <- list(domain = c(.85, 1))
hide <- list(title = "", showticklabels = FALSE)
subplot(
  layout(p1, xaxis = c(a1, hide), yaxis = a2),
  layout(p2, xaxis = a1, yaxis = a1),
  layout(p3, xaxis = a2, yaxis = c(a1, hide))
) 

# ----------------------------------------------------------------------
# https://plot.ly/r/error-bars/
# ----------------------------------------------------------------------

library(dplyr)
library(plotly)

p <- ggplot2::mpg %>% group_by(class) %>%
  summarise(mn = mean(hwy), sd = 1.96 * sd(hwy)) %>%
  arrange(desc(mn)) %>% 
  plot_ly(x = class, y = mn, error_y = list(value = sd), 
          mode = "markers", name = "Highway") %>%
  layout(yaxis = list(title = "Miles Per Gallon"))
p

df2 <- mpg %>% group_by(class) %>%
  summarise(mn = mean(cty), sd = 1.96 * sd(cty))

add_trace(p, y = mn, error_y = list(value = sd), 
          name = "City", data = df2)

# ----------------------------------------------------------------------
# https://plot.ly/r/heatmaps/
# ----------------------------------------------------------------------

library(plotly)
plot_ly(z = volcano, type = "heatmap")


#' categorical x/y axis
m <- matrix(rnorm(9), nrow = 3, ncol = 3)
plot_ly(z = m, x = c("a", "b", "c"), y = c("d", "e", "f"), type = "heatmap")

#' Sequential Colorscales (Hot)
plot_ly(z = volcano, colorscale = "Hot", type = "heatmap")

#' Sequential Colorscales (Greys)
plot_ly(z = volcano, colorscale = "Greys", type = "heatmap")

#' Sequential Colorscales (Greens)
plot_ly(z = volcano, colorscale = "Greens", type = "heatmap")

#' Custom colorscale via scales package
vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
plot_ly(z = volcano, colorscale = colz, type = "heatmap")

library(viridis)
plot_ly(z = volcano, colors = viridis(256), type = "heatmap")

# ----------------------------------------------------------------------
# https://plot.ly/r/2D-Histogram/
# ----------------------------------------------------------------------

library(plotly)
s <- matrix(c(1, -.75, -.75, 1), ncol = 2)
obs <- mvtnorm::rmvnorm(500, sigma = s)
plot_ly(x = obs[,1], y = obs[,2], type = "histogram2d")

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

#' Scatterplot with qualitative colorscale
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Species, 
        mode = "markers")

#' colors argument accepts colorbrewer2.org palette names
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Species,
        colors = "Set1", mode = "markers")
#' By default, colors will 'span the gamut'
# scales::show_col(RColorBrewer::brewer.pal("Set1"))

#' If you want finer control over the color scheme, you can pass
#' RGB or hex color codes directly to colors
pal <- RColorBrewer::brewer.pal(nlevels(iris$Species), "Set1")
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Species,
        colors = pal, mode = "markers")

#' Scatterplot with sequential colorscale
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Petal.Width, mode = "markers")

#' Basic time-series (line) plot with loess smooth 
plot_ly(economics, x = date, y = uempmed, name = "unemployment")
add_trace(y = fitted(loess(uempmed ~ as.numeric(date))))

#' Density plot
dens <- with(diamonds, tapply(price, INDEX = cut, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)
plot_ly(df, x = x, y = y, color = cut)

#' Different line interpolation options
x <- 1:5
y <- c(1, 3, 2, 3, 1)
plot_ly(x = x, y = y, name = "linear", line = list(shape = "linear")) %>%
  add_trace(y = y + 5, name = "spline", line = list(shape = "spline")) %>%
  add_trace(y = y + 10, name = "vhv", line = list(shape = "vhv")) %>%
  add_trace(y = y + 15, name = "hvh", line = list(shape = "hvh")) %>%
  add_trace(y = y + 20, name = "vh", line = list(shape = "vh")) %>%
  add_trace(y = y + 25, name = "hv", line = list(shape = "hv"))

# ----------------------------------------------------------------------
# https://plot.ly/r/log-plot/
# ----------------------------------------------------------------------

d <- diamonds[sample(nrow(diamonds), 1000), ]

#' Without log scales
(p <- plot_ly(d, x = carat, y = price, mode = "markers"))

#' With log scales
layout(p, xaxis = list(type = "log", autorange = T),
       yaxis = list(type = "log", autorange = T))

# ---------------------------------------------------------------------
# https://plot.ly/r/graphing-multiple-chart-types/
# ----------------------------------------------------------------------

#' Scatterplot with loess smoother

library(plotly)
mtcars <- mtcars[order(mtcars$disp), ]
p <- plot_ly(mtcars, x = disp, y = mpg, mode = "markers", 
             text = rownames(mtcars), showlegend = FALSE) 
add_trace(p, y = fitted(loess(mpg ~ disp)), mode = "lines", 
          name = "loess smoother", showlegend = TRUE) 

#' Scatterplot with loess smoother and it's uncertaincy estimates
m <- loess(mpg ~ disp, data = mtcars)
f <- with(predict(m, se = TRUE), data.frame(fit, se.fit))

l <- list(
  color = toRGB("gray90", alpha = 0.3),
  fillcolor = toRGB("gray90", alpha = 0.3)
)

p %>%
  add_trace(p, data = f, y = fit, mode = "lines") %>%
  add_trace(p, data = f, y = fit + 1.96 * se.fit, mode = "lines", 
            fill = "tonexty", line = l) %>%
  add_trace(p, data = f, y = fit - 1.96 * se.fit, mode = "lines", 
            fill = "tonexty", line = l)

# ----------------------------------------------------------------------
# https://plot.ly/r/polar-chart/
# ----------------------------------------------------------------------

p <- plot_ly(plotly::mic, r = r, t = t, color = nms, mode = "lines")
layout(p, title = "Mic Patterns", orientation = -90)

p <- plot_ly(plotly::hobbs, r = r, t = t, color = nms, opacity = 0.7, mode = "markers")
layout(p, title = "Hobbs-Pearson Trials", plot_bgcolor = toRGB("grey90"))

p <- plot_ly(plotly::wind, r = r, t = t, color = nms, type = "area")
layout(p, radialaxis = list(ticksuffix = "%"), orientation = 270)

# ----------------------------------------------------------------------
# https://plot.ly/r/time-series/
# ----------------------------------------------------------------------

#' POSIXlt date/time class
now_lt <- as.POSIXlt(Sys.time(), tz = "GMT")
tm <- seq(0, 600, by = 10)
x <- now_lt - tm
y <- rnorm(length(x))
plot_ly(x = x, y = y, text = paste(tm, "seconds from now in GMT"))

#' POSIXct date/time class
now_ct <- as.POSIXct(Sys.time())
tm <- seq(0, 600, by = 10)
x <- now_ct - tm
y <- rnorm(length(x))
plot_ly(x = x, y = y, text = paste(tm, "seconds from now in", Sys.timezone()))

#' Dates
today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))
plot_ly(x = x, y = y, text = paste(tm, "days from today"))

# ----------------------------------------------------------------------------
#  https://plot.ly/r/choropleth-maps/  (new)
# ----------------------------------------------------------------------------

#' World Choropleth Map
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(
  color = toRGB("white"), 
  width = 2
)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_ly(df, z = total.exports, text = hover, locations = code, type = 'choropleth', 
        locationmode = 'USA-states', color = total.exports, colors = 'Purples', 
        marker = list(line = l), colorbar = list(title = "Millions USD")) %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)

#' World Choropleth Map
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

# light grey boundaries
l <- list(
  color = toRGB("grey"),
  width = 0.5
)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

plot_ly(df, z = GDP..BILLIONS., text = COUNTRY, locations = CODE, type = 'choropleth', 
        color = GDP..BILLIONS., colors = 'Blues', marker = list(line = l),
        colorbar = list(tickprefix = '$', title = 'GDP Billions US$')) %>%
  layout(title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>', 
         geo = g)

#' Choropleth Inset Map
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_ebola.csv')
# restrict from June to September
df <- subset(df, Month %in% 6:9)
# ordered factor variable with month abbreviations
df$abbrev <- ordered(month.abb[df$Month], levels = month.abb[6:9])
# September totals
df9 <- subset(df, Month == 9)

# common plot options
g <- list(
  scope = 'africa',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)


plot_ly(df, type = 'scattergeo', mode = 'markers', locations = Country,
        locationmode = 'country names', text = paste(Value, "cases"), 
        color = as.ordered(abbrev), marker = list(size = Value/50), inherit = F) %>%
  add_trace(type = 'scattergeo', mode = 'text', geo = 'geo2', showlegend = F,
            lon = 21.0936, lat = 7.1881, text = 'Africa') %>%
  add_trace(type = 'choropleth', locations = Country, locationmode = 'country names',
            z = Month, colors = "black", showscale = F, geo = 'geo2', data = df9) %>%
  layout(title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
         geo = g1, geo2 = g2)

# ----------------------------------------------------------------------------
#  https://plot.ly/r/lines-on-maps/ (new)
# ----------------------------------------------------------------------------

#' US Flight Paths Map

# airport locations
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# flights between airports
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_ly(air, lon = long, lat = lat, text = airport, type = 'scattergeo',
        locationmode = 'USA-states', marker = list(size = 2, color = 'red'),
        inherit = FALSE) %>% 
  add_trace(lon = list(start_lon, end_lon), lat = list(start_lat, end_lat),
            group = id, opacity = cnt/max(cnt), data = flights,
            mode = 'lines', line = list(width = 1, color = 'red'),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'Feb. 2011 American Airline flight paths<br>(Hover for airport names)',
         geo = geo, showlegend = FALSE)

#' London to NYC Great Circle
library(plotly)
plot_ly(lat = c(40.7127, 51.5072), lon = c(-74.0059, 0.1275), type = 'scattergeo',
        mode = 'lines', line = list(width = 2, color = 'blue')) %>%
  layout(
    title = 'London to NYC Great Circle',
    showlegend = FALSE,         
    geo = list(
      resolution = 50,
      showland = TRUE,
      showlakes = TRUE,
      landcolor = toRGB("grey80"),
      countrycolor = toRGB("grey80"),
      lakecolor = toRGB("white"),
      projection = list(type = "equirectangular"),
      coastlinewidth = 2,
      lataxis = list(
        range = c(20, 60),
        showgrid = TRUE,
        tickmode = "linear",
        dtick = 10
      ),
      lonaxis = list(
        range = c(-100, 20),
        showgrid = TRUE,
        tickmode = "linear",
        dtick = 20
      )
    )
  )

#' Contour lines on globe
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/globe_contours.csv')
df$id <- seq_len(nrow(df))

library(tidyr)
d <- df %>%
  gather(key, value, -id) %>%
  separate(key, c("l", "line"), "\\.") %>%
  spread(l, value)

p <- plot_ly(type = 'scattergeo', mode = 'lines', 
             line = list(width = 2, color = 'violet'))

for (i in unique(d$line)) 
  p <- add_trace(p, lat = lat, lon = lon, data = subset(d, line == i))

geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("grey90"),
  lakecolor = toRGB("white"),
  oceancolor = toRGB("white"),
  projection = list( 
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )            
  ),
  lonaxis = list( 
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list( 
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

layout(p, showlegend = FALSE, geo = geo,
       title = 'Contour lines over globe<br>(Click and drag to rotate)')

# ----------------------------------------------------------------------------
#  https://plot.ly/r/scatter-plots-on-maps/ (new)
# ----------------------------------------------------------------------------

#' US Airports Map
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
df$hover <- with(df, paste(airport, city, state, "Arrivals: ", cnt))

# marker styling
m <- list(
  colorbar = list(title = "Incoming flights February 2011"),
  size = 8, opacity = 0.8, symbol = 'square'
)

# geo styling
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5        
)

plot_ly(df, lat = lat, lon = long, text = hover, color = cnt, 
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers',
        marker = m) %>%
  layout(title = 'Most trafficked US airports<br>(Hover for airport)', geo = g)

#' North American Precipitation Map
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2015_06_30_precipitation.csv')
df$hover <- paste(df$Globvalue, "inches")

# change default color scale title
m <- list(colorbar = list(title = "Total Inches"))

# geo styling
g <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = 'conic conformal',
    rotation = list(
      lon = -100
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(20, 60),
    dtick = 5
  )
)

plot_ly(df, lat = Lat, lon = Lon, text = hover, color = Globvalue,
        type = 'scattergeo', marker = m) %>%
  layout(title = 'US Precipitation 06-30-2015<br>Source: NOAA', geo = g)

# ----------------------------------------------------------------------------
#  https://plot.ly/r/bubble-maps/ (new)
# ----------------------------------------------------------------------------

#' United States Bubble Map
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$hover <- paste(df$name, "Population", df$pop/1e6, " million")

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),       
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white") 
)

plot_ly(df, lon = lon, lat = lat, text = hover,
        marker = list(size = sqrt(pop/10000) + 1),
        color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

#' Ebola Cases in West Africa

# see 'Choropleth Inset Map' example

# ----------------------------------------------------------------------------
# https://plot.ly/r/map-subplots-and-small-multiples/ (new)
# ----------------------------------------------------------------------------

#' US map small multiples
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/1962_2006_walmart_store_openings.csv')

# common map properties
g <- list(
  scope = 'usa',
  showland = T,
  landcolor = toRGB("gray90"),
  showcountries = F,
  subunitcolor = toRGB("white")
)

# year text labels
yrs <- unique(df$YEAR)
id <- seq_along(yrs)
df2 <- data.frame(
  YEAR = yrs,
  id = id
)

# id for anchoring traces on different plots
df$id <- as.integer(factor(df$YEAR))

p <- plot_ly(df, type = 'scattergeo', lon = LON, lat = LAT, group = YEAR, 
             geo = paste0("geo", id), showlegend = F,
             marker = list(color = toRGB("blue"), opacity = 0.5)) %>%
  add_trace(lon = -78, lat = 47, mode = 'text', group = YEAR,
            geo = paste0("geo", id), text = YEAR, data = df2) %>%
  layout(title = 'New Walmart Stores per year 1962-2006<br> Source: <a href="http://www.econ.umn.edu/~holmes/data/WalMart/index.html">University of Minnesota</a>',
         geo = g,
         autosize = F,
         width = 1000,
         height = 900,
         hovermode = F)

subplot(p, nrows = 9)

################################################################################
# Multiple Axes, Subplots, and Insets (https://plot.ly/r/#multiple-axes-subplots-and-insets)
################################################################################


# ----------------------------------------------------------------------------
# https://plot.ly/r/subplots/
# ----------------------------------------------------------------------------

#' Basic subplot
library(plotly)
subplot(
  plot_ly(economics, x = date, y = uempmed),
  plot_ly(economics, x = date, y = unemploy),
  margin = 0.05
) %>% layout(showlegend = FALSE)

#' Sharing an axis
subplot(
  plot_ly(economics, x = date, y = uempmed),
  plot_ly(economics, x = date, y = unemploy),
  margin = 0.03,
  nrows = 2,
  # fyi, share doesn't work (yet)
  share = "x"
) %>% layout(showlegend = FALSE)

# ----------------------------------------------------------------------------
# https://plot.ly/r/multiple-axes/
# ----------------------------------------------------------------------------

library(plotly)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right"
)
plot_ly(x = 1:3, y = 10*(1:3), name = "slope of 10") %>%
  add_trace(x = 2:4, y = 1:3, name = "slope of 1", yaxis = "y2") %>%
  layout(title = "Double Y Axis", yaxis2 = ay)


# ----------------------------------------------------------------------------
# https://plot.ly/r/insets/
# ----------------------------------------------------------------------------

p1 <- plot_ly(x = c(1, 2, 3), y = c(4, 3, 2))
p2 <- plot_ly(x = c(20, 30, 40), y = c(30, 40, 50)) %>%
  layout(xaxis = list(domain = c(0.6, 0.95)), 
         yaxis = list(domain = c(0.6, 0.95)))
subplot(p1, p2)


################################################################################
# Layout Options
################################################################################

# ----------------------------------------------------------------------------
# https://plot.ly/r/setting-graph-size/
# ----------------------------------------------------------------------------

library(plotly)
m = list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
plot_ly(x = seq(0, 8), y = seq(0, 8)) %>%
  layout(autosize = F, width = 500, height = 500, margin = m)

# ----------------------------------------------------------------------------
# https://plot.ly/r/legend/
# ----------------------------------------------------------------------------

#' Legend Names
library(plotly)
p <- plot_ly(x = seq(0, 8), y = rnorm(8), name = "Blue Trace") %>%
  add_trace(y = rnorm(8), name = "Orange Trace")
p

#' Hiding the Legend
p %>% layout(showlegend = FALSE)

#' Positioning the Legend
p %>% layout(legend = list(x = 0.5, y = 0))

#' Styling the Legend
f <- list(
  family = "sans-serif",
  size = 12,
  color = "#000"
)
l <- list(
  font = f,
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2
) 
p %>% layout(legend = l)

#' Hiding Legend Entries
plot_ly(x = seq(0, 8), y = rnorm(8), showlegend = FALSE) %>%
  add_trace(y = rnorm(8), name = "Orange Trace", showlegend = TRUE)

# ----------------------------------------------------------------------------
# https://plot.ly/r/LaTeX/
# ----------------------------------------------------------------------------

library(plotly)
plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16),
        name = "$\\alpha_{1c} = 352 \\pm 11 \\text{ km s}^{-1}$") %>%
  add_trace(x = c(1, 2, 3, 4), y = c(0.5, 2, 4.5, 8),
            name = "$\\beta_{1c} = 25 \\pm 11 \\text{ km s}^{-1}$") %>%
  layout(xaxis = list(title = "$\\sqrt{(n_\\text{c}(t|{T_\\text{early}}))}$"),
         yaxis = list(title = "$d, r \\text{ (solar radius)}$"))

# ----------------------------------------------------------------------------
# https://plot.ly/r/figure-labels/
# ----------------------------------------------------------------------------

# NOTE: title and link of this page could be improvded

library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "x Axis",
  titlefont = f
)
y <- list(
  title = "y Axis",
  titlefont = f
)
plot_ly(x = rnorm(10), y = rnorm(10), mode = "markers") %>%
  layout(xaxis = x, yaxis = y)

# ----------------------------------------------------------------------------
# https://plot.ly/r/font/
# ----------------------------------------------------------------------------

library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
plot_ly(x = 0:8, y = 0:8) %>%
  layout(title = "Global Font", font = f)

# ----------------------------------------------------------------------------
# https://plot.ly/r/axes/
# ----------------------------------------------------------------------------

#' Style Axes Ticks and Placement

library(plotly)
a <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 0.25,
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("blue")
)
s <- seq(1, 4, by = 0.25)
plot_ly(x = s, y = s) %>%
  layout(xaxis = a, yaxis = a)

#' Style Axes Title and Ticks Labels

library(plotly)
f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "lightgrey"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
)
a <- list(
  title = "AXIS TITLE",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 45,
  tickfont = f2,
  exponentformat = "e",
  showexponent = "All"
)

s <- seq(0, 8)
plot_ly(x = s, y = s) %>%
  add_trace(y = rev(s)) %>%
  layout(xaxis = a, yaxis = a, showlegend = FALSE)

#' Style Axes and the Zero-Line

library(plotly)
ax <- list(
  zeroline = TRUE,
  showline = TRUE,
  mirror = "ticks",
  gridcolor = toRGB("gray50"),
  gridwidth = 2,
  zerolinecolor = toRGB("red"),
  zerolinewidth = 4,
  linecolor = toRGB("black"),
  linewidth = 6
)
s <- seq(-1, 4)
plot_ly(x = s, y = s) %>%
  layout(xaxis = ax, yaxis = ax) %>% offline

#' Hide Axes Title, Lines, Ticks, and Labels

library(plotly)
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

plot_ly(x = c(1, 2), y = c(1, 2)) %>%
  layout(xaxis = ax, yaxis = ax) %>% offline

#' Reversed Axes

library(plotly)
plot_ly(x = c(1, 2), y = c(1, 2)) %>%
  layout(xaxis = list(autorange = "reversed"))

#' Logarithmic Axes

library(plotly)
s <- seq(1, 8)
plot_ly(x = s, y = exp(s), name = "exponential") %>%
  add_trace(y =  s, name = "linear") %>%
  layout(yaxis = list(type = "log"))

#' Rangemode

library(plotly)
plot_ly(x = seq(2, 6, by = 2), y = seq(-3, 3, by = 3)) %>%
  layout(
    xaxis = list(rangemode = "tozero"),
    yaxis = list(rangemode = "nonnegative")
  )

#' Manual ranges

library(plotly)
s <- seq(1, 8)
plot_ly(x = s, y = s) %>%
  add_trace(y = rev(s)) %>%
  layout(
    xaxis = list(range = c(2, 5)),
    yaxis = list(range = c(2, 5))
  )

# ----------------------------------------------------------------------------
# https://plot.ly/r/text-and-annotations/
# ----------------------------------------------------------------------------

#' Text mode
plot_ly(mtcars, x = wt, y = mpg, text = rownames(mtcars), mode = "text")

#' Styling text
t <- list(
    family = "sans serif",
    size = 18,
    color = toRGB("grey50")
)
plot_ly(mtcars, x = wt, y = mpg, text = rownames(mtcars), mode = "markers+text",
        textfont = t, textposition = "top middle")

#' Show custom text on hover
plot_ly(mtcars, x = wt, y = mpg, text = rownames(mtcars), mode = "markers")

#' Single Annotation

m <- mtcars[which.max(mtcars$mpg), ]

a <- list(
  x = m$wt,
  y = m$mpg,
  text = rownames(m),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

plot_ly(mtcars, x = wt, y = mpg, mode = "markers") %>%
  layout(annotations = a)

#' Styling and Coloring Annotations

m <- mtcars[which.max(mtcars$mpg), ]

f <- list(
  family = "sans serif",
  size = 18,
  color = toRGB("white")
)

a <- list(
  textposition = "top right",
  font = f,
  x = m$wt,
  y = m$mpg,
  text = rownames(m),
  xref = "x",
  yref = "y",
  ax = 20,
  ay = -40,
  align = "center",
  arrowhead = 2,
  arrowsize = 1,
  arrowwidth = 2,
  arrowcolor = toRGB("lightblue"),
  bordercolor = toRGB("gray50"),
  borderwidth = 2,
  borderpad = 4,
  bgcolor = toRGB("blue"),
  opacity = 0.8
)

plot_ly(mtcars, x = wt, y = mpg, mode = "markers") %>%
  layout(annotations = a)

#' Many Annotations

a <- list()
for (i in seq_len(nrow(mtcars))) {
  m <- mtcars[i, ]
  a[[i]] <- list(
    x = m$wt,
    y = m$mpg,
    text = rownames(m),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
}
# could be improved with label dodging, but how to measure text height/width?
plot_ly() %>% layout(annotations = a)


################################################################################
# File Settings
################################################################################

# ----------------------------------------------------------------------------
# https://plot.ly/r/privacy/
# ----------------------------------------------------------------------------

library(plotly)

#' public
plot_ly(x = c(0, 2, 4), y = c(0, 4, 2))

#' private
plot_ly(x = c(0, 2, 4), y = c(0, 4, 2), world_readable = FALSE)

# another option is to "build" the plot, then tack on these properties
p <- plot_ly(x = c(0, 2, 4), y = c(0, 4, 2))
p <- plotly_build(p)
p$world_readable <- FALSE
p

# ----------------------------------------------------------------------------
# https://plot.ly/r/file-options/
# ----------------------------------------------------------------------------

library(plotly)

# By default, everytime you print a plotly object, it will create a new file
plot_ly(x = c(1, 2), y = c(1, 2), filename = "myPlot")

# There are a couple ways to prevent a new file from being created
# (1) Use get_figure() to obtain a figure object. 
fig <- get_figure("cpsievert", "559")
# Then modify and print that object (this will only work if you have proper credentials)
layout(fig, title = paste("Modified on ", Sys.time()))

# (2) If you know the filename attached to the plot you want to modify,
# place that name in filename and specify fileopt to overwrite that file
plot_ly(x = c(1, 2), y = c(1, 2), filename = "myPlot", fileopt = "overwrite")

# NOTE: filenames that contain "/" be treated as a Plotly directory and will be saved to your Plotly account in a folder tree. For example, to save your graphs to the folder my-graphs: 

################################################################################
# Get Requests, Static Image Export, and Interactive Embedding  (https://plot.ly/r/#get-requests-static-image-export)
################################################################################

# ----------------------------------------------------------------------------
# https://plot.ly/r/embedding-plotly-graphs-in-HTML/
# ----------------------------------------------------------------------------

# Maybe copy/paste relevant bits from this vignette? ->
# https://github.com/ropensci/plotly/blob/carson-dsl/vignettes/intro.Rmd

# ----------------------------------------------------------------------------
# https://plot.ly/r/shiny-tutorial/
# ----------------------------------------------------------------------------

# Maybe link to an updated version of this blog post?
# http://moderndata.plot.ly/dashboards-in-r-with-shiny-plotly/

# If we want, we could copy/paste source from this folder ->
# https://github.com/ropensci/plotly/tree/carson-dsl/inst/examples

# ----------------------------------------------------------------------------
# https://plot.ly/r/get-requests/
# ----------------------------------------------------------------------------

fig <- get_figure("cpsievert", "559")
layout(fig, title = paste("Modified on ", Sys.time()))

################################################################################
# Miscellaneous
################################################################################

# ----------------------------------------------------------------------------
# https://plot.ly/r/static-image-export/ (currently no R page)
# ----------------------------------------------------------------------------

# Use the curl package to download a static image of any publicly viewable figure
library(curl)
curl_download("https://plot.ly/~cpsievert/1000.png", "image.png")
curl_download("https://plot.ly/~cpsievert/1000.pdf", "image.pdf")

# you can also download the underlying SVG

curl_download("https://plot.ly/~cpsievert/1000.svg", "image.svg")
