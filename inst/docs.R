# install the new/experimental plotly R package
# devtools::install_github("ropensci/plotly@carson-dsl")

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

# necessary?

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
#  https://plot.ly/python/choropleth-maps/
# ----------------------------------------------------------------------------

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


##########################################################################

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
  # TODO: how to add the hyperlink? (<a href=""> doesn't seem to work)
  layout(title = '2014 Global GDP<br>Source: CIA World Factbook', geo = g)

##########################################################################

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
            # plotly should support "unboxed" constants
            lon = list(21.0936), lat = list(7.1881), text = list('Africa')) %>%
  add_trace(type = 'choropleth', locations = Country, locationmode = 'country names',
            z = Month, colors = "black", showscale = F, geo = 'geo2', data = df9) %>%
  layout(title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
         geo = g1, geo2 = g2)

# ----------------------------------------------------------------------------
#  https://plot.ly/python/lines-on-maps/
# ----------------------------------------------------------------------------

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

##########################################################################


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

##########################################################################

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
#  https://plot.ly/python/scatter-plots-on-maps/
# ----------------------------------------------------------------------------

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
df$hover <- with(df, paste(airport, city, state, "Arrivals: ", cnt))

# TODO: rework utils so that marker specs aren't written over
plot_ly(df, lat = lat, lon = long, text = hover, color = cnt, 
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers',
        marker = list(size = 8, opacity = 0.8, symbol = 'square')) %>%
  layout(
    title = 'Most trafficked US airports<br>(Hover for airport names)',
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5        
    )
  )

##########################################################################

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2015_06_30_precipitation.csv')
df$hover <- paste(df$Globvalue, "inches")

# change default color scale title
m <- list(colorbar = list(title = "Total Inches"))

plot_ly(df, lat = Lat, lon = Lon, text = hover, color = Globvalue,
        type = 'scattergeo', marker = m) %>%
  layout(title = 'US Precipitation 06-30-2015<br>Source: NOAA',
         geo = list(
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
             range= c(-140, -55),
             dtick = 5
           ),
           lataxis = list(
             showgrid = TRUE,
             gridwidth = 0.5,
             range= c(20, 60),
             dtick = 5
           )
         )
  )

# ----------------------------------------------------------------------------
#  https://plot.ly/python/bubble-maps/
# ----------------------------------------------------------------------------

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$hover <- paste(df$name, "Population", df$pop/1e6, " million")

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

plot_ly(df, lon = lon, lat = lat, text = hover,
        marker = list(size = sqrt(pop/10000) + 1),
        color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(
    title = '2014 US city populations<br>(Click legend to toggle traces)',
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),       
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white") 
    )
  )

# ----------------------------------------------------------------------------
# https://plot.ly/python/map-subplots-and-small-multiples/
# ----------------------------------------------------------------------------

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


yrs <- unique(df$YEAR)
id <- seq_along(yrs)
df$id <- factor(df$YEAR, levels = id)
df2 <- data.frame(
  YEAR = yrs,
  id = id
)


p <- plot_ly(df, type = 'scattergeo', lon = LON, lat = LAT, group = YEAR, 
             geo = paste0("geo", id), showlegend = F,
             marker = list(color = toRGB("blue"), opacity = 0.5)) %>%
  add_trace(lon = list(-78), lat = list(47), mode = 'text', group = YEAR,
            geo = paste0("geo", id), text = list(YEAR), data = df2) %>%
  layout(title = 'New Walmart Stores per year 1962-2006<br> 
         Source: <a href="http://www.econ.umn.edu/~holmes/data/WalMart/index.html">
         University of Minnesota</a>',
         geo = g,
         autosize = F,
         width = 1000,
         height = 900,
         hovermode = F)

subplot(p, nrows = 7)


# ----------------------------------------------------------------------------
# https://plot.ly/r/privacy/
# ----------------------------------------------------------------------------

library(plotly)

#' public
plot_ly(x = c(0, 2, 4), y = c(0, 4, 2))

#' private
plot_ly(x = c(0, 2, 4), y = c(0, 4, 2), 
        world_readable = FALSE, filename = "privacy-true")

# another option is to "build" the plot, then tack on these properties
p <- plot_ly(x = c(0, 2, 4), y = c(0, 4, 2))
p <- plotly_build(p)
p$world_readable <- FALSE
p$filename <- "privacy-true"
p

