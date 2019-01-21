# An improvement on https://www.r-bloggers.com/ternary-plots-in-r-using-plotly/

library(plotly)

# acquire data
ds <- jsonlite::fromJSON(
  "https://gist.githubusercontent.com/davenquinn/988167471993bc2ece29/raw/f38d9cb3dd86e315e237fde5d65e185c39c931c2/data.json"
)
df <- dplyr::bind_rows(ds, .id = "id")

# reusable function for creating annotation object
label <- function(txt) {
  list(
    text = txt, 
    x = 0.1, y = 1,
    ax = 0, ay = 0,
    xref = "paper", yref = "paper", 
    align = "center",
    font = list(family = "serif", size = 15, color = "white"),
    bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
  )
}

# reusable function for axis formatting
axis <- function(txt) {
  list(
    title = txt, tickformat = ".0%", tickfont = list(size = 10)
  )
}

ternaryAxes <- list(
  aaxis = axis("Clay"), 
  baxis = axis("Sand"), 
  caxis = axis("Silt")
)

plot_ly(
  df, a = ~clay, b = ~sand, c = ~silt, color = I("black"), type = "scatterternary"
) %>%
  layout(
    annotations = label("Ternary Markers"), ternary = ternaryAxes
  )

plot_ly(
  df, a = ~clay, b = ~sand, c = ~silt, color = I("black"), type = "scatterternary", 
  split = ~id, mode = "lines"
) %>%
  layout(
    annotations = label("Ternary Lines"), ternary = ternaryAxes
  )

plot_ly(
  df, a = ~clay, b = ~sand, c = ~silt, color = ~id, type = "scatterternary",
  fill = "toself", mode = "lines"
) %>%
  layout(
    annotations = label("Ternary Contour"), ternary = ternaryAxes
  )
