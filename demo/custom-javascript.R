library(plotly)
library(htmlwidgets)

mtcars$url <- paste0(
  "http://google.com/#q=", 
  rownames(mtcars)
)

p <- plot_ly(mtcars, x = ~wt, y = ~mpg) %>%
  add_markers(customdata = ~url)

# open google search (on click)
onRender(p, "
  function(el, x) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      window.open(url);
    });
  }
")

# annotate graph (on click)
onRender(p, "
  function(el, x) {
    el.on('plotly_click', function(d) {
      var ann = {
        text: d.points[0].customdata,
        x: 0,
        y: 0,
        xref: 'paper',
        yref: 'paper',
        yshift: -40,
        showarrow: false
      }
      Plotly.relayout(el.id, {annotations: [ann]})
    });
  }
")

# annotate on hover
onRender(p, "
  function(el, x) {
    el.on('plotly_hover', function(d) {
      var ann = {
        text: d.points[0].customdata,
        x: 0,
        y: 0,
        xref: 'paper',
        yref: 'paper',
        yshift: -40,
        showarrow: false
      }
      Plotly.relayout(el.id, {annotations: [ann]})
    });
  }
")

# combine highlight() api with custom javascript
mtcars %>%
  highlight_key() %>%
  plot_ly(x = ~wt, y = ~mpg, customdata = ~url) %>%
  highlight(color = "red") %>%
  onRender("
  function(el, x) {
    el.on('plotly_click', function(d) {
      var ann = {
        text: d.points[0].customdata,
        x: 0,
        y: 0,
        xref: 'paper',
        yref: 'paper',
        yshift: -40,
        showarrow: false
      }
      Plotly.relayout(el.id, {annotations: [ann]})
    });
  }
  ")

# inspect different plotly.js events via browser console
# see also https://plot.ly/javascript/plotlyjs-events/
onRender(p, "
  function(el, x) {
    el.on('plotly_hover', function(d) {
      console.log('hover', d)
    });
    el.on('plotly_click', function(d) {
      console.log('click', d)
    });
    el.on('plotly_selected', function(d) {
      console.log('selected', d)
    });
    el.on('plotly_relayout', function(d) {
      console.log('relayout', d)
    });
  }"
)

