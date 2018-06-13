library(plotly)

nPatients <- 50
nVisits <- 10

d <- data.frame(
  perc = rnorm(n = nPatients * nVisits, mean = 50, sd = 10),
  patient = rep(seq(nPatients), each = nVisits),
  visit = rep(seq(nVisits), nPatients)
)

# Define a 'primary key' variable (patient) for interactive queries
hd <- highlight_key(d, ~patient)

# Create a "repeated measures" plot
p <- plot_ly(hd, x = ~visit, y = ~perc, color = I("black"),
             text = ~paste("Patient:", patient)) %>%
  group_by(patient) %>%
  add_trace(mode = "markers+lines")

# Since the data provided (hd) has a primary key definition of 'patient',
# **plotly** knows to highlight any lines/markers matching the selected patient(s).
# Go ahead and *click* on any marker and watch the entire 'patient' be highlighted
layout(p, title = "Click on a marker to highlight that patient")

# By default, the "on event" is "plotly_click", but we can change that to 
# "plotly_selected", which corresponds to click and drag mouse events.
p %>%
  layout(title = "Click and drag to select patient") %>%
  highlight("plotly_selected")


# Plotly provides two types of drag modes that will trigger a "plotly_selected"
# event: "lasso" and "select". You can change the dragmode interactively via 
# the modebar and/or set the default dragmode via `layout()`.
p %>%
  layout(title = "Click and drag to select patient", dragmode = "lasso") %>%
  highlight("plotly_selected")

# The first argument of `highlight()`, `on`, sets the interaction type used 
# trigger a "highlight selection". The second argument, `off`, sets the 
# interaction required to clear the selection set and return to the original view. 
# IF you don't provide an `off` event, a sensible one will be choosen based
# on the value of `on`.
p %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick") %>% 
  layout(dragmode = "zoom")

# By default, all selections are transient, meaning prior selections are 
# removed from the selection set before new selections are added. To prevent
# prior selections from being removed, hold down the shift key while triggering
# the event
p %>%
  layout(title = "Shift the key to accumulate selections") %>%
  highlight("plotly_hover")

# Sometimes its useful to compare two or more different selection sets. 
# For example, how do patients with a high response on visit 1 compare to those 
# with a low response? To make this sort of comparison, we can alter the color
# in multiple persistent selections. By setting the dynamic argument to `TRUE` 
# a colourpicker will appear just above the plotly visualization. 
# At any given time, the value of this widget controls the color of new selection(s).
highlight(p, dynamic = TRUE)

# By default, the colourpicker widget uses colors from the "Set1" 
# colour brewer palette (@RColorBrewer), but any set of valid R colors can 
# be supplied to the color argument.
colors <- RColorBrewer::brewer.pal(4, "Dark2")
highlight(p, color = colors, dynamic = TRUE)
