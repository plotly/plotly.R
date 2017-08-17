library(plotly)
library(crosstalk)

nPatients <- 50
nVisits <- 10

df <- data.frame(
  perc = rnorm(n = nPatients * nVisits, mean = 50, sd = 10),
  patient = rep(seq(nPatients), each = nVisits),
  visit = rep(seq(nVisits), nPatients)
)

# delare the patient variable as the "unit of interest"
sd <- SharedData$new(df, ~patient)

p <- plot_ly(sd, x = ~visit, y = ~perc, color = I("black"),
             text = ~paste("Patient:", patient)) %>%
  group_by(patient) %>%
  add_trace(mode = "markers+lines") %>%
  highlight("plotly_selected")

# Since crosstalk's SharedData object was supplied to plot_ly() with a key of
# patient, it knows to highlight any lines/markers matching the selected patient(s).
# By default, the "on trigger" is "plotly_click", but we've changed that to 
# "plotly_selected", which corresponds to click and drag mouse events.
# Plotly provides two types of drag modes that will trigger a "plotly_selected"
# event: "lasso" and "select". You can change the dragmode interactively via 
# the modebar and/or set the default dragmode via `layout()`.
layout(p, dragmode = "lasso")

# Other interaction types, beyond click and drag interactions, can also select 
# value(s) of a SharedData key and are specified via the highlight() function. 
# The first argument, `on`, sets the interaction type used to add values to the 
# selection set. The second argument, `off`, sets the interaction required to 
# clear the selection set and return to the original view. By default, a
# "plotly_relayout" event will clear the selection set. This event is triggered
# by clicking on the home icon in the mode bar, or double-clicking on the plot
# when in a zoom or pan dragmode. Some other sensible events for clearing the
# selection set are "plotly_deselect" and "plotly_doubleclick". Both events are
# triggered with a double click, but are dependant upon the current dragmode
# ("plotly_deselect" is triggered when in select/lasso dragmode and 
# "plotly_doubleclick" when in zoom/pan dragmode).
p %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick") %>% 
  layout(dragmode = "zoom")

# By default, all selections are transient, meaning prior selections are 
# removed from the selection set before new selections are added. To prevent
# prior selections from being removed, simply set the persistent argument to 
# `TRUE`.
highlight(p, on = "plotly_hover", persistent = TRUE)

# Sometimes its useful to compare two or more different selection sets. 
# For example, how do patients with a high response on visit 1 compare to those 
# with a low response? To make this sort of comparison, we can alter the color
# in multiple persistent selections. By setting the dynamic argument to `TRUE` 
# a colourpicker htmlwidget (@colourpicker) will appear just above the plotly 
# visualization. At any given time, the value of this widget controls the 
# color of new selection(s).
highlight(p, on = "plotly_hover", persistent = TRUE, dynamic = TRUE)

# By default, the colourpicker widget uses colors from the "Set1" 
# colour brewer palette (@RColorBrewer), but any set of valid R colors can 
# be supplied to the color argument.
colors <- RColorBrewer::brewer.pal(4, "Dark2")
highlight(p, on = "plotly_hover", color = colors, dynamic = TRUE, persistent = TRUE)
