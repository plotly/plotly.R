library(plotly)
library(crosstalk)

nPatients <- 50
nVisits <- 10

df <- data.frame(
  perc = rnorm(n = nPatients * nVisits, mean = 50, sd = 10),
  patient = rep(seq(nPatients), each = nVisits),
  visit = rep(seq(nVisits), nPatients)
)

sd <- SharedData$new(df, ~patient)

# click and drag to select patient(s)
plot_ly(sd, x = ~visit, y = ~perc) %>%
  group_by(patient) %>%
  add_lines(color = I("orange")) %>% 
  add_markers(color = I("steelblue"))
