library(plotly)
library(crosstalk)

nPatients <- 50
nVisits <- 10

df <- data.frame(
  fev1_perc = rnorm(n = nPatients * nVisits, mean = 100, sd = 10),
  uin = rep(seq(nPatients), each = nVisits),
  visit = rep(seq(nVisits), nPatients)
)

sd <- SharedData$new(df, ~uin)

plot_ly(sd, x = ~visit, y = ~fev1_perc) %>%
  group_by(uin) %>%
  add_lines(color = I("orange")) %>% 
  add_markers(color = I("steelblue"))
