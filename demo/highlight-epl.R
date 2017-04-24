library(engsoccerdata)
library(dplyr)
library(tidyr)
library(plotly)
library(crosstalk)

# shape data into desired format
dat <- england %>% 
  gather(location, team, home, visitor) %>% 
  # focus on tier 1 teams that are still playing in 2015
  filter(team %in% maketable_eng(england, 2015, 1)[["team"]]) %>%
  mutate(
    pts = ifelse(location == "home" & goaldif > 0, 3, 
                 ifelse(location == "away" & goaldif < 0, 3, 1))
  ) %>%
  arrange(Date) %>%
  group_by(Season, team) %>% 
  mutate(gameno = row_number(), cumpts = cumsum(pts))

sd <- SharedData$new(dat, ~Season, "Select a season")

p <- ggplot(sd, aes(x = gameno, y = cumpts)) + 
  geom_line(aes(color = Season, group = Season), alpha = 0.5) + 
  facet_wrap(~ team) + ggtitle("English Premier League Performance (1888-2015)") +
  xlab("Game in Season") + ylab("Cumulative Points") 

gg <- ggplotly(p, tooltip = "colour")

highlight(gg, opacityDim = 0.05, selectize = TRUE)

