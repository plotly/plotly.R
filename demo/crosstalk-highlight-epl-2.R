library(engsoccerdata)
library(dplyr)
library(tidyr)
library(plotly)

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
  mutate(gameno = row_number(), cumpts = cumsum(pts)) %>%
  ungroup() %>%
  group_by(gameno) %>%
  mutate(meanP = mean(cumpts)) %>%
  filter(Season > 2006)

sd <- highlight_key(dat, ~team, "Select a team")

# a 'wormchart' like fig 8 here http://www.gradaanwr.net/wp-content/uploads/2016/06/dataApr16.pdf
p <- ggplot(sd, aes(x = gameno, y = cumpts - meanP)) + 
  geom_line(aes(group = team), alpha = 0.5) + 
  facet_wrap(~ Season, ncol = 3) + 
  labs(
    title = "English Premier League Performance",
    x = "Game in season",
    y = "Cumulative points (above/below) average"
  ) 

gg <- ggplotly(p, tooltip = "team")

highlight(
  gg, 
  dynamic = TRUE, 
  selectize = TRUE,
  color = RColorBrewer::brewer.pal(12, "Paired")
)
