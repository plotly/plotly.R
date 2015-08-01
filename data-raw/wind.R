r = c(77.5, 72.5, 70.0, 45.0, 22.5, 42.5, 40.0, 62.5)
t = c("North", "N-E", "East", "S-E", "South", "S-W", "West", "N-W")
r2 = c(57.5, 50.0, 45.0, 35.0, 20.0, 22.5, 37.5, 55.0)
t2 = c("North", "N-E", "East", "S-E", "South", "S-W", "West", "N-W")
r3 = c(40.0, 30.0, 30.0, 35.0, 7.5, 7.5, 32.5, 40.0)
t3 = c("North", "N-E", "East", "S-E", "South", "S-W", "West", "N-W")
r4 = c(20.0, 7.5, 15.0, 22.5, 2.5, 2.5, 12.5, 22.5)
t4 = c("North", "N-E", "East", "S-E", "South", "S-W", "West", "N-W")

nms <- c(
  rep("11-14 m/s", length(r)),
  rep("8-11 m/s", length(r2)),
  rep("5-8 m/s", length(r3)),
  rep("less than m/s", length(r4))
)

wind <- data.frame(
  r = c(r, r2, r3, r4),
  t = c(t, t2, t3, t4),
  nms = ordered(nms, levels = c("less than m/s", "5-8 m/s", "8-11 m/s", "11-14 m/s"))
)
save(wind, file = "data/wind.rda")
