context("cookbook-scatter")

# Make some noisily increasing data
dat <- data.frame(
  cond = rep(c("A", "B"), each = 10),
  xvar = c(1.475957, -3.423712, 1.966129, 5.575364, 2.954719, 2.768286, 3.507499, 6.945000, 12.135050, 10.231673, 13.040393, 12.231689, 13.506993, 13.590874, 15.455178, 28.431185, 17.758937, 24.730797, 22.954238, 21.122766),
  yvar = c(-1.315387, 3.323239, 4.452183, 4.597885, 5.697203, 5.991221, 5.764561, 10.163165, 14.805634, 11.447913, 12.163597, 10.930851, 13.491366, 11.800783, 19.246991, 13.870457, 11.031923, 22.700302, 24.877547, 22.520114)
)
# cond         xvar         yvar
#    A -4.252354091  3.473157275
#    A  1.702317971  0.005939612
#   ...
#    B 17.793359218 19.718587761
#    B 19.319909163 19.647899863

test_that("scatterplots are generated without error", {
  g <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_point(shape=1)      # Use hollow circles
  expect_doppelganger_built(g, "hollow")
  
  g <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_point(shape=1) +
    geom_smooth(method=lm)   # Add linear regression line
  expect_doppelganger_built(g, "smooth-lm")
  
  g <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_point(shape=1) +
    geom_smooth(method=lm, se=FALSE)    # Don't add shaded confidence region
  expect_doppelganger_built(g, "smooth-lm-se-false")
  
  
  g <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth()            # Add a loess smoothed fit curve with confidence region
  expect_doppelganger_built(g, "loess")
  
  # Set color by cond
  g <- ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1)
  expect_doppelganger_built(g, "color")
  
  # # Same, but with different colors and add regression lines
  g <- ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=lm, se=FALSE)
  expect_doppelganger_built(g, "scale-color-hue")
  
  # Extend the regression lines beyond the domain of the data
  g <- ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1) +
    scale_colour_hue(l=50) +
    geom_smooth(method=lm, se=FALSE, fullrange=T)
  expect_doppelganger_built(g, "full-range")
  
  # Set shape by cond
  g <- ggplot(dat, aes(x=xvar, y=yvar, shape=cond)) + geom_point()
  expect_doppelganger_built(g, "shape")
  
  # Same, but with different shapes
  g <- ggplot(dat, aes(x=xvar, y=yvar, shape=cond)) + geom_point() +
    scale_shape_manual(values=c(1,2))  # Use a hollow circle and triangle
  expect_doppelganger_built(g, "shape-manual")
  
  # Round xvar and yvar to the nearest 5
  dat$xrnd <- round(dat$xvar/5)*5
  dat$yrnd <- round(dat$yvar/5)*5
  
  # Make each dot partially transparent, with 1/4 opacity
  # For heavy overplotting, try using smaller values
  g <- ggplot(dat, aes(x=xrnd, y=yrnd)) +
    geom_point(shape=19,      # Use solid circles
               alpha=1/4)     # 1/4 opacity
  expect_doppelganger_built(g, "overlap")
  
  # Jitter the points
  # Jitter range is 1 on the x-axis, .5 on the y-axis
  g <- ggplot(dat, aes(x=xrnd, y=yrnd)) +
    geom_point(shape=1,      # Use hollow circles
               position=position_jitter(width=1,height=.5))
  expect_doppelganger_built(g, "jitter")
  
  # Jitter the points using geom_jitter
  # Jitter range is 1 on the x-axis, .5 on the y-axis
  g <- ggplot(dat, aes(x = xrnd, y = yrnd)) +
    geom_jitter(shape = 1,      # Use hollow circles
                width = 1, height = 0.5)
  expect_doppelganger_built(g, "geom_jitter")
})
