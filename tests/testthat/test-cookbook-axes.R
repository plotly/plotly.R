context("cookbook axes")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("cookbook-axes-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

bp <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

# Reverse the order of a discrete-valued axis
# Get the levels of the factor
flevels <- levels(PlantGrowth$group)
# "ctrl" "trt1" "trt2"
# Reverse the order
flevels <- rev(flevels)
# "trt2" "trt1" "ctrl"
bp.flevels <- bp + scale_x_discrete(limits = flevels)

test_that("factor levels determine tick order", {
  info <- expect_traces(bp.flevels, 1, "flevels")
  expect_equivalent(info$layout$xaxis$ticktext, c("trt2", "trt1", "ctrl"))
})

## These two do the same thing; all data points outside the graphing
## range are dropped, resulting in a misleading box plot.
bp.ylim.hide <- bp + ylim(5, 7.5)
test_that("ylim hides points", {
  info <- expect_traces(bp.ylim.hide, 1, "ylim.hide")
})

bp.scale.hide <- bp + scale_y_continuous(limits = c(5, 7.5))
test_that("scale_y(limits) hides points", {
  info <- expect_traces(bp.scale.hide, 1, "scale.hide")
  expect_equivalent(range(info$layout$yaxis$tickvals), c(5, 7.5))
  y <- unlist(lapply(info$data, "[[", "y"))
  expect_true(all(5 <= y & y <= 7.5, na.rm = TRUE))
})

bp.coord <- bp + coord_cartesian(ylim = c(5, 7.5))
test_that("Using coord_cartesian zooms into the area", {
  info <- expect_traces(bp.coord, 1, "coord-ylim")
  expect_equivalent(range(info$layout$yaxis$tickvals), c(5, 7.5))
  y <- unlist(lapply(info$data, "[[", "y"))
  expect_false(all(5 <= y & y <= 7.5))
})

# Create some noisy exponentially-distributed data
dat <- data.frame(
  xval = c(0.26932812,-0.05341404,0.36977717,0.91504712,0.46329006,0.37956526, 0.93290644,0.75558976,0.67633497,0.48655293,0.79478162,0.55109982, 0.51681398,0.81073512,0.49406579,0.93919618,0.90472008,0.98732256, 0.94379876,0.95790909,0.54614241,1.13356941,1.13299144,1.18159277, 1.16428407,1.22955005,1.21030897,1.23314811,1.53822718,1.53674330, 1.80020468,1.40774011,1.74573515,1.26651625,2.06607711,1.50237263, 1.38480531,1.83625381,2.35275649,1.99004291,2.80396442,2.20863240, 2.42998876,2.12801180,2.26290348,2.38185989,2.14936036,2.66587947, 2.64586596,2.44240603,2.39266452,3.11831215,2.70258927,2.65529134, 2.65634690,2.95984290,2.71058076,2.87919480,3.07739358,2.66841935, 3.10792706,3.17134285,3.98070271,3.55497279,3.36831009,3.31390892, 3.32753965,2.86981968,3.22741000,3.78806438,3.74434536,3.56928928, 3.83783177,3.24485807,4.05766233,4.13619455,4.26888054,3.47546258, 3.93045819,3.77620080,4.66676431,3.88059240,4.54694485,4.03915767, 4.25556093,4.39251819,4.42692029,4.23262929,4.44890758,4.84981161, 4.51104252,4.33004508,5.06350705,4.89714069,4.21599077,4.55457578, 5.04044393,4.89111297,5.03105215,4.64113164), 
  yval = c(1.177512e+01,7.303113e+00,6.109053e+00,2.545169e+01,3.366341e+01,1.042255e+01,2.703767e+01,1.178223e+01,4.495965e+01,1.614609e+01,4.003015e+01,1.038442e+02,4.024992e+01,4.163942e+01,9.108197e+01,3.116299e+01,2.558871e+02,7.482977e+01,2.502789e+01,5.923683e+01,3.967814e+01,9.207318e+01,1.298618e+02,1.138197e+02,1.804303e+02,3.363494e+02,3.197204e+02,4.968737e+02,1.783433e+02,4.765546e+02,4.486885e+02,6.736079e+02,4.289288e+02,3.433946e+02,5.658634e+02,4.667053e+02,5.257803e+02,3.401038e+02,6.131335e+02,5.928647e+02,7.838524e+02,7.987915e+02,3.348470e+03,1.704767e+03,1.264169e+03,2.690011e+03,2.738240e+03,1.663862e+03,5.377442e+03,3.883820e+03,6.673624e+03,1.857346e+03,6.683962e+03,1.213027e+03,1.742885e+03,2.146094e+03,4.597174e+03,4.357154e+03,8.413851e+03,8.194194e+03,7.076611e+03,1.554628e+04,6.984783e+03,1.027392e+04,1.158795e+04,9.193111e+03,3.226748e+04,3.955445e+04,2.978953e+04,1.926420e+04,7.610544e+04,2.129694e+04,1.438764e+04,7.908876e+04,2.676003e+04,1.791758e+05,3.978871e+04,9.411120e+04,4.486940e+04,1.270526e+05,1.587331e+05,1.616173e+05,3.351522e+05,3.001782e+05,2.527824e+05,2.745851e+05,3.446376e+05,1.544497e+05,1.318314e+05,8.334336e+05,2.464391e+05,8.694818e+05,2.747323e+05,6.373497e+05,2.918690e+05,9.505114e+05,7.835278e+05,3.775567e+05,1.795523e+06,1.568159e+06)
)

sp <- ggplot(dat, aes(xval, yval)) + geom_point()

test_that("A scatterplot with regular (linear) axis scaling", {
  info <- expect_traces(sp, 1, "linear-axes")
})

library(scales)
sp.log2.scale <- sp + scale_y_continuous(trans = log2_trans())

test_that("log2 scaling of the y axis (with visually-equal spacing)", {
  info <- expect_traces(sp.log2.scale, 1, "log2-scale")
})

sp.log2.coord <- sp + coord_trans(y = "log2")

test_that("log2 coordinate transformation with visually-diminishing spacing", {
  info <- expect_traces(sp.log2.coord, 1, "log2-coord")
})

sp.labels <- sp +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))

test_that("log2 transform with labels", {
  info <- expect_traces(sp.labels, 1, "log2-labels")
})

sp.log10 <- sp + scale_y_log10()

test_that("scale_y_log10", {
  info <- expect_traces(sp.log10, 1, "scale_y_log10")
})

sp.log10.labels <- sp +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

test_that("log10 with exponents on tick labels", {
  info <- expect_traces(sp.log10.labels, 1, "scale_y_log10-labels")
})

no.x.title <- bp +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Weight (Kg)")                    # Set y-axis label

test_that("coord_fixed(ratio)", {
  info <- expect_traces(no.x.title, 1, "no-x-title")
  expect_identical(info$layout$xaxis$title, "")
})

# Also possible to set the axis label with the scale
# Note that vertical space is still reserved for x"s label

bp.scale.name <- bp + scale_x_discrete(name="") +
  scale_y_continuous(name="Weight (Kg)")

test_that("scale(name)", {
  info <- expect_traces(bp.scale.name, 1, "scale-name")
})

# Change font options:
# X-axis label: bold, red, and 20 points
# X-axis tick marks: rotate 90 degrees CCW, move to the left a bit (using vjust,
#   since the labels are rotated), and 16 points

bp.fonts <- bp +
  theme(
    axis.title.x = element_text(face = "bold", colour = "#990000", size = 20),
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 16)
  )

test_that("element_text face, colour, size, angle, vjust, size", {
  info <- expect_traces(bp.fonts, 1, "fonts")
  expect_equivalent(info$layout$xaxis$tickangle, -90)
  expect_match(info$layout$xaxis$title, "<b>", fixed = TRUE)
  expect_match(info$layout$xaxis$title, "</b>", fixed = TRUE)
})

# Label formatters
library(scales)   # Need the scales package

label.funs <- bp +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = abbreviate)

test_that("In this particular case, x scale has no effect", {
  info <- expect_traces(label.funs, 1, "label-funs")
})

# Self-defined formatting function for times.
timeHMS_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  s <- round(60*(x %% 1))                   # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- gsub("^00:", "", lab)              # Remove leading 00: if present
  lab <- gsub("^0", "", lab)                # Remove leading 0 if present
}

custom.formatter <- bp + scale_y_continuous(label = timeHMS_formatter)

test_that("custom HMS formatter function", {
  info <- expect_traces(custom.formatter, 1, "custom-formatter")
})

blank.minor.major <- bp +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

test_that("Hide all the gridlines", {
  info <- expect_traces(blank.minor.major, 1, "blank-minor-major")
})

blank.minor <- bp +
  theme(panel.grid.minor = element_blank())

test_that("Hide just the minor gridlines", {
  info <- expect_traces(blank.minor, 1, "blank-minor")
})

blank.x <- bp +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

test_that("Hide all the horizontal gridlines", {
  info <- expect_traces(blank.x, 1, "blank-x")
})

blank.y <- bp +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

test_that("Hide all the vertical gridlines", {
  info <- expect_traces(blank.y, 1, "blank-y")
})
