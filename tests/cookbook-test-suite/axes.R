bp <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()

bp1 <- bp + coord_flip()
save_outputs(bp1, "axes/coord flip", file_prefix="")

# Manually set the order of a discrete-valued axis
bp2 <- bp + scale_x_discrete(limits=c("trt1","trt2","ctrl"))
save_outputs(bp2, "axes/discrete valued axes", file_prefix="")

# Reverse the order of a discrete-valued axis
# Get the levels of the factor
flevels <- levels(PlantGrowth$group)
# "ctrl" "trt1" "trt2"
# Reverse the order
flevels <- rev(flevels)
# "trt2" "trt1" "ctrl"
bp3 <- bp + scale_x_discrete(limits=flevels)
save_outputs(bp3, "axes/reversed ordered axes - 1", file_prefix="")

# Or it can be done in one line:
bp4 <- bp + scale_x_discrete(limits = rev(levels(PlantGrowth$group)) )
save_outputs(bp4, "axes/reversed ordered axes - 2", file_prefix="")

bp5 <- bp + scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"), labels=c("Control", "Treat 1", "Treat 2"))
save_outputs(bp5, "axes/setting tick mark labels", file_prefix="")

# Hide x tick marks, labels, and grid lines
bp6 <- bp + scale_x_discrete(breaks=NULL)
save_outputs(bp6, "axes/hidden tick marks labels gridline", file_prefix="")

# Hide all tick marks and labels (on X axis), but keep the gridlines
bp7 <- bp + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
save_outputs(bp7, "axes/hidden tick marks and labels", file_prefix="")

# Set the range of a continuous-valued axis
# These are equivalent
bp8 <- bp + ylim(0,8)
save_outputs(bp8, "axes/set range of continuous-valued axis - 1", file_prefix="")
bp9 <- bp + scale_y_continuous(limits=c(0,8))
save_outputs(bp9, "axes/set range of continuous-valued axis - 2", file_prefix="")

# These two do the same thing; all data points outside the graphing range are dropped,
# resulting in a misleading box plot
bp10 <- bp + ylim(5, 7.5)
save_outputs(bp10, "axes/misleading range", file_prefix="")
bp + scale_y_continuous(limits=c(5, 7.5))

# Using coord_cartesian "zooms" into the area
bp11 <- bp + coord_cartesian(ylim=c(5, 7.5))
save_outputs(bp11, "axes/coord_cartesian", file_prefix="")

# Specify tick marks directly
bp12 <- bp + coord_cartesian(ylim=c(5, 7.5)) +
  scale_y_continuous(breaks=seq(0, 10, 0.25))  # Ticks from 0-10, every .25
save_outputs(bp12, "axes/specify tick marks directly", file_prefix="")

# Reverse order of a continuous-valued axis
bp13 <- bp + scale_y_reverse()
save_outputs(bp13, "axes/reverse y scale", file_prefix="")

# Setting the tick marks on an axis
# This will show tick marks on every 0.25 from 1 to 10
# The scale will show only the ones that are within range (3.50-6.25 in this case)
bp14 <- bp + scale_y_continuous(breaks=seq(1,10,1/4))
save_outputs(bp14, "axes/manual tick marks", file_prefix="")

# The breaks can be spaced unevenly
bp15 <- bp + scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6,8))
save_outputs(bp15, "axes/uneven tick marks", file_prefix="")

# Suppress ticks and gridlines
bp16 <- bp + scale_y_continuous(breaks=NULL)
save_outputs(bp16, "axes/suppress y ticks labels and gridlines", file_prefix="")

# Hide tick marks and labels (on Y axis), but keep the gridlines
bp17 <- bp + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
save_outputs(bp17, "axes/suppress y ticks and labels", file_prefix="")

# Create some noisy exponentially-distributed data
xval = c(0.26932812,-0.05341404,0.36977717,0.91504712,0.46329006,0.37956526, 0.93290644,0.75558976,0.67633497,0.48655293,0.79478162,0.55109982, 0.51681398,0.81073512,0.49406579,0.93919618,0.90472008,0.98732256, 0.94379876,0.95790909,0.54614241,1.13356941,1.13299144,1.18159277, 1.16428407,1.22955005,1.21030897,1.23314811,1.53822718,1.53674330, 1.80020468,1.40774011,1.74573515,1.26651625,2.06607711,1.50237263, 1.38480531,1.83625381,2.35275649,1.99004291,2.80396442,2.20863240, 2.42998876,2.12801180,2.26290348,2.38185989,2.14936036,2.66587947, 2.64586596,2.44240603,2.39266452,3.11831215,2.70258927,2.65529134, 2.65634690,2.95984290,2.71058076,2.87919480,3.07739358,2.66841935, 3.10792706,3.17134285,3.98070271,3.55497279,3.36831009,3.31390892, 3.32753965,2.86981968,3.22741000,3.78806438,3.74434536,3.56928928, 3.83783177,3.24485807,4.05766233,4.13619455,4.26888054,3.47546258, 3.93045819,3.77620080,4.66676431,3.88059240,4.54694485,4.03915767, 4.25556093,4.39251819,4.42692029,4.23262929,4.44890758,4.84981161, 4.51104252,4.33004508,5.06350705,4.89714069,4.21599077,4.55457578, 5.04044393,4.89111297,5.03105215,4.64113164)
yval = c(1.177512e+01,7.303113e+00,6.109053e+00,2.545169e+01,3.366341e+01,1.042255e+01,2.703767e+01,1.178223e+01,4.495965e+01,1.614609e+01,4.003015e+01,1.038442e+02,4.024992e+01,4.163942e+01,9.108197e+01,3.116299e+01,2.558871e+02,7.482977e+01,2.502789e+01,5.923683e+01,3.967814e+01,9.207318e+01,1.298618e+02,1.138197e+02,1.804303e+02,3.363494e+02,3.197204e+02,4.968737e+02,1.783433e+02,4.765546e+02,4.486885e+02,6.736079e+02,4.289288e+02,3.433946e+02,5.658634e+02,4.667053e+02,5.257803e+02,3.401038e+02,6.131335e+02,5.928647e+02,7.838524e+02,7.987915e+02,3.348470e+03,1.704767e+03,1.264169e+03,2.690011e+03,2.738240e+03,1.663862e+03,5.377442e+03,3.883820e+03,6.673624e+03,1.857346e+03,6.683962e+03,1.213027e+03,1.742885e+03,2.146094e+03,4.597174e+03,4.357154e+03,8.413851e+03,8.194194e+03,7.076611e+03,1.554628e+04,6.984783e+03,1.027392e+04,1.158795e+04,9.193111e+03,3.226748e+04,3.955445e+04,2.978953e+04,1.926420e+04,7.610544e+04,2.129694e+04,1.438764e+04,7.908876e+04,2.676003e+04,1.791758e+05,3.978871e+04,9.411120e+04,4.486940e+04,1.270526e+05,1.587331e+05,1.616173e+05,3.351522e+05,3.001782e+05,2.527824e+05,2.745851e+05,3.446376e+05,1.544497e+05,1.318314e+05,8.334336e+05,2.464391e+05,8.694818e+05,2.747323e+05,6.373497e+05,2.918690e+05,9.505114e+05,7.835278e+05,3.775567e+05,1.795523e+06,1.568159e+06)

dat <- data.frame(xval = xval, yval = yval)

# A scatterplot with regular (linear) axis scaling
sp <- ggplot(dat, aes(xval, yval)) + geom_point()
save_outputs(sp, "axes/linear axes", file_prefix="")

# log2 scaling of the y axis (with visually-equal spacing)
library(scales)     # Need the scales package
sp1 <- sp + scale_y_continuous(trans=log2_trans())
save_outputs(sp1, "axes/ln y axes with visual-equal spacing", file_prefix="")

# log2 coordinate transformation (with visually-diminishing spacing)
sp2 <- sp + coord_trans(y="log2")
save_outputs(sp2, "axes/ln y axes with visually diminishing spacing", file_prefix="")

sp3 <- sp + scale_y_continuous(trans = log2_trans(),
                               breaks = trans_breaks("log2", function(x) 2^x),
                               labels = trans_format("log2", math_format(2^.x)))
save_outputs(sp3, "axes/ln y axes with exponent tick marks", file_prefix="")

dat10 <- data.frame(xval = xval, yval = yval)

sp10 <- ggplot(dat10, aes(xval, yval)) + geom_point()

# log10
sp101 <- sp10 + scale_y_log10()
save_outputs(sp101, "axes/log_10 y axes", file_prefix="")

# log10 with exponents on tick labels
sp102 <- sp10 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))
save_outputs(sp102, "axes/log_10 y axes with exponent tick marks", file_prefix="")

# Data where x ranges from 0-10, y ranges from 0-30
set.seed(202)
dat <- data.frame(xval = runif(40,0,10), yval = runif(40,0,30))
sp <- ggplot(dat, aes(xval, yval)) + geom_point()

# Force equal scaling
sp4 <- sp + coord_fixed()
save_outputs(sp4, "axes/forced equal spacing", file_prefix="")

# Equal scaling, with each 1 on the x axis the same length as y on x axis
sp5 <- sp + coord_fixed(ratio=1/3)
save_outputs(sp5, "axes/forced equal scaling", file_prefix="")

bp10 <- bp + theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Weight (Kg)")                    # Set y-axis label
save_outputs(bp10, "axes/axes labels", file_prefix="")

# Also possible to set the axis label with the scale
# Note that vertical space is still reserved for x"s label
bp11 <- bp + scale_x_discrete(name="") +
  scale_y_continuous(name="Weight (Kg)")
save_outputs(bp11, "axes/axes labels set with scale", file_prefix="")

# Change font options:
# X-axis label: bold, red, and 20 points
# X-axis tick marks: rotate 90 degrees CCW, move to the left a bit (using vjust,
#   since the labels are rotated), and 16 points
bp12 <- bp + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
                   axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
save_outputs(bp12, "axes/axes labels with formatting", file_prefix="")

# Label formatters
library(scales)   # Need the scales package
bp13 <- bp + scale_y_continuous(labels=percent) +
  scale_x_discrete(labels=abbreviate)  # In this particular case, it has no effect
save_outputs(bp13, "axes/axes labels with percent labels", file_prefix="")

# Self-defined formatting function for times.
timeHMS_formatter <- function(x) {
    h <- floor(x/60)
    m <- floor(x %% 60)
    s <- round(60*(x %% 1))                   # Round to nearest second
    lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
    lab <- gsub("^00:", "", lab)              # Remove leading 00: if present
    lab <- gsub("^0", "", lab)                # Remove leading 0 if present
}

bp14 <- bp + scale_y_continuous(label=timeHMS_formatter)
save_outputs(bp14, "axes/axes labels with custom time labels", file_prefix="")

# Hide all the gridlines
bp15 <- bp + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
save_outputs(bp15, "axes/hidden gridlines", file_prefix="")

# Hide just the minor gridlines
bp16 <- bp + theme(panel.grid.minor=element_blank())
save_outputs(bp16, "axes/hidden minor gridlines", file_prefix="")

# Hide all the horizontal gridlines
bp17 <- bp + theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())
save_outputs(bp17, "axes/hidden horizontal gridlines", file_prefix="")

# Hide all the vertical gridlines
bp18 <- bp + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())
save_outputs(bp18, "axes/hidden vertical gridlines", file_prefix="")
