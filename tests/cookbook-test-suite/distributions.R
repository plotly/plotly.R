set.seed(1234)
df <- data.frame(cond = factor( rep(c("A","B"), each=200) ),
                   rating = c(
                    c(-1.207065749, 0.277429242, 1.084441177, -2.345697703, 0.429124689, 0.506055892, -0.574739960, -0.546631856, -0.564451999, -0.890037829-0.477192700, -0.998386445, -0.776253895, 0.064458817, 0.959494059-0.110285494, -0.511009506, -0.911195417, -0.837171680, 2.415835178, 0.134088220, -0.490685897, -0.440547872, 0.459589441, -0.693720247-1.448204910, 0.574755721, -1.023655723, -0.015138300, -0.935948601, 1.102297546, -0.475593079, -0.709440038, -0.501258061, -1.629093469-1.167619262, -2.180039649, -1.340993192, -0.294293859, -0.465897540, 1.449496265, -1.068642724, -0.855364634, -0.280623002, -0.994340076-0.968514318, -1.107318193, -1.251985886, -0.523828119, -0.496849957-1.806031257, -0.582075925, -1.108889624, -1.014962009, -0.162309524, 0.563055819, 1.647817473, -0.773353424, 1.605909629, -1.157808548, 0.656588464, 2.548991071, -0.034760390, -0.669633580, -0.007604756, 1.777084448, -1.138607737, 1.367827179, 1.329564791, 0.336472797, 0.006892838, -0.455468738, -0.366523933, 0.648286568, 2.070270861-0.153398412, -1.390700947, -0.723581777, 0.258261762, -0.317059115-0.177789958, -0.169994077, -1.372301886, -0.173787170, 0.850232257, 0.697608712, 0.549997351, -0.402731975, -0.191593770, -1.194527880-0.053158819, 0.255196001, 1.705964007, 1.001513252, -0.495583443, 0.355550297, -1.134608044, 0.878203627, 0.972916753, 2.121117105, 0.414523534, -0.474718474, 0.065993494, -0.502477782, -0.825998587, 0.166989280, -0.896264626, 0.168185388, 0.354968261, -0.052105117-0.195934619, -0.649069752, -1.109767231, 0.849274203, 0.022362526, 0.831140617, -1.244287851, 0.169026414, 0.673166307, -0.026276376-0.191392169, -0.781906647, 2.058161988, 0.750501453, 1.824208302, 0.080059641, -0.631409299, -1.513288120, -0.636099831, 0.226301532, 1.013690347, 0.252750135, -1.171948313, 0.668714329, -1.650100935-0.365852248, -0.316118329, -1.948246047, 0.920057522, -0.622871595-0.334036650, 1.395147893, 0.636674411, -0.108431697, 0.513762778, 0.399271807, 1.662856447, 0.275893404, 0.506272623, 0.347551975-0.377237647, 0.097619463, 1.638744645, -0.875592474, 0.121759999, 1.362130661, -0.234621087, -1.053382808, -0.869783606, -0.390127030-0.847350073, -0.260639392, -0.414419706, -0.183050798, 0.407056098, 0.624633128, 1.678205744, -0.068693654, -0.320839913, 1.471005717, 1.704329398, 0.043244038, -0.332657319, -1.822235418, 1.411262399-0.837582434, -1.123762794, 3.043765886, 0.235021308, -0.033258610-2.732219523, -0.099790588, 0.976031735, 0.413868915, 0.912322162, 1.983732201, 1.169108514, -0.508737015, 0.704180178, -0.198416274-0.538070789, -2.855758655, -0.789646852, 0.487814635, 2.168032540, 0.500694614, 0.620210204, -0.965903210, 0.162654708, -2.078237542),
                    c(1.28522682, 1.49676878, 0.98551392, 1.50073352, 1.11168103, 1.56046236, 2.64246363, 1.91236284, 0.83266396, -0.31444896, 1.21805782, 0.39976476, 2.29349310, -0.80708094, 0.38424821, 1.22200837, 0.64826346, 0.19384888, 0.49527893, 1.42953610, 1.69517198, 1.46021263, 3.07348352, 1.97349757, 1.08770973, 0.14022991, 3.71914013, 1.47741550, 0.11567966, 0.98649208, 0.47560670, 0.52529578, -0.13350334, 0.91684534, 1.11916024, -0.27754212, -2.43315213, 0.54512535, 0.82951783, 1.39427377, 0.85913517, 1.21339889, -0.29777217, 1.51117526, 1.51888873, 1.05165107, 2.15727444, 1.20446847, 1.06436427, 1.06804390, 1.23693058, 1.86012391, 1.25219040, 1.46319862, -0.33637355, 0.42950248, 2.27696959, -0.42390375, 1.05806839, 1.20500281, 1.77580332, 0.45112326, 0.95862544, -0.96325507, 1.13859605, 0.13343497, 0.56135338, -0.38776528, 1.18493532, 1.46657952, 0.49538611, 2.62501106, 1.47055937, 1.74863257, 2.84940300, 0.14888639, 1.60861927, 1.78658061, 0.79382920, 1.11905236, -0.21182190, 1.27016755, 0.09902967, 1.61368286, -0.01143078, 1.11939749, -0.04652265, 0.55423682, -0.75285901, 0.92843403, 1.78544339, 0.98324752, -0.96622921, 0.17946630, 2.45604304, 2.60980539, -0.37503677, 0.43329674, 1.15362545, 1.11915622, 0.22004301, -0.15327870, 0.62057141, 1.80980821, 0.82362661, 0.15097178, 0.29562578, 2.41439150, 0.35304019, 1.56317676, 2.27171869, 1.24366490, 0.37827813, 0.75999837, 0.30772003, 2.02771712, 0.65044643, 2.34998338, 0.23838746, 0.15288275, 0.94313216, 0.82418865, 0.29554848, -0.78139681, 0.83006642, 0.08342330, 1.88261096, -0.15268545, 1.92648273, 0.15095698, 1.09247008, 1.69870272, 0.28125764, 1.35443855, 0.71202633, -0.33521293, 0.52992039, 2.41978988, 0.58586883, -0.01778246, 0.74597708, 1.13014161, 1.75532461, 1.94395988, 0.90052240, 1.96457525, 0.03574005, -1.54451340, 0.32831657, 0.28414451, -1.51603615, 1.36247176, 0.01622486, 0.57394601, -0.78710299, 1.34752420, 2.69122702, -0.07807711, 0.68744109, 2.74871306, 1.73381633, 2.71305942, 0.79476594, 0.64773995, 0.29036834, 2.23457370, -0.48583853, 1.10731422, 0.75368147, 3.05184180, 0.19196627, -0.70928817, 1.03263177, 0.76035130, -0.03912507, 0.93229109, 0.52447525, 0.12124136, 1.30083593, 0.46833769, -1.03498025, -1.85174125, 0.21941783, 2.25418687, 1.63812938, 2.01505358, 1.78250544, 1.11576402, -0.70706263, 1.00556979, 2.39722809, -2.59606353, 0.01864772, 1.90246464, 1.32874502, 1.58939440, 1.25709951, 1.33883312, 0.81464312, -0.11648914, 1.28522682, 1.49676878, 0.98551392, 1.50073352, 1.11168103, 1.56046236, 2.64246363, 1.91236284, 0.83266396, -0.31444896, 1.21805782, 0.39976476, 2.29349310, -0.80708094, 0.38424821, 1.22200837, 0.64826346, 0.19384888)))
# cond     rating
#    A -1.2070657
#    A  0.2774292
#    A  1.0844412
#   ...
#    B  1.3388331
#    B  0.8146431
#    B -0.1164891


# Basic histogram from the vector "rating". Each bin is .5 wide.
# These both do the same thing:
qplot(df$rating, binwidth=.5)
g <- ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5)
save_outputs(g, "distributions/basic-histogram", file_prefix="")

# Draw with black outline, white fill
g <- ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white")
save_outputs(g, "distributions/basic-histogram-white-filling", file_prefix="")

# Density curve
g <- ggplot(df, aes(x=rating)) + geom_density()
save_outputs(g, "distributions/basic-density-curve", file_prefix="")

# Histogram overlaid with kernel density curve
g <- ggplot(df, aes(x=rating)) +
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
save_outputs(g, "distributions/basic-density-curve-with-histogram", file_prefix="")

g <- ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white") +
    geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1)
save_outputs(g, "distributions/histogram-with-vertical-line", file_prefix="")

# Overlaid histograms
g <- ggplot(df, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, alpha=.5, position="identity")
save_outputs(g, "distributions/overlaid-histograms", file_prefix="")

# Interleaved histograms
g <- ggplot(df, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")
save_outputs(g, "distributions/grouped-histograms", file_prefix="")

# Density plots
g <- ggplot(df, aes(x=rating, colour=cond)) + geom_density()
save_outputs(g, "distributions/multiple-density-plots", file_prefix="")

# Density plots with semi-transparent fill
g <- ggplot(df, aes(x=rating, fill=cond)) + geom_density(alpha=.3)
save_outputs(g, "distributions/filled-density-plots", file_prefix="")

# Find the mean of each group
library(plyr)
cdf <- ddply(df, "cond", summarise, rating.mean=mean(rating))
# cond rating.mean
#    A -0.05775928
#    B  0.87324927


# Overlaid histograms with means
g <- ggplot(df, aes(x=rating, fill=cond)) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity") +
    geom_vline(data=cdf, aes(xintercept=rating.mean,  colour=cond),
               linetype="dashed", size=1)
## TODO: uncomment when fixed: error "rating" not found
# save_outputs(g, "distributions/overlaid-histograms-with-means", file_prefix="")

# Density plots with means
g <- ggplot(df, aes(x=rating, colour=cond)) + geom_density() +
    geom_vline(data=cdf, aes(xintercept=rating.mean,  colour=cond),
               linetype="dashed", size=1)
## TODO: uncomment when fixed: error "rating" not found
# save_outputs(g, "distributions/density-plot-with-means", file_prefix="")

g <- ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white") +
    facet_grid(cond ~ .)
save_outputs(g, "distributions/faceted-histograms", file_prefix="")

# With mean lines, using cdf from above
g <- ggplot(df, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white") +
    facet_grid(cond ~ .) +
    geom_vline(data=cdf, aes(xintercept=rating.mean),
               linetype="dashed", size=1, colour="red")
## TODO: uncomment when fixed: error "rating" not found
# save_outputs(g, "distributions/faceted-histograms-with-mean-lines", file_prefix="")

# A basic box plot
g <- ggplot(df, aes(x=cond, y=rating)) + geom_boxplot()
save_outputs(g, "distributions/basic-box-plot", file_prefix="")

# A basic box with the conditions colored
g <- ggplot(df, aes(x=cond, y=rating, fill=cond)) + geom_boxplot()
save_outputs(g, "distributions/box-plot-with-conditions-colored", file_prefix="")

# The above adds a redundant legend. With the legend removed:
g <- ggplot(df, aes(x=cond, y=rating, fill=cond)) + geom_boxplot() +
    guides(fill=FALSE)
save_outputs(g, "distributions/box-plot-with-legend-removed", file_prefix="")

# With flipped axes
g <- ggplot(df, aes(x=cond, y=rating, fill=cond)) + geom_boxplot() +
    guides(fill=FALSE) + coord_flip()
save_outputs(g, "distributions/box-plot-with-flipped-axes", file_prefix="")

# Add a diamond at the mean, and make it larger
g <- ggplot(df, aes(x=cond, y=rating)) + geom_boxplot() +
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)
save_outputs(g, "distributions/box-plot-with-diamond-means", file_prefix="")


