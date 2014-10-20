bp <- ggplot(data=PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()

# Remove legend for a particular aesthetic (fill)
temp <- bp + guides(fill=FALSE)
save_outputs(temp, 'legends/hidden legend - 1', file_prefix="")

# It can also be done when specifying the scale
temp <- bp + scale_fill_discrete(guide=FALSE)
save_outputs(temp, 'legends/hidden legend - 2', file_prefix="")

# This removes all legends
temp <- bp + theme(legend.position="none")
save_outputs(temp, 'legends/hidden legend - all', file_prefix="")

bp + scale_fill_discrete(breaks=c("trt1","ctrl","trt2"))

# These two methods are equivalent:
bp + guides(fill = guide_legend(reverse=TRUE))
temp <- bp + scale_fill_discrete(guide = guide_legend(reverse=TRUE))
save_outputs(temp, 'legends/reversed legend', file_prefix="")

# You can also modify the scale directly:
bp + scale_fill_discrete(breaks = rev(levels(PlantGrowth$group)))

# Remove title for fill legend
temp <- bp + guides(fill=guide_legend(title=NULL))
save_outputs(temp, 'legends/removed legend title', file_prefix="")

# Remove title for all legends
temp <- bp + theme(legend.title=element_blank())
save_outputs(temp, 'legends/removed legend titles', file_prefix="")

temp <- bp + scale_fill_discrete(name="Experimental\nCondition")
save_outputs(temp, 'legends/multiple line legend title', file_prefix="")

temp <- bp + scale_fill_discrete(name="Experimental\nCondition",
                         breaks=c("ctrl", "trt1", "trt2"),
                         labels=c("Control", "Treatment 1", "Treatment 2"))
save_outputs(temp, 'legends/legend title with custom labels', file_prefix="")

# Using a manual scale instead of hue
temp <- bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                       name="Experimental\nCondition",
                       breaks=c("ctrl", "trt1", "trt2"),
                       labels=c("Control", "Treatment 1", "Treatment 2"))
save_outputs(temp, 'legends/legend title with custom colors', file_prefix="")

# A different data set
df1 <- data.frame(sex        = factor(c("Female","Female","Male","Male")),
                  time       = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
                  total_bill = c(13.53, 16.81, 16.24, 17.42))

# A basic graph
lp <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex)) + geom_line() + geom_point()

# Change the legend
temp <- lp + scale_shape_discrete(name  ="Payer",
                          breaks=c("Female", "Male"),
                          labels=c("Woman", "Man"))
save_outputs(temp, 'legends/line chart with custom legend', file_prefix="")

# Specify colour and shape
lp1 <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) + geom_line() + geom_point()

# Here's what happens if you just specify colour
lp1 + scale_colour_discrete(name  ="Payer",
                            breaks=c("Female", "Male"),
                            labels=c("Woman", "Man"))

# Specify both colour and shape
lp1 + scale_colour_discrete(name  ="Payer",
                            breaks=c("Female", "Male"),
                            labels=c("Woman", "Man")) +
      scale_shape_discrete(name  ="Payer",
                           breaks=c("Female", "Male"),
                           labels=c("Woman", "Man"))

pg <- PlantGrowth    # Copy data into new data frame
# Rename the column and the values in the factor
levels(pg$group)[levels(pg$group)=="ctrl"] <- "Control"
levels(pg$group)[levels(pg$group)=="trt1"] <- "Treatment 1"
levels(pg$group)[levels(pg$group)=="trt2"] <- "Treatment 2"
names(pg)[names(pg)=="group"]  <- "Experimental Condition"

# The end product
# pg
# weight Experimental Condition
#   4.17                   ctrl
#   5.58                   ctrl
#    ...
#   5.80                   trt2
#   5.26                   trt2

# Make the plot
ggplot(data=pg, aes(x=`Experimental Condition`, y=weight, fill=`Experimental Condition`)) + geom_boxplot()

# Title appearance
temp <- bp + theme(legend.title = element_text(colour="blue", size=16, face="bold"))
save_outputs(temp, 'legends/styled legend title', file_prefix="")

# Label appearance
temp <- bp + theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))
save_outputs(temp, 'legends/styled legend labels', file_prefix="")

bp + theme(legend.background = element_rect())
temp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
save_outputs(temp, 'legends/box around legend', file_prefix="")

temp <- bp + theme(legend.position="top")
save_outputs(temp, 'legends/legend on top', file_prefix="")

# Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
temp <- bp + theme(legend.position=c(.5, .5))
save_outputs(temp, 'legends/legend in middle', file_prefix="")

# Set the "anchoring point" of the legend (bottom-left is 0,0; top-right is 1,1)
# Put bottom-left corner of legend box in bottom-left corner of graph
temp <- bp + theme(legend.justification=c(0,0), legend.position=c(0,0))
save_outputs(temp, 'legends/legend in bottom left', file_prefix="")
# Put bottom-right corner of legend box in bottom-right corner of graph
temp <- bp + theme(legend.justification=c(1,0), legend.position=c(1,0))
save_outputs(temp, 'legends/legend in bottom right', file_prefix="")

# No outline
ggplot(data=PlantGrowth, aes(x=group, fill=group)) + geom_bar()

# Add outline, but slashes appear in legend
ggplot(data=PlantGrowth, aes(x=group, fill=group)) + geom_bar(colour="black")

# A hack to hide the slashes: first graph the bars with no outline and add the legend,
# then graph the bars again with outline, but with a blank legend.
ggplot(data=PlantGrowth, aes(x=group, fill=group)) + geom_bar() + geom_bar(colour="black", show_guide=FALSE)
