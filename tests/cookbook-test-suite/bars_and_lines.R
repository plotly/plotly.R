df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(14.89, 17.23))
#   time total_bill
#  Lunch      14.89
# Dinner      17.23

# Very basic bar graph
g <- ggplot(data=df, aes(x=time, y=total_bill)) + geom_bar(stat="identity")
save_outputs(g, "bars-and-lines/basic-bar", file_prefix="")

# Map the time of day to different fill colors. These both have the same result.
g <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) + geom_bar(stat="identity")
save_outputs(g, "bars-and-lines/basic-bar-fill-colors-1", file_prefix="")
g <- ggplot(data=df, aes(x=time, y=total_bill)) + geom_bar(aes(fill=time), stat="identity")
save_outputs(g, "bars-and-lines/basic-bar-fill-colors-2", file_prefix="")

# Add a black outline
g <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) + geom_bar(colour="black", stat="identity")
save_outputs(g, "bars-and-lines/basic-bar-fill-colors-black-outline", file_prefix="")

# No legend, since the information is redundant
g <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE)
save_outputs(g, "bars-and-lines/basic-bar-fill-colors-black-outline-no-legend", file_prefix="")

library(reshape2)
tips
# total_bill   tip    sex smoker  day   time size
#      16.99  1.01 Female     No  Sun Dinner    2
#      10.34  1.66   Male     No  Sun Dinner    3
#      21.01  3.50   Male     No  Sun Dinner    3
#  ... <244 total rows> ...
#      22.67  2.00   Male    Yes  Sat Dinner    2
#      17.82  1.75   Male     No  Sat Dinner    2
#      18.78  3.00 Female     No Thur Dinner    2

# Bar graph of counts
g <- ggplot(data=tips, aes(x=day)) + geom_bar(stat="bin")
save_outputs(g, "bars-and-lines/bar-graph-of-counts", file_prefix="")

# Equivalent to this, since stat="bin" is the default:
g <- ggplot(data=tips, aes(x=day)) + geom_bar()
save_outputs(g, "bars-and-lines/bar-graph-of-counts-2", file_prefix="")

# Basic line graph. These both have the same result.
g <- ggplot(data=df, aes(x=time, y=total_bill, group=1)) + geom_line()
save_outputs(g, "bars-and-lines/basic-line", file_prefix="")
g <- ggplot(data=df, aes(x=time, y=total_bill)) + geom_line(aes(group=1))
save_outputs(g, "bars-and-lines/basic-line-2", file_prefix="")

# Add points
g <- ggplot(data=df, aes(x=time, y=total_bill, group=1)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/basic-line-with-points", file_prefix="")

# Change color of both line and points
# Change line type and point type, and use thicker line and larger points
# Change points to circles with white fill
g <- ggplot(data=df, aes(x=time, y=total_bill, group=1)) +
    geom_line(colour="red", linetype="dotted", size=1.5) +
    geom_point(colour="red", size=4, shape=21, fill="white")
save_outputs(g, "bars-and-lines/basic-dashed-line-with-colors", file_prefix="")

# Change the y-range to go from 0 to the maximum value in the total_bill column,
# and change axis labels
g <- ggplot(data=df, aes(x=time, y=total_bill, group=1)) + geom_line() + geom_point() +
    ylim(0, max(df$total_bill)) +
    xlab("Time of day") + ylab("Total bill") +
    ggtitle("Average bill for 2 people")
save_outputs(g, "bars-and-lines/basic-line-fully-styled", file_prefix="")

df1 <- data.frame(sex       = factor(c("Female","Female","Male","Male")),
                 time       = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(13.53, 16.81, 16.24, 17.42))
#    sex   time total_bill
# Female  Lunch      13.53
# Female Dinner      16.81
#   Male  Lunch      16.24
#   Male Dinner      17.42

# Stacked bar graph -- this is probably not what you want
g <- ggplot(data=df1, aes(x=time, y=total_bill, fill=sex)) + geom_bar(stat="identity")
save_outputs(g, "bars-and-lines/multi-var-stacked-bar", file_prefix="")

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
g <- ggplot(data=df1, aes(x=time, y=total_bill, fill=sex)) + geom_bar(stat="identity", position=position_dodge())
save_outputs(g, "bars-and-lines/multi-var-grouped-bar", file_prefix="")
g <- ggplot(data=df1, aes(x=time, y=total_bill, fill=sex)) + geom_bar(stat="identity", position=position_dodge(), colour="black")
save_outputs(g, "bars-and-lines/multi-var-grouped-bar-black-outline", file_prefix="")

# Change colors
g <- ggplot(data=df1, aes(x=time, y=total_bill, fill=sex)) + geom_bar(stat="identity", position=position_dodge(), colour="black") +
    scale_fill_manual(values=c("#999999", "#E69F00"))
save_outputs(g, "bars-and-lines/multi-var-grouped-bar-colored", file_prefix="")

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
g <- ggplot(data=df1, aes(x=sex, y=total_bill, fill=time)) + geom_bar(stat="identity", position=position_dodge(), colour="black")
save_outputs(g, "bars-and-lines/multi-var-grouped-bar-reversed-vars", file_prefix="")

# Basic line graph with points
g <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/basic-line-with-points", file_prefix="")

# Map sex to color
g <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, colour=sex)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/basic-line-with-mapped-colors", file_prefix="")

# Map sex to different point shape, and use larger points
g <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/basic-line-with-symbols", file_prefix="")

# Use thicker lines and larger points, and hollow white-filled points
g <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
    geom_line(size=1.5) +
    geom_point(size=3, fill="white") +
    scale_shape_manual(values=c(22,21))
save_outputs(g, "bars-and-lines/basic-line-with-points-fully-styled", file_prefix="")

g <- ggplot(data=df1, aes(x=sex, y=total_bill, group=time, shape=time, color=time)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/basic-line-swapped-vars", file_prefix="")

# A bar graph
g <- ggplot(data=df1, aes(x=time, y=total_bill, fill=sex)) +
    geom_bar(colour="black", stat="identity",
             position=position_dodge(),
             size=.3) +                        # Thinner lines
    scale_fill_hue(name="Sex of payer") +      # Set legend title
    xlab("Time of day") + ylab("Total bill") + # Set axis labels
    ggtitle("Average bill for 2 people") +  # Set title
    theme_bw()
save_outputs(g, "bars-and-lines/finished-bar-bw-theme", file_prefix="")

# A line graph
g <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) +
    geom_line(aes(linetype=sex), size=1) +     # Set linetype by sex
    geom_point(size=3, fill="white") +         # Use larger points, fill with white
    ylim(0, max(df1$total_bill)) +             # Set y range
    scale_colour_hue(name="Sex of payer",      # Set legend title
                     l=30)  +                  # Use darker colors (lightness=30)
    scale_shape_manual(name="Sex of payer",
                       values=c(22,21)) +      # Use points with a fill color
    scale_linetype_discrete(name="Sex of payer") +
    xlab("Time of day") + ylab("Total bill") + # Set axis labels
    ggtitle("Average bill for 2 people") +  # Set title
    theme_bw() +
    theme(legend.position=c(.7, .4)) # Position legend inside
                                    # This must go after theme_bw
save_outputs(g, "bars-and-lines/finished-line", file_prefix="")

dfn <- read.table(header=T, text="
supp dose length
  OJ  0.5  13.23
  OJ  1.0  22.70
  OJ  2.0  26.06
  VC  0.5   7.98
  VC  1.0  16.77
  VC  2.0  26.14
")

g <- ggplot(data=dfn, aes(x=dose, y=length, group=supp, colour=supp)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/line-continuous-numerical-x-axis", file_prefix="")

# Copy the data frame and convert dose to a factor
dfn2 <- dfn
dfn2$dose <- factor(dfn2$dose)
g <- ggplot(data=dfn2, aes(x=dose, y=length, group=supp, colour=supp)) + geom_line() + geom_point()
save_outputs(g, "bars-and-lines/line-continuous-categorical-x-axis", file_prefix="")

# Use the original data frame, but put factor() directly in the plot specification

## TODO: Uncomment when Plotly supports this
## g <- ggplot(data=dfn, aes(x=factor(dose), y=length, group=supp, colour=supp)) + geom_line() + geom_point()
## save_outputs(g, "bars-and-lines/line-continuous-categorical-x-axis-with-factor", file_prefix="")

# Use dfn2 from above
g <- ggplot(data=dfn2, aes(x=dose, y=length, fill=supp)) + geom_bar(stat="identity", position=position_dodge())
save_outputs(g, "bars-and-lines/bar-categorical-numerical-labels", file_prefix="")

# Use the original data frame, but put factor() directly in the plot specification
g <- ggplot(data=dfn, aes(x=factor(dose), y=length, fill=supp)) + geom_bar(stat="identity", position=position_dodge())
save_outputs(g, "bars-and-lines/bar-categorical-numerical-labels-with-factor", file_prefix="")

# Add title, narrower bars, gray fill, and change axis labels
g <- ggplot(data=df, aes(x=time, y=total_bill, fill=time)) +
    geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") +
    guides(fill=FALSE) +
    xlab("Time of day") + ylab("Total bill") +
    ggtitle("Average bill for 2 people")
save_outputs(g, "bars-and-lines/basic-bar-fully-styled", file_prefix="")
