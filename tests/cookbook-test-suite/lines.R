# Some sample data
df <- read.table(header=T, text="
     cond result
  control     10
treatment   11.5
")

# Basic bar plot
bp <- ggplot(df, aes(x=cond, y=result)) + geom_bar(position=position_dodge())
bp

# Add a horizontal line
temp <- bp + geom_hline(aes(yintercept=12))
save_outputs(temp, "lines/basic horizontal line", file_prefix="")

# Make the line red and dashed
temp <- bp + geom_hline(aes(yintercept=12), colour="#990000", linetype="dashed")
save_outputs(temp, "lines/dashed red line", file_prefix="")

# Draw separate hlines for each bar. First add another column to df
df$hline <- c(9,12)
#      cond result hline
#   control   10.0     9
# treatment   11.5    12

# Need to re-specify bp, because the data has changed
bp <- ggplot(df, aes(x=cond, y=result)) + geom_bar(position=position_dodge())

# Draw with separate lines for each bar
bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")

# Make the lines narrower
bp + geom_errorbar(width=0.5, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")


# Can get the same result, even if we get the hline values from a second data frame
# Define data frame with hline
df.hlines <- data.frame(cond=c("control","treatment"), hline=c(9,12))
#      cond hline
#   control     9
# treatment    12

# The bar graph are from df, but the lines are from df.hlines
bp + geom_errorbar(data=df.hlines, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")

df <- read.table(header=T, text="
     cond group result hline
  control     A     10     9
treatment     A   11.5    12
  control     B     12     9
treatment     B     14    12
")

# Define basic bar plot
bp <- ggplot(df, aes(x=cond, y=result, fill=group)) + geom_bar(position=position_dodge())
bp

# The error bars get plotted over one another -- there are four but it looks like two
bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), linetype="dashed")

df <- read.table(header=T, text="
     cond group result hline
  control     A     10    11
treatment     A   11.5    12
  control     B     12  12.5
treatment     B     14    15
")

# Define basic bar plot
bp <- ggplot(df, aes(x=cond, y=result, fill=group)) + geom_bar(position=position_dodge())
bp

bp + geom_errorbar(aes(y=hline, ymax=hline, ymin=hline), linetype="dashed", position=position_dodge())

df <- read.table(header=T, text="
      cond xval yval
   control 11.5 10.8
   control  9.3 12.9
   control  8.0  9.9
   control 11.5 10.1
   control  8.6  8.3
   control  9.9  9.5
   control  8.8  8.7
   control 11.7 10.1
   control  9.7  9.3
   control  9.8 12.0
 treatment 10.4 10.6
 treatment 12.1  8.6
 treatment 11.2 11.0
 treatment 10.0  8.8
 treatment 12.9  9.5
 treatment  9.1 10.0
 treatment 13.4  9.6
 treatment 11.6  9.8
 treatment 11.5  9.8
 treatment 12.0 10.6
")

library(ggplot2)

# The basic scatterplot
sp <- ggplot(df, aes(x=xval, y=yval, colour=cond)) + geom_point()


# Add a horizontal line
temp <- sp + geom_hline(aes(yintercept=10))
save_outputs(temp, "lines/hline on scatter", file_prefix="")

# Add a red dashed vertical line
temp <- sp + geom_hline(aes(yintercept=10)) + geom_vline(aes(xintercept=11.5), colour="#BB0000", linetype="dashed")
save_outputs(temp, "lines/hline n vline on scatter", file_prefix="")

# Add colored lines for the mean xval of each group
temp <- sp + geom_hline(aes(yintercept=10)) +
     geom_line(stat="vline", xintercept="mean")
save_outputs(temp, "lines/colored lines on scatter", file_prefix="")

# Facet, based on cond
spf <- sp + facet_grid(. ~ cond)
spf

# Draw a horizontal line in all of the facets at the same value
temp <- spf + geom_hline(aes(yintercept=10))
save_outputs(temp, "lines/hline on facets", file_prefix="")

df.vlines <- data.frame(cond=levels(df$cond), xval=c(10,11.5))
#      cond xval
#   control 10.0
# treatment 11.5

spf + geom_hline(aes(yintercept=10)) +
      geom_vline(aes(xintercept=xval), data=df.vlines,
                    colour="#990000", linetype="dashed")

spf + geom_hline(aes(yintercept=10)) +
      geom_line(stat="vline", xintercept="mean")
