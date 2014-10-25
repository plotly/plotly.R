library(reshape2)
tips
# total_bill   tip    sex smoker  day   time size
#      16.99  1.01 Female     No  Sun Dinner    2
#      10.34  1.66   Male     No  Sun Dinner    3
#      21.01  3.50   Male     No  Sun Dinner    3
#        ...
#      22.67  2.00   Male    Yes  Sat Dinner    2
#      17.82  1.75   Male     No  Sat Dinner    2
#      18.78  3.00 Female     No Thur Dinner    2

library(ggplot2)
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
save_outputs(sp, 'facets/no features', file_prefix="")

# Divide by levels of "sex", in the vertical direction
sp1 <- sp + facet_grid(sex ~ .)
save_outputs(sp1, 'facets/vertical facet_grid', file_prefix="")

# Divide by levels of "sex", in the horizontal direction
sp2 <- sp + facet_grid(. ~ sex)
save_outputs(sp2, 'facets/horizontal facet_grid', file_prefix="")

# Divide with "sex" vertical, "day" horizontal
sp3 <- sp + facet_grid(sex ~ day)
save_outputs(sp3, 'facets/2x4 facet_grid - sex-vertical day-horizontal', file_prefix="")

# Divide by day, going horizontally and wrapping with 2 columns
sp4 <- sp + facet_wrap( ~ day, ncol=2)
save_outputs(sp4, 'facets/facet_wrap', file_prefix="")

sp5 <- sp + facet_grid(sex ~ day) +
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="red", fill="#CCCCFF"))
save_outputs(sp5, 'facets/facet label colors', file_prefix="")

mf_labeller <- function(var, value){
    value <- as.character(value)
    if (var=="sex") {
        value[value=="Female"] <- "Woman"
        value[value=="Male"]   <- "Man"
    }
    return(value)
}

sp6 <- sp + facet_grid(. ~ sex, labeller=mf_labeller)
save_outputs(sp6, 'facets/custom facet labels', file_prefix="")

tips2 <- tips
levels(tips2$sex)[levels(tips2$sex)=="Female"] <- "Woman"
levels(tips2$sex)[levels(tips2$sex)=="Male"]   <- "Man"
# total_bill  tip   sex smoker day   time size
#      16.99 1.01 Woman     No Sun Dinner    2
#      10.34 1.66   Man     No Sun Dinner    3
#       ...

sp2 <- ggplot(tips2, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp2 + facet_grid(. ~ sex)

# A histogram of bill sizes
hp <- ggplot(tips, aes(x=total_bill)) + geom_histogram(binwidth=2,colour="white")

# Histogram of total_bill, divided by sex and smoker
sp7 <- hp + facet_grid(sex ~ smoker)
save_outputs(sp7, 'facets/histogram facets', file_prefix="")

# Same as above, with scales="free_y"
sp8 <- hp + facet_grid(sex ~ smoker, scales="free_y")
save_outputs(sp8, 'facets/histogram facets with free_y', file_prefix="")

# With panels that have the same scaling, but different range (and therefore different physical sizes)
sp9 <- hp + facet_grid(sex ~ smoker, scales="free", space="free")
save_outputs(sp9, 'facets/histogram facets with scales=free', file_prefix="")
