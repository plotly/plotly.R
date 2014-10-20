bp <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
save_outputs(bp, 'titles/no title')

bp1 <- bp + ggtitle("Plant growth")
save_outputs(bp1, 'titles/graph with title - 1')
# Equivalent to
bp2 <- bp + labs(title="Plant growth")
save_outputs(bp2, 'titles/graph with title - 2')

# If the title is long, it can be split into multiple lines with \n
# As to be recognized by Plotly: use <br> instead of \n
bp3 <- bp + ggtitle("Plant growth with\ndifferent treatments")
save_outputs(bp3, 'titles/graph with multi-line title')

# Reduce line spacing and use bold text
bp4 <- bp + ggtitle("Plant growth with\ndifferent treatments") +
     theme(plot.title = element_text(lineheight=.8, face="bold"))
save_outputs(bp4, 'titles/shorter and bold title')
