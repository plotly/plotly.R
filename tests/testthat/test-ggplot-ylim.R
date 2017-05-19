context("ggplot ylim")

# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_%28ggplot2%29/

df <- data.frame(
  time = factor(c("Lunch","Dinner"), levels = c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)

gg.ylim <- 
  ggplot(data = df, aes(x = time, y = total_bill, group = 1)) +
  geom_line() +
  ylim(0, max(df$total_bill)) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("ylim-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

test_that("ylim is respected for 1 trace", {
  info <- expect_traces(gg.ylim, 1, "one-trace")
  expect_equivalent(min(info$layout$yaxis$tickvals), 0)
  expect_identical(info$data[[1]]$showlegend, FALSE)
})
