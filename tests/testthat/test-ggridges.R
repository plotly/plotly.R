skip_if_not_installed("ggridges")
library(ggridges)

test_that(
  desc = "ggridges basic ridgelines", 
  code = {
    
    # simple ridge plot
    data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, 3, 4, 2))
    p <- ggplot(data, aes(x, y, height = height)) + geom_ridgeline()
    
    p2 <- ggplotly(p)
    
    expect_doppelganger(p2, 'basic_ridgeline')
    
    
    # Negative height
    data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, -1, 3, 2))
    plot_base <- ggplot(data, aes(x, y, height = height))
    
    ## Negative height cut off
    p <- plot_base + geom_ridgeline()
    
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'negative_height_cut')
    
    
    ## Negative height allowed
    p <- plot_base + geom_ridgeline(min_height = -2)
    
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'negative_height_retained')
    
    
    # Multiple ridgelines at same time 
    d <- data.frame(
      x = rep(1:5, 3),
      y = c(rep(0, 5), rep(1, 5), rep(2, 5)),
      height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1)
    )
    
    p <- ggplot(d, aes(x, y, height = height, group = y)) + 
      geom_ridgeline(fill = "lightblue")
    
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'multiple_ridgelines')
    
    # stat = identity (works)
    p <- ggplot(d, aes(x, y, height = height, group = y)) + 
      geom_density_ridges(stat = "identity", scale = 1)
    
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'stat_identity')
  }
)

test_that(
  desc = "ggridges density_ridgeline", 
  code = {
    
    # Density ridgeline plots
    
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_density_ridges()
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'density_ridgeline')
    
    # geom_density_ridges2 (closed polygon)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges2()
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'density_ridgeline2')
    
    # Grouping aesthetic
    # modified dataset that represents species as a number
    iris_num <- transform(iris, Species_num = as.numeric(Species))
    
    p <- ggplot(iris_num,
                aes(x = Sepal.Length,
                    y = Species_num,
                    group = Species_num)) + 
      geom_density_ridges()
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'numeric_grouping')
    
    # Cutting trailing tails (works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
      geom_density_ridges(rel_min_height = 0.01)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'cutting_tails')
    
    # Non-overlapping ridges (Works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_density_ridges(scale = 0.9)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'overlapping_none')
    
    
    # Exactly touching (Works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
      geom_density_ridges(scale = 1)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'overlapping_touching')
    
    
    # scale = 5, substantial overlap (Works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_density_ridges(scale = 5)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'overlapping_lot')
    
    
    # Panel scaling (Works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
      geom_density_ridges(scale = 1) + facet_wrap(~Species)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'overlapping_facet_touching')
    
  }
)
    
test_that(
  desc = "ggridges fill colours", 
  code = {
    
    # Varying fill colors along the x axis
    
    # Example 1 (Works, but extra legend that is not shown in ggridges)
    d <- data.frame(
      x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
      y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
      height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
    
    p <- ggplot(d, aes(x, y, height = height, group = y, fill = factor(x+y))) +
      geom_ridgeline_gradient() +
      scale_fill_viridis_d(direction = -1, guide = "none")
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'varying_fill_colours')
    
    # geom_density_ridges_gradient (Doesn't work)
    # p <- ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = Month, fill = stat(x))) +
    #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    #   scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
    #   labs(title = 'Temperatures in Lincoln NE in 2016')
    # ggplotly(p) # gets stuck
    
    # Stats
    
    ## Quantile lines and coloring by quantiles or probabilities (Works)
    
    # quantile multiple lines
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      stat_density_ridges(quantile_lines = TRUE)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'quantile_lines_multi')
    
    # quantile single line
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'quantile_lines_1')
    
    # quantile by cut points
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      stat_density_ridges(quantile_lines = TRUE,
                          quantiles = c(0.025, 0.975),
                          alpha = 0.7)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'quantile_cut_points')
    
    
    ## Colour by quantile
    # warning since ggridges uses stat(quantile) 
    suppressWarnings(
      p <- ggplot(iris, aes(x=Sepal.Length, y=Species, fill = factor(stat(quantile)))) +
        stat_density_ridges(
          geom = "density_ridges_gradient", calc_ecdf = TRUE,
          quantiles = 4, quantile_lines = TRUE
        ) +
        scale_fill_viridis_d(name = "Quartiles")
    )
    
    suppressWarnings(
      p2 <- ggplotly(p)
    )
    expect_doppelganger(p2, 'quantile_colouring')
    
    
    # highglight tails of distributions (works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE,
        quantiles = c(0.025, 0.975)
      ) +
      scale_fill_manual(
        name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
        labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
      )
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'quantile_colouring_tails_only')
    
    # mapping prob onto colour (doesn't work)
    # p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    #   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
    #   scale_fill_viridis_c(name = "Tail probability", direction = -1)
    # ggplotly(p)
    
    
  }
)
    
    
test_that(
  desc = "ggridges points", 
  code = {
    
    set.seed(123) # make jittering reproducible
    # jittering points (works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_density_ridges(jittered_points = TRUE)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'jittering points')
    
    # raincloud effect (works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_density_ridges(
        jittered_points = TRUE, position = "raincloud",
        alpha = 0.7, scale = 0.9
      )
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'raincloud_effect')
    
    # rug effect (doesn't work, point shape not taken into account)
    # p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
    #   geom_density_ridges(
    #     jittered_points = TRUE,
    #     position = position_points_jitter(width = 0.05, height = 0),
    #     point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    #   )
    
    
    # styling points 
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
      geom_density_ridges(
        aes(point_color = Species, point_fill = Species, point_shape = Species),
        alpha = .2, point_alpha = 1, jittered_points = TRUE
      ) +
      scale_point_color_hue(l = 40) +
      scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'styling_points')
    
    # styling points 2
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
      geom_density_ridges(
        aes(point_shape = Species, point_fill = Species, point_size = Petal.Length), 
        alpha = .2, point_alpha = 1, jittered_points = TRUE
      ) +
      scale_point_color_hue(l = 40) + scale_point_size_continuous(range = c(0.5, 4)) +
      scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'styling_points2')
    
    
    # aesthetics for vertical line (works) (might need to check line on top of points)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_density_ridges(
        jittered_points = TRUE, quantile_lines = TRUE, scale = 0.9, alpha = 0.7,
        vline_size = 1, vline_color = "red",
        point_size = 0.4, point_alpha = 1,
        position = position_raincloud(adjust_vlines = TRUE)
      )
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'raincloud_vertical_line_points')
    
  }
)
    
    
test_that(
  desc = "ggridges alternate stats", 
  code = {
    
    ## stat_density_ridges (works)
    suppressWarnings({
      p <- ggplot(iris, aes(x = Sepal.Length, y = Species, height = stat(density))) + 
        geom_density_ridges(stat = "density")
      
      p2 <- ggplotly(p)
    })
    expect_doppelganger(p2, 'stat_density')
    
    
    skip_if_not_installed("dplyr")
    iris_densities <- iris %>%
      dplyr::group_by(Species) %>%
      dplyr::group_modify(~ ggplot2:::compute_density(.x$Sepal.Length, NULL)) %>%
      dplyr::rename(Sepal.Length = x)
    
    p <- ggplot(iris_densities, aes(x = Sepal.Length, y = Species, height = density)) + 
      geom_density_ridges(stat = "identity")
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'manual_densities_stat_identity')
    
    ## histograms (works)
    p <- ggplot(iris, aes(x = Sepal.Length, y = Species, height = stat(density))) + 
      geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE)
    p2 <- ggplotly(p)
    expect_doppelganger(p2, 'histogram_ridges')
    
  }
)
    
