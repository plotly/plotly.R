if(require(treemapify)){
  test_that("`geom_treemap` gets converted correctly", {
    p <- ggplot(G20, aes(area = gdp_mil_usd, fill = hdi)) +
      geom_treemap()
    expect_doppelganger(ggplotly(p), "tree-map")
  } )
  test_that("`geom_treemap`/`geom_treemap_text` gets converted correctly", {
    p <- ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label= country)) +
      geom_treemap() + 
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)

    expect_doppelganger(ggplotly(p), "tree-map-text")
  })
  test_that("`treemap_subgroup` gets converted correctly", {
    p <- ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country,
                subgroup = region)) +
            geom_treemap() +
            geom_treemap_subgroup_border() +
            geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                       "black", fontface = "italic", min.size = 0) +
            geom_treemap_text(colour = "white", place = "topleft", reflow = T)
    expect_doppelganger(ggplotly(p), "subgroup")
  })
  test_that("multiple `subgroup`s get converted correctly", {
    p <- ggplot(G20, aes(area = 1, label = country, subgroup = hemisphere,
                          subgroup2 = region, subgroup3 = econ_classification)) +
            geom_treemap() +
            geom_treemap_subgroup3_border(colour = "blue", size = 1) +
            geom_treemap_subgroup2_border(colour = "white", size = 3) +
            geom_treemap_subgroup_border(colour = "red", size = 5) +
            geom_treemap_subgroup_text(
              place = "middle",
              colour = "red",
              alpha = 0.5,
              grow = T
            ) +
            geom_treemap_subgroup2_text(
              colour = "white",
              alpha = 0.5,
              fontface = "italic"
            ) +
            geom_treemap_subgroup3_text(place = "top", colour = "blue", alpha = 0.5) +
            geom_treemap_text(colour = "white", place = "middle", reflow = T)

    expect_doppelganger(ggplotly(p), "mult-subgroups")
  })
  
}