test_that(
  "icicle trace basically works", {
    icicle <- jsonlite::fromJSON(
      "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/uniformtext_icicle.json", simplifyVector = FALSE
    )
    expect_doppelganger(
      as_widget(icicle), 
      "uniformtext_icicle"
    )
})
