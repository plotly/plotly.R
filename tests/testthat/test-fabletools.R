test_that("mimics the autoplot output", {
    # taken from https://fable.tidyverts.org/articles/fable.html
    data <- tsibble::tourism %>%
            filter(Region == "Melbourne") %>%
            `[`(, c("Quarter", "Trips", "Region")) %>%
            distinct(Quarter, .keep_all = TRUE) %>%
            as_tsibble(key = Region) 
    p <- data %>%
            model(
                ets = ETS(Trips ~ trend("A")),
            ) %>%
            forecast(h = "5 years") %>%
            autoplot(data)
    expect_doppelganger(ggplotly(p), "autoplot-fable") 
})