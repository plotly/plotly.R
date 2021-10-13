test_that("mimics the autoplot output", {
    # taken from https://fable.tidyverts.org/articles/fable.html
    p <- tsibble::tourism %>%
            filter(Region == "Melbourne") %>%
            model(
                ets = ETS(Trips ~ trend("A")),
                arima = ARIMA(Trips)
            ) %>%
            forecast(h = "5 years") %>%
            autoplot(tourism_melb)
    expect_doppelganger(ggplotly(p), "autoplot-fable") 
})