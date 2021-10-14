if(require(plotROC)){
    set.seed(555)
    D.ex <- rbinom(200, size = 1, prob = .5)
    M1 <- rnorm(200, mean = D.ex, sd = .65)
    M2 <- rnorm(200, mean = D.ex, sd = 1.5)

    test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)
    longtest <- melt_roc(test, "D", c("M1", "M2"))
    p <- ggplot(test, aes(d = D, m = M1))

    
    test_that("Plots `geom_roc` with more labels", {
        expect_doppelganger(ggplotly(p + geom_roc(n.cuts = 50)), "geom-roc-more-labs")
    })
    test_that("Plots `geom_roc` with no cuts", {          
        expect_doppelganger(ggplotly(p +  geom_roc(n.cuts = 0)), "geom-roc-no-cuts") 
    })
    test_that("Plots `geom_roc`/`geom_rocci` with no labels", {          
        expect_doppelganger(ggplotly(p +  geom_roc(labels = FALSE)), "geom-roc-no-labs") 
    })
    test_that("Plots `geom_roc`/`geom_rocci` with bigger labels and more decimal precision", {
        # they use the same mechanism undeer the hood no need to re-chek for geom_rocci
        expect_doppelganger(ggplotly(p+geom_roc(labelsize = 5, labelround = 2)), "geom-roc-big-labs")
        expect_failure(expect_doppelganger(ggplotly(p+geom_roc(labelsize = 5, labelround = 2)), "geom-roc"))
    })
    test_that("Plots `geom_roc` as expected", {
        expect_doppelganger(ggplotly(p + geom_roc()), "geom-roc")
    })
    test_that("Plots `geom_rocci` as expected", {
        expect_doppelganger(ggplotly(p + geom_rocci()), "geom-rocci")
    })
    p <- ggplot(longtest, aes(d = D, m = M, color = name))
    test_that("Can handle multiple `geom_roc`", {
        expect_doppelganger(ggplotly(p + geom_roc() ), "geom-roc-mult")
    } )
    test_that("Can handle multiple `geom_rocci`", {
        expect_doppelganger(ggplotly(p + geom_rocci() ), "geom-rocci-mult")
    } )
    
}