# Contributing to plotly.js

## Opening issues

See the [opening issues template](https://github.com/ropensci/plotly/blob/master/.github/ISSUE_TEMPLATE.md)

## Development guidelines

If you'd like to contribute changes to plotly, we use [the GitHub flow](https://guides.github.com/introduction/flow/index.html) for proposing, submitting, reviewing, and accepting changes. If you aren't familiar with git and/or GitHub, we recommend studying these excellent free resources by [Hadley Wickham](http://r-pkgs.had.co.nz/git.html) and [Jenny Bryan](http://happygitwithr.com). We also prefer R coding style that adheres to <http://style.tidyverse.org/>.

If your pull request fixes a bug, or implements a new feature, please [write a test via testthat](http://r-pkgs.had.co.nz/tests.html) to demonstrate it's working. The testing suite can optionally leverage **vdiffr**'s `expect_doppelganger()` to monitor changes via svg -- to make sure those those tests run, do:

```r
Sys.setenv("USE_VDIFFR" = "true")
devtools::test()
```

Say your changes produce changes in the corresponding svg of existing tests and/or you want to do write new tests (hooray!). In that case, you'll want to run `vdiffr::manage_cases()`, which should eventually launch a shiny app of "mismatched" and "new" to visually inspect before approving the changes.

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/ropensci/plotly/blob/master/CONDUCT.md) for more information.
