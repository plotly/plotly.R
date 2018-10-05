# Contributing to plotly.js

## Opening issues

See the [opening issues template](https://github.com/ropensci/plotly/blob/master/.github/ISSUE_TEMPLATE.md)

## Development guidelines

If you'd like to contribute changes to plotly, we use [the GitHub flow](https://guides.github.com/introduction/flow/index.html) for proposing, submitting, reviewing, and accepting changes. If you aren't familiar with git and/or GitHub, we recommend studying these excellent free resources by [Hadley Wickham](http://r-pkgs.had.co.nz/git.html) and [Jenny Bryan](http://happygitwithr.com). We also prefer R coding style that adheres to <http://style.tidyverse.org/>.

If your pull request fixes a bug and/or implements a new feature, please [write a test via testthat](http://r-pkgs.had.co.nz/tests.html) to demonstrate it's working. Tests should generally check the return value of `plotly_build()`, but can also use **vdiffr**'s `expect_doppelganger()` to add a visual test! By default, `devtools::tests()` won't run the visual tests, but you *can* run visual tests on your machine via `Sys.setenv("VDIFFR" = "true"); devtools::test()`. That being said, false positives are likely to occur simply due to superfluous differences in your system environment, so we recommend running visual tests via docker.

### Running visual tests via docker

To ensure a consistent and reproducible environment, visual tests should be run against the [cpsievert/plotly-orca](https://hub.docker.com/r/cpsievert/plotly-orca/) docker image. If you add a new visual test and/or expect any differences, run a container like so:

```shell
git clone https://github.com/ropensci/plotly.git
cd plotly
docker run -v $(pwd):/home/plotly --privileged -p 3838:3838 cpsievert/plotly-orca
```

This will launch a shiny app for inspecting and validating any visual differences. To see the shiny app, open your browser to <http://0.0.0.0/3838>. If there are differences that look 'good', you should validate them via the shiny app. This will automatically copy over the new "baseline" figures over to your host machine (so that you can git add/commit/push the new baselines).

If, for some reason, you want to just run the visual tests to see if they'll pass, do:

```shell
docker run -e VMODE="ci" -v $(pwd):/home/plotly --privileged cpsievert/plotly-orca
```

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/ropensci/plotly/blob/master/CONDUCT.md) for more information.
