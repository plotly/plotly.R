# Contributing to plotly.js

## Opening issues

See the [opening issues template](https://github.com/ropensci/plotly/blob/master/.github/ISSUE_TEMPLATE.md)

## Development guidelines

If you'd like to contribute changes to plotly, we use [the GitHub flow](https://guides.github.com/introduction/flow/index.html) for proposing, submitting, reviewing, and accepting changes. If you aren't familiar with git and/or GitHub, we recommend studying these excellent free resources by [Hadley Wickham](http://r-pkgs.had.co.nz/git.html) and [Jenny Bryan](http://happygitwithr.com). We also prefer R coding style that adheres to <http://style.tidyverse.org/>.

If your pull request fixes a bug and/or implements a new feature, please [write a test via testthat](http://r-pkgs.had.co.nz/tests.html) to demonstrate it's working. Tests should generally check the return value of `plotly_build()`, but can also use **vdiffr**'s `expect_doppelganger()` to add a visual test! By default, `devtools::tests()` won't run the visual tests, but you *can* run visual tests on your machine via `Sys.setenv("VDIFFR" = "true"); devtools::test()`. That being said, false positives are likely to occur simply due to superfluous differences in your system environment, so we recommend running visual tests via docker.

### Running visual tests via docker

The **plotly** package ships with a [Dockerfile](https://github.com/ropensci/plotly/blob/master/Dockerfile) for running visual tests in a consistent and reproducible environment. You can build and run the docker container like so:

```shell
git clone https://github.com/ropensci/plotly.git
cd plotly
docker build -t plotly-vdiffr .
docker run --privileged -p 3838:3838 plotly-orca
```

This will launch a shiny app for inspecting and validating any visual differences. To see the shiny app, open your browser to <http://0.0.0.0/3838>. If there are differences that look 'good', you should validate them, then copy the new "baseline" figures over to your host machine (so that you can git add/commit/push the new baselines).

```shell
# assuming the most recent container you've run has the new baselines
docker cp $(docker ps -aq | head -n 1):/home/plotly/tests/figs ./tests
```

Say your changes produce changes in the corresponding svg of existing tests and/or you want to do write new tests (hooray!). In that case, you'll want to run `vdiffr::manage_cases()`, which should eventually launch a shiny app of "mismatched" and "new" to visually inspect before approving the changes.

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/ropensci/plotly/blob/master/CONDUCT.md) for more information.
