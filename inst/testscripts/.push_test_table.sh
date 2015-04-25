#!/bin/bash

# exit on error
set -e

# -----------------------------------------------------------------------
# When pushing to a pull request on GitHub, Travis does two builds:
# (1) One for the pull request itself. In this case, $TRAVIS_PULL_REQUEST 
# is 'false' and $TRAVIS_BRANCH contains the branch name
# (2) One to *simulate* a merge with master. In this case, $TRAVIS_PULL_REQUEST
# is the pull ID number and $TRAVIS_BRANCH = 'master'
# 
# Read more about Travis environment variables --
# http://docs.travis-ci.com/user/ci-environment/
# -----------------------------------------------------------------------

# Only build test table if $TRAVIS_PULL_REQUEST is false
[ "${TRAVIS_PULL_REQUEST}" != "false" ] && exit 0

git config --global user.name "cpsievert"
git config --global user.email "cpsievert1@gmail.com"

# Since Travis does `git clone --branch=$TRAVIS_BRANCH --depth=50`, 
# we can't simply `git checkout master`. As far as I can tell, we are
# forced to re-clone -- https://gist.github.com/cpsievert/698c7f1404f972782e71

cd ..
rm -rf plotly/
git clone https://github.com/ropensci/plotly.git
cd plotly
echo "user,SHA1,label" >> ../code_commits.csv
echo "ropensci,`git rev-parse HEAD`,master" >> ../code_commits.csv
git checkout $TRAVIS_BRANCH
echo "ropensci,`git rev-parse HEAD`,${TRAVIS_BRANCH}" >> ../code_commits.csv
Rscript -e "devtools::install()"

cd ..
git clone https://github.com/ropensci/plotly-test-table.git
cd plotly-test-table
git checkout gh-pages

mv ../code_commits.csv .
cat code_commits.csv
touch table.R
make

# add, commit, push to gh-pages branch of plotly-test-table
./git-add.sh
git commit -a -m "Travis build number ${TRAVIS_BUILD_NUMBER} of ${TRAVIS_REPO_SLUG}"
# This post explains how this works -- http://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html
GH_REPO="@github.com/ropensci/plotly-test-table.git"
FULL_REPO="https://${GH_TOKEN}${GH_REPO}"
git pull $FULL_REPO gh-pages
git push $FULL_REPO gh-pages


# post some comments on the pull request
Rscript ../plotly/inst/testscripts/comment.R ${TRAVIS_PULL_REQUEST} ${TRAVIS_COMMIT}
