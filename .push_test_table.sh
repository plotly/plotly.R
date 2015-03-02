#!/bin/bash

# make sure we're in the plotly repo & install it
pwd
Rscript -e "devtools::install()"

# just for my curiousity
echo $TRAVIS_PULL_REQUEST
echo $TRAVIS_BRANCH

# -----------------------------------------------------------------------
# When pushing to a pull request on GitHub, Travis does two builds:
# (1) One for the pull request itself. In this case, $TRAVIS_PULL_REQUEST 
# is 'false' and $TRAVIS_BRANCH contains the branch name
# (2) One to *simulate* a merge with master. In this case, $TRAVIS_PULL_REQUEST
# is the pull ID number and $TRAVIS_BRANCH = 'master'
# 
# Read more about Travis environment variables --
# http://docs.travis-ci.com/user/ci-environment/
#
# Also, Travis does a checkout by commit which leaves a detached HEAD. 
# Thus we have to checkout the appropriate branch
# -----------------------------------------------------------------------

# Only build test table if $TRAVIS_PULL_REQUEST is false
[ "${TRAVIS_PULL_REQUEST}" != "false" ] && exit 0

MASTER_SHA1=`git checkout master && git rev-parse HEAD`
git checkout $TRAVIS_BRANCH
echo "user, SHA1, label" >> code_commits.csv
echo "${USER}, ${MASTER_SHA1}, master" >> code_commits.csv
echo "${USER}, `git rev-parse HEAD`, ${TRAVIS_BRANCH}" >> code_commits.csv

cd ..
git clone https://github.com/ropensci/plotly-test-table.git
cd plotly-test-table
git checkout gh-pages
git config --global user.name "Travis Bot"
git config --global user.email "cpsievert1@gmail.com"

mv ../plotly/code_commits.csv .
cat code_commits.csv
make

# add, commit, push to gh-pages branch of plotly-test-table
git add tables/*/*.html data/*/*.png
git commit -a -m "Travis build number ${TRAVIS_BUILD_NUMBER} of ${TRAVIS_REPO_SLUG}"
# This post explains how this works -- http://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html
GH_REPO="@github.com/ropensci/plotly-test-table.git"
FULL_REPO="https://${GH_TOKEN}${GH_REPO}"


# remove comment when we're ready to deploy
# git push --force --quiet $FULL_REPO master:gh-pages
