#!/bin/bash

# make sure we're in the plotly repo
pwd

# just for my curiousity
echo $TRAVIS_PULL_REQUEST
echo $TRAVIS_BRANCH
echo $GH_TOKEN

# -----------------------------------------------------------------------
# Note that during build, Travis does a checkout by commit which leaves a
# detached HEAD. Thus we have to checkout the appropriate branch
# -----------------------------------------------------------------------

echo "user, SHA1, label" >> code_commits.csv
# Travis CI environment variables docs --
# http://docs.travis-ci.com/user/ci-environment/
if ["${TRAVIS_PULL_REQUEST}"=="false"]; then
if ["${TRAVIS_BRANCH}"=="master"]; then
# If pushing directly to master, checkout master & build latest commit
git checkout master
echo "${USER}, `git rev-parse HEAD`, master" >> code_commits.csv
else
  echo "Unexpected case; not pushing anything to ropensci/plotly-test-table"
exit 0
fi
else
  git checkout $TRAVIS_BRANCH
echo "${USER}, `git rev-parse HEAD`, ${TRAVIS_BRANCH}" >> code_commits.csv
fi

git branch
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
