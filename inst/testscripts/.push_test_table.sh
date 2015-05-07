#!/bin/bash

# exit on error
set -e

# -----------------------------------------------------------------------
# Travis does two types of builds:
#
# (1) A so-called "push". This essentially does a checkout on the most 
# recent commit of the pull request, but *doesn't* merge with master.
# In this case, $TRAVIS_PULL_REQUEST = "false"
# (2) A so-called "pr" (pull request). This *does* merge with master.
# In this case, $TRAVIS_PULL_REQUEST contains the pull request number.
# -----------------------------------------------------------------------

# We need the pull request number to talk to the GitHub API, make comments, etc. 
[ "${TRAVIS_PULL_REQUEST}" = "false" ] && exit 0

git config --global user.name "cpsievert"
git config --global user.email "cpsievert1@gmail.com"

cd ..
git clone https://github.com/cpsievert/plotly-test-table.git
cd plotly-test-table
git checkout gh-pages

# Read more about Travis environment variables --
# http://docs.travis-ci.com/user/ci-environment/
Rscript ../plotly/inst/testscripts/comment.R $TRAVIS_PULL_REQUEST $TRAVIS_BUILD_ID $TRAVIS_COMMIT $GH_TOKEN
