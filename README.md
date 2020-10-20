# covid-19-forecast
Public repository for CMU Delphi forecasting efforts for SARS-Cov2

# gh-actions

When adding a new forecaster or other package, take the following steps BEFORE you issue a pull request.
1. On main/.github, copy one of the existing actions.
2. Modify it to apply to your new package. And rename it following the conventions.
3. Switch to develop and merge main onto develop to get your new Action there.
4. Now switch to your package branch, issue a pull request to develop, and tag @dajmcdon or @jsharpna as reviewers.

If the check fails, revisit your package. Run R CMD Check locally and fix the problems. Make sure any dependencies are included as necessary. See the DESCRIPTION for `{aardvark}` as an example. If that all fails, get help on Slack.
