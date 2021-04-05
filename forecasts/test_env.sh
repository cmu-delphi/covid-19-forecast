#! /usr/bin/env bash
## Never edit this. Edit production_script.Rmd markdown
## Generate this by purling and then uncomment
##

## Export environment variables
export TODAY=$(date '+%Y-%m-%d')
export LASTWEEK=$(date --date='-7 days' '+%Y-%m-%d')
## export FORECAST_DATE=$(date --date='tomorrow'  '+%Y-%m-%d')
## Now forecasting always for next Monday
export FORECAST_DATE=${TODAY}
export PROD_PREFIX=/home/${LOGNAME}/production_run
export PROD_DOCKER_DIR=${PROD_PREFIX}/docker2
export PROD_DIR=${PROD_PREFIX}/prod_${TODAY}
export REPO_PATH=/home/${LOGNAME}/GitHub/covidcast-forecast
export C19_REPO_PATH=/home/${LOGNAME}/GitHub/covid-19-forecast
export GITHUB_PAT=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
