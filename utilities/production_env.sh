#! /usr/bin/env bash

## Export environment variables
export TODAY=$(date '+%Y-%m-%d')
export LASTWEEK=$(date --date='-7 days' '+%Y-%m-%d')

## Forecasting always for Monday
if [[ $(date +%u) -lt 2 ]]; then
    export FORECAST_DATE=${TODAY} ## TODAY is a Monday
else
    export FORECAST_DATE=$(date --date='next Monday'  '+%Y-%m-%d')
fi

export PROD_PREFIX=/home/${LOGNAME}/production_run
export PROD_DOCKER_DIR=${PROD_PREFIX}/docker2
export PROD_DIR=${PROD_PREFIX}/prod_${TODAY}
export REPO_PATH=/home/${LOGNAME}/GitHub/covidcast-forecast
export C19_REPO_PATH=/home/${LOGNAME}/GitHub/covid-19-forecast

