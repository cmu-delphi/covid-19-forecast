#! /usr/bin/env bash
##
## Stop on first error to prevent spurious problems downstream
set -euxo pipefail

function branch_exists_locally () {
    git -C ${C19_REPO_PATH} show-ref -q --heads ${1}
    if [ $? -ne 0 ]; then
	## Branch does NOT exist locally
        echo "false"
    else
	## Branch exists locally
        echo "true"
    fi
}

function branch_exists_remotely () {
    git -C ${C19_REPO_PATH} ls-remote --exit-code --heads origin ${1} # zero return code if exists remotely
    if [ $? -ne 0 ]; then
	## Branch does NOT exist remotely
        echo "false"
    else
	## Branch exists remotely
        echo "true"
    fi
}

echo "0. Copy scripts and markdowns for run"
## Check out main branch first and sync up all branches
(cd ${C19_REPO_PATH} &&
    git checkout main &&
    git fetch &&
    git pull --all --ff-only)

cp ${C19_REPO_PATH}/forecasts/weekly_reports/${LASTWEEK}/predictions_cards.rds ${PROD_DOCKER_DIR}

(cd ${C19_REPO_PATH} &&
    git checkout develop &&
    git pull)

cp ${C19_REPO_PATH}/utilities/weekly_evaluation_notebook/covidhub_evaluation.Rmd ${C19_REPO_PATH}/utilities/weekly_evaluation_notebook/create_reports.R ${PROD_DOCKER_DIR}

echo "1. Run forecasts in docker"
(docker run --env TODAY --env FORECAST_DATE --env GITHUB_PAT --env TZ="America/New_York" --rm --name AZ --volume ${PROD_DOCKER_DIR}:/mnt --workdir /mnt covidcast R CMD BATCH --no-save --no-restore /mnt/create_reports.R /mnt/create_reports_${TODAY}.Rout)

echo "2. Commit results to repo main branch under today's date"
## Check out main branch first
git -C ${C19_REPO_PATH} checkout main
## Make directory
mkdir ${C19_REPO_PATH}/forecasts/weekly_reports/${TODAY}
## Move output and log
mv ${PROD_DOCKER_DIR}/predictions_cards.rds ${PROD_DOCKER_DIR}/score_cards_state_deaths.rds ${PROD_DOCKER_DIR}/score_cards_county_cases.rds ${PROD_DOCKER_DIR}/state_evaluations.html ${PROD_DOCKER_DIR}/county_evaluations.html ${PROD_DOCKER_DIR}/create_reports_${TODAY}.Rout ${C19_REPO_PATH}/forecasts/weekly_reports/${TODAY}
## Commit and push
(cd ${C19_REPO_PATH} &&
     git add --all &&
     git commit -m "Weekly report run for ${TODAY}" &&
     git push origin)

echo "Done."




