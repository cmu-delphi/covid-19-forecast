#! /usr/bin/env bash
## Never edit this. Edit production_new_forecasters.Rmd markdown
## Generate this by purling and then uncomment
##
## Stop on first error to prevent spurious problems downstream
set -euxo pipefail

function branch_exists_locally () {
    git -C ${REPO_PATH} show-ref -q --heads ${1}
    if [ $? -ne 0 ]; then
	## Branch does NOT exist locally
        echo "false"
    else
	## Branch exists locally
        echo "true"
    fi
}

function branch_exists_remotely () {
    git -C ${REPO_PATH} ls-remote --exit-code --heads origin ${1} # zero return code if exists remotely
    if [ $? -ne 0 ]; then
	## Branch does NOT exist remotely
        echo "false"
    else
	## Branch exists remotely
        echo "true"
    fi
}

(docker run --env TODAY --env FORECAST_DATE --env GITHUB_PAT --env TZ="America/New_York" --rm --name AZ --volume ${PROD_DOCKER_DIR}:/mnt --workdir /mnt covidcast R CMD BATCH --no-save --no-restore /mnt/run_az.R /mnt/run_az_${TODAY}.Rout)

echo "Done."


