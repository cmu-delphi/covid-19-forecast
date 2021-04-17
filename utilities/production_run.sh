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

(cd ${C19_REPO_PATH} &&
    git checkout develop &&
    git pull)

cp ${C19_REPO_PATH}/utilities/production_script.R ${C19_REPO_PATH}/utilities/production_params.R ${PROD_DOCKER_DIR}
cp ${C19_REPO_PATH}/utilities/QA-reports/anteater.Rmd ${C19_REPO_PATH}/utilities/QA-reports/zebra.Rmd ${PROD_DOCKER_DIR}

echo "1. Run forecasts in docker"
(docker run --env TODAY --env FORECAST_DATE --env GITHUB_PAT --env TZ="America/New_York" --rm --name AZ --volume ${PROD_DOCKER_DIR}:/mnt --workdir /mnt covidcast R CMD BATCH --no-save --no-restore /mnt/production_script.R /mnt/production_script_${TODAY}.Rout)


echo "2. Commit results to repo under today's branch"
## Check out main branch first
git -C ${C19_REPO_PATH} checkout main

## Only create local branch if not already present due to aborted runs etc.
echo "Step 2a. Create local branch if not already present"
TODAYS_BRANCH=prodrun_${TODAY}
branch_exists="$(branch_exists_locally ${TODAYS_BRANCH})"
if [ "${branch_exists}" == "false" ]; then 
    # Branch does not exist locally, so create and checkout
    git -C ${C19_REPO_PATH} checkout -b ${TODAYS_BRANCH}
else 
    # Branch exists, so just checkout
    git -C ${C19_REPO_PATH} checkout ${TODAYS_BRANCH}
fi

echo "Step 2b. Move the outputs to forecasts area"
mv ${PROD_DOCKER_DIR}/state-output/*.RDS ${PROD_DOCKER_DIR}/state-output/*.html ${C19_REPO_PATH}/forecasts/state-output
mv ${PROD_DOCKER_DIR}/county-output/*.RDS ${PROD_DOCKER_DIR}/county-output/*.html ${C19_REPO_PATH}/forecasts/county-output
mv ${PROD_DOCKER_DIR}/*.csv ${C19_REPO_PATH}/forecasts
(cd ${C19_REPO_PATH} &&
     git add --all &&
     git commit -m "${TODAY} production run forecast for ${FORECAST_DATE}" &&
     git push --set-upstream origin ${TODAYS_BRANCH} &&
     git checkout main)

echo "Step 2c. Cleanup last week branch locally"
## Make sure we are back on main
git -C ${C19_REPO_PATH} checkout main
branch_exists="$(branch_exists_locally prodrun_${LASTWEEK})"
if [ "${branch_exists}" == "true" ]; then 
    # Branch exists locally, so delete
    echo "Deleting local branch prodrun_${LASTWEEK}"
    git -C ${C19_REPO_PATH} branch -D prodrun_${LASTWEEK}
fi

echo "Step 3d. Cleanup last week branch remotely"
branch_exists="$(branch_exists_remotely prodrun_${LASTWEEK})"
if [ "${branch_exists}" == "true" ]; then 
    # Branch exists remotely, so delete
    echo "Deleting remote branch prodrun_${LASTWEEK}"
    git -C ${C19_REPO_PATH} push origin --delete prodrun_${LASTWEEK}
fi

echo "Done."




