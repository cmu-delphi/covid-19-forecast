# Utilities

## Manifest

1. `README.md`: this file
2. `Dockerfile`: the docker file for creating containers. Note the
   dependence on the [`gurobi`](https://www.gurobi.com) solver,
   requiring the download of the `gurobi9.1.1_linux64.tar.gz` for
   Linux X86_64 and placement in the (docker invocation)
   directory. Furthermore, note the use of a license token server in
   the `cmu.edu` realm by default; this can be modified to use a local
   academic license if needed. The command `docker build -t covidcast
   -f <path_to_Dockerfile>` will then create a docker container tagged
   `covidcast` on any machine.

3. `zookeeper`: R package containing code to run our forecast
   production pipeline. Most of these functions were previously
   available elsewhere. Currently applies corrections and supports
   `aardvark` which previously relied on `evalforecast` (now
   deprecated).

4. Files for automating the current (`evalforecast`) pipeline.

5. `QA-reports`: R markdowns for QC reports for various forecasters.

6. `current-pipeline`: dated pipeline, no longer in use.

7. `production_env.sh`: the bash script that sets environment
   variables such as where the github repos are locally located,
   dates, etc. _Note that the environment variable `GITHUB_PAT` is
   assumed to be set somewhere in the profile and available during the
   run. For security reasons, it should never be part of any public
   script._

8. `production_params.R`: one file that controls the parameters used
   by `production_script.R` and also the QC markdowns. _In particular,
   this is where you change the signal to use, if the default one is
   not available._

9. `production_run.sh` the bash script that ensures that the
   repositories are updated, runs docker instances, pushes results
   back to the appropriate branch of repo, and deletes 7-day old
   branches. 

10. `production_script.R` the R script run by `production_run.sh` that
   in turn, runs forecasts, generates QC reports and the submission
   CSV.
  
11. `crontab.sh` a sample crontab file to be edited suitably for use. 


