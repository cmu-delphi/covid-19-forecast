# Utilities

## Manifest

1. README, this file
2. `zookeeper` R package containing code to run our forecast production pipeline. Most of these functions were previously available elsewhere. Currently applies corrections and supports `aardvark` which previously relied on `evalforecast` (now deprecated).
3. Files for automating the current (`evalforecast`) pipeline
4. `QA-reports` R markdowns for QC reports for various forecasters
5. `current-pipeline` dated pipeline, no longer in use
6. File `production_params.R`: One file that controls the parameters
   used by `production_script.R` and also the QC markdowns. _In
   particular, this is where you change the signal to use, for
   example, if the default one is not available._
7. `production_script.R` the production script that runs docker
   containers for anteater and zebra. 


