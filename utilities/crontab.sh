SHELL=/bin/bash
GITHUB_PAT=<GITHUB_PAT here>
30 10 * * * (cd <production_dir> && source ./production_env.sh && ./production_run.sh >> production_run_${TODAY}.log 2>&1)
