SHELL=/bin/bash
GITHUB_PAT=<GITHUB_PAT here>
40 14 * * * (cd <production_dir> && source ./production_env.sh && ./production_run.sh >> production_run_${TODAY}.log 2>&1)
