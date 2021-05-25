## Emacs, make this -*- mode: sh; -*-

FROM rocker/verse

MAINTAINER "Balasubramanian Narasimhan" naras@stanford.edu

## Copy Gurobi installation file
COPY ./gurobi9.1.1_linux64.tar.gz /tmp

# Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTEND=noninteractive

## Install tidyverse and needed packages

## Install packages needed for nearly everything
RUN Rscript -e 'install.packages(c("glmnet", "zoo", "fuzzyjoin", "Iso", "logger", "usmap", "Rglpk", "maptools", "MMWRweek", "sf", "Cairo", "triebeard", "httpcode", "urltools", "crul", "fauxpas", "base64url", "webmockr", "mockery", "RcppArmadillo", "scoringRules", "goftest", "data.table", "ggnewscale", "RcppRoll", "scoringutils", "TTR", "xts", "quantmod", "quadprog", "urca", "tseries", "timeDate", "lmtest", "fracdiff", "forecast"))'

## Install commonly used packages for our packages
RUN Rscript -e 'install.packages("covidcast", repos = "https://cran.r-project.org")

## Install the Gurobi optimizer libraries
RUN cd /opt && tar xzf /tmp/gurobi9.1.1_linux64.tar.gz

## Create the Gurobi License file that uses the CMU Gurobi token server
RUN echo "TOKENSERVER=gurobi-license-1.srv.cs.cmu.edu" > /opt/gurobi911/linux64/gurobi.lic

## Set environment variables for GUROBI and install R Gurobi package
ENV GUROBI_HOME "/opt/gurobi911/linux64"
ENV GRB_LICENSE_FILE "/opt/gurobi911/linux64/gurobi.lic"
ENV PATH "${PATH}:${GUROBI_HOME}/bin"
ENV LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"
RUN Rscript -e 'install.packages("/opt/gurobi911/linux64/R/gurobi_9.1-1_R_4.0.2.tar.gz", repos = NULL)'

## Install quantgen
RUN Rscript -e 'devtools::install_github(repo = "ryantibs/quantgen", subdir = "R-package/quantgen")'



