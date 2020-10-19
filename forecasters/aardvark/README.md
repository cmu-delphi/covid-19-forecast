# aardvark

The Carnegie Mellon Delphi Lab COVID-19 state death forecaster (aardvark)

The production package for the Carnegie Mellon Delphi Lab COVID-19 state death forecaster.
This forecaster, which is submitted to the CDC as CMU-TimeSeries is a basic AR-type time 
series model fit using lagged values of case counts and deaths as features. No assumptions 
are made regarding reopening or governmental interventions. The model is jointly fit across 
all 50 US states, after some time-alignment is performed as a preprocessing step to facilitate 
comparison between states. Heavier weight is placed on more recent training data, to account 
for nonstationarity in the underlying process. A lasso penalty is added to induce variable 
selection and prevent overfitting. Quantiles are computed using a residual (Gaussian) 
bootstrap, separately for each location.


Relevant links:

https://delphi.cmu.edu

https://covidcast.cmu.edu/

https://viz.covid19forecasthub.org/
