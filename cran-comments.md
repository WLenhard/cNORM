## cNORM version 3.6.1

This release focuses on optimization, code hardening, and improved handling of 
beta-binomial models.

**Major Features & Behavioral Changes**
* **Model Averaging:** Introduced an `averaging` argument in `bestModel()` to 
  compute final coefficients as a BIC-weighted average across consistency-
  screened models. This reduces model selection variance and guarantees 
  consistent models.
* **Analytic Consistency Checks:** Replaced the discrete grid check with an 
  exact analytical monotonicity check using `polyroot()` (now the default in 
  `checkConsistency()`). This detects narrow violations that a discrete grid 
  can miss and is faster.
* **Consistency Screening:** Flat candidate models (independent of L) are now 
  correctly flagged as inconsistent, and screening evaluates 8 age points (up 
  from 2) to catch intersecting percentile curves.
* **Plotting:** Discrete beta-binomial quantiles are now correctly rendered as 
  step functions in `plot()` and `compare()`.

**Deprecations**
* Deprecated `subsample_lm()` and the `subsampling` argument in `bestModel()`. 
  Subsampling OLS coefficients adds Monte-Carlo noise without improving the fit 
  and is entirely replaced by the new `averaging` method.

**Bug Fixes & Performance Optimizations**
* Fixed issues causing `NaN` RMSE values (switched from `.lm.fit()` to 
  `lm.fit()`) and crashes during consistency screening with non-Taylor 
  predictors.
* Fixed beta-binomial probability normalization over truncated supports and 
  resolved L-BFGS-B optimization convergence failures.
* Fixed an edge case failure in `checkConsistency()` for conventional norming 
  (`minA1 == maxA1`).
* Significant performance improvements in beta-binomial modeling via 
  precomputation of `lchoose()` and grouped-data optimizations in `predict()`.


## Test environments
* local WIN11, 64Bit install, R 4.6.0
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest, 
  Windows latest


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package.
