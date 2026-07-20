## cNORM version 3.6.2

Fine-tuning of internal functions (relaxed monotonicity check) and a new S3 
method for predicting distributional moments of fitted Taylor, beta binomial
and shash models. Model averaging was falsly already set to TRUE by default
(now FALSE).

## Changes

* New function `predictMoments()`: Computes model-implied distributional 
  moments (mean, standard deviation, variance, skewness and excess kurtosis) 
  of the raw score distribution at one or more ages.
* Monotonicity check in Taylor polynomials now accept minimal inconsistencies
  (violations of less than 1% of the raw score range; parameter added to cnorm 
  and bestModel).
* The averaging feature has been turned of by default. We have to conduct 
  more research first.
* Deprectated subsampling parameter and according function removed  
* Added analytic grading in fitting shash models


## Test environments
* local WIN11, 64Bit install, R 4.6.0
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest, 
  Windows latest


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package.
