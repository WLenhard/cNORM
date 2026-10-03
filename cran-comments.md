## cNORM version 3.7.0

New distributional family for modelling speeded tests and count data with the
Conway-Maxwell Poisson Counts Model (CMPCM), which allows for adjusting
dispersion.

### New features
* Modelling speeded tests with the Conway-Maxwell Poisson Counts Model (CMPCM)
* Vignette for CMP modelling
* New dataset 'speed' with word reading fluency per age (synthetic data based
  on ELFE2)
* Extended test coverage for CMP

### Changes
* Checks on number of groups in Taylor modelling added. Now actively advises
  on 't' parameter reduction.
* Added section on model selection strategies to vignettes


## Test environments
* local WIN11, 64Bit install, R 4.6.0
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest, 
  Windows latest


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package.
