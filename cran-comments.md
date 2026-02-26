## Resubmission
This is a minor release of an existing package. In this version I have:

*    fixed scale information in diagnostics (betabinomial and shash), which
     was not correctly passed to the function
*    input validation checks in cnorm.betabinomial and cnorm.shash     
*    optimization of shash functions
*    robustness of betabinomial functions
*    new shiny GUI for parametric modelling
*    Shiny GUI files restructured
*    plotPercentile did not minAge and maxAge parameter - fixed
*    plotPercentile performance improvement
*    saving plots in plotPercentileSeries, can run out of bounds - fixed
*    plotNorm object checks improved
*    plotCnorm helper parameter checks - fixed
*    fixed geom_hline warning in plotSubset
*    removed the raw parameter from plotRaw - it always uses the model data
*    improved model type check in plotDensity


## Test environments
* local WIN11, 64Bit install, R 4.5.1
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest,
  Windows latest



## R CMD check results
There were no ERRORs, WARNINGs or NOTES


## Downstream dependencies
There are currently no downstream dependencies for this package.
