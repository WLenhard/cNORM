## Resubmission
This is a minor release of an existing package. In this version I have:

*    corrected a bug in 'bestModel'
*    Parametric modelling with beta binomial functions now fully implemented
*    S3 functions predict, plot and summary added for bet a binomial models
*    Fixed input validation in getNormScoreSE    
*    Started intensive work on regularization in Taylor models (planned for v4.0)
*    Vignette on modelling with beta binomial distribution
*    Transition from lattice to ggplot2
*    Code on covariates removed from the complete package
*    cNORM-Demo vignette revised
*    code simplification in plotting functions, parameters removed
*    new parameter to plot raw scores in plotPercentiles (default FALSE)
*    'buildCnormObject' function added to help with compatibility (joins data and model
     to cnorm object)
*    cNORM.GUI() updated
*    datasets life, mortality and EPM removed
*    performance optimization


## Test environments
* local WIN10, 64Bit install, R 4.4.1
* winbuilder win release, win old release, win development and mac release


## R CMD check results
There were no ERRORs or WARNINGs. There were NOTES on potential link and DOI issues in DESCRIPTION, which are correct, however.

## Downstream dependencies
There are currently no downstream dependencies for this package.
