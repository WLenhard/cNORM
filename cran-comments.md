## Resubmission
This is a minor release of an existing package. This release aims at a better 
integration of parametric functions for the plotting functions, an overhaul of 
the Shiny Apps and huge performance improvements in the Taylor modelling 
(rankBySlidingWindows and checking for monotonicity). The subsampling will be 
deprecated and deleted from a future release of cNORM. In this version I have:

Changes:

*    Set nbest to 10 in exhaustive search
*    Author name order corrected in README.md
*    ShinyApp streamlined
*    discrete parameter when plotting betabinomial models added
*    plotNorm, plotNormCurves and plotDensity fixed to correctly handle parametric
     models
*    boost performance in rankBySlidingWindow, which as well affects performance
     of the parametric modelling, especially when plotting
*    broader test coverage
*    subsampling set to FALSE on default; the internal function is deprecated and  
     will be removed in a future release
*    Monotinicity checks with performance improvements


## Test environments
* local WIN11, 64Bit install, R 4.6.0
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest,
  Windows latest



## R CMD check results
There were no ERRORs, WARNINGs or NOTES


## Downstream dependencies
There are currently no downstream dependencies for this package.
