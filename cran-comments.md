## Resubmission
This is a medium release of an existing package. This release includes new 
functions for automatically selecting parametric models based on BIC. The raking 
and weighting was revised and ranking methods optimized. The monotonicity 
checks are now much more strict. In this version I have:

Changes:

*    new methods: autoselect.betabinomial, autoselect.shash
*    Information on modelling Logits from IRT models added to vignette
*    function getGroups hardened for rare exceptions
*    Stricter check on monotonicity in Taylor polynomials
*    Cleaned up derive function
*    Vectorization in regressionFunction()
*    Code review of the raking code and the weighted.rank
*    Code review for rankByGroups and rankBySlidingWindow; performance improvements
*    prepareData and cnorm functions hardened
*    added example code for new functions to vignette and README.md


## Test environments
* local WIN11, 64Bit install, R 4.6.0
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest,
  Windows latest



## R CMD check results
There were no ERRORs, WARNINGs or NOTES


## Downstream dependencies
There are currently no downstream dependencies for this package.
