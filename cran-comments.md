## Resubmission
This is a minor release of an existing package. In this version I have:

*    checkConsistency performance improvement; now runs 100 times faster
*    visualization improvement in 'plotDerivative'
*    bug in subheadline of plotPercentileSeries fixed
*    reduced number of messages
*    Starting work on inclusion of regularized Taylor models. Since simulation
     studies with glmnet did not show improvement, we generate much more models
     and preselect those, which pass an initial consistency check.
     Now, the a consistent model with the highest R2 is selected. R^2 and terms
     can of course still be specified as usual.
*    plotSubset improved: now indicates, which models did not pass the initial 
     consistency check via empty circles
*    Plots improved generally, now prints formula and statistical indicators
     in Greek letters and R^2 with the 2 uppercase


## Test environments
* local WIN10, 64Bit install, R 4.4.1
* winbuilder win release, win old release, win development and mac release
* rhub2 win, mac, mac-arm64, ubuntu-latest, ubuntu-release


## R CMD check results
There were no ERRORs or WARNINGs. There was a NOTE not on a potentially invalid URL in DESCRIPTION,
which is however correct.


## Downstream dependencies
There are currently no downstream dependencies for this package.
