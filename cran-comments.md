## Resubmission
This is a minor release of an existing package. In this version I have:

*    added check on score data in cnorm.betabinomial
*    switch to rankByGroup in cnorm in case age variable is plausibly grouping variable
*    bug in subtitles of plotPercentileSeries fixed
*    adjusted output and model recommendations in cnorm.cv
*    fixed legend in plotDensity()
*    bug in predictRaw fixed, which caused plotDerivative to plot wrong results
*    improved initial starting and control parameters in cnorm.betabinomial2
*    bugs in diagnostics.betabinomial fixed
*    vignettes revised


## Test environments
* local WIN10, 64Bit install, R 4.4.1
* winbuilder win release, win old release, win development and mac release
* rhub2 win, mac, mac-arm64, ubuntu-latest, ubuntu-release


## R CMD check results
There were no ERRORs, WARNINGs or NOTES.


## Downstream dependencies
There are currently no downstream dependencies for this package.
