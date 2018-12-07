## Test environments
* local WIN10, 64Bit install, R 3.5.1
* winbuilder release and development
* OS X (macOS High Sierra, Version 10.13.6), R 3.5.1
* Ubuntu 14.04.5 LTS via Travis-CI for release and development
* OS X 10.13.3 via Travis-CI for release and development
* R-hub Fedora Linux, R-devel, GCC 
* R-hub macOS 10.11 El Capitan, R-release (experimental) 

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Update and resubmission
This is an update to the prior release (1.0.1). In this version I have:

* included lot's of minor impovements (dealing with missings ...)
* added extra plotting functions
* added cross validation

In the resubmission, the following changes have been applied:

* removed UTF-8 attributes from ppvt dataset and cleared all datasets from non ASCII signs
* deleted code in vignette needing to much build time
* additional tests run on R-hub
