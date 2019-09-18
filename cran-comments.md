## Test environments
* local WIN10, 64Bit install, R 3.6.1
* winbuilder release and development
* OS X (macOS High Sierra, Version 10.13.6), R 3.6.1
* Ubuntu 14.04.5 LTS via Travis-CI for release and development
* OS X 10.13.3 via Travis-CI for release and development
* R-hub Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTES

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Update and resubmission
This is the fifth release to CRAN  (v1.2.2). In this version I have:

* improved handling of covariates
* adjusted parameters in bestModel function, when R2 > .99 is not reached
* fixed bugs, especially with regards to predicting norm scores in large samples
* added checks and warning messages
* improved usability
