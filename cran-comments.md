## Resubmission
This is a new release of an existing package. In this version I have:

* added features (cross validation; norm table compilation)
* checked spelling
* repeated all tests on R CMD Check, rhub, Travis (Win, Linux, MacOS X)

## Test environments
* local WIN10, 64Bit install, R 4.0.2
* winbuilder release and development
* OS X (macOS High Sierra, Version 10.13.6), R 3.6.1
* Ubuntu 16.04.6 LTS via Travis-CI for release and development, R 4.1.0
* OS X 10.13.6 via Travis-CI for release and development
* R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTES.
There were no ERRORs or WARNINGs, but NOTES on potentially outdated links in data descriptions on Ubuntu Linux. These are all false positives.

## Downstream dependencies
There are currently no downstream dependencies for this package.
