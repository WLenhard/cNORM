## Resubmission
This is a resubmission. In this version I have:

* added feature updates, new datasets and corrected bugs
* checked spelling
* repeated all tests on R CMD Check, rhub, Travis (Win, Linux, MacOS X)

## Test environments
* local WIN10, 64Bit install, R 3.6.1
* winbuilder release and development
* OS X (macOS High Sierra, Version 10.13.6), R 3.6.1
* Ubuntu 14.04.5 LTS via Travis-CI for release and development
* OS X 10.13.3 via Travis-CI for release and development
* R-hub Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTES.
There were no ERRORs or WARNINGs, but NOTES on potentially outdated links in data descriptions on Ubuntu Linux. These are all false positives.

## Downstream dependencies
There are currently no downstream dependencies for this package.
