## Resubmission
This is a minor release of an existing package. In this version I have:

* corrected bugs
* revised several functions
* added documentation

## Test environments
* local WIN10, 64Bit install, R 4.0.2
* winbuilder release, old release and development
* OS X (macOS High Sierra, Version 10.13.6), R 3.6.1
* Ubuntu 16.04.6 LTS via Travis-CI for release and development, R 4.1.0
* OS X 10.13.6 via Travis-CI for release and development
* R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or WARNINGs, but NOTES on potentially outdated links in data descriptions. These are all false positives. In the first submission, I had wrongly used 2020 as the year. The error is corrected.

## Downstream dependencies
There are currently no downstream dependencies for this package.
