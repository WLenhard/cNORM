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
* R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub Fedora Linux, R-devel, clang, gfortran
* R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs, but NOTES on winbuilder develop on potentially outdated links in data descriptions and a problem with the date in description. The notes on links all false positives.

## Downstream dependencies
There are currently no downstream dependencies for this package.
