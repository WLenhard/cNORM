## Resubmission
This is a minor release of an existing package. In this version I have:

*    Added warning in 'cnorm' in case, both age and group is specified
*    extended cnorm.cv for use of weights and sliding window
*    added silent option to several functions to reduced the number of messages
*    revised function documentation   


## Test environments
* local WIN10, 64Bit install, R 4.3.1
* winbuilder release, old release and development
* rhub: Windows Server 2022, R-devel, 64 bit
				Ubuntu Linux 20.04.1 LTS, R-release, GCC
				Fedora Linux, R-devel, clang, gfortran


## R CMD check results
There were no ERRORs or WARNINGs. There were NOTES on potential link and DOI issues in DESCRIPTION, which are correct, however.

## Downstream dependencies
There are currently no downstream dependencies for this package.
