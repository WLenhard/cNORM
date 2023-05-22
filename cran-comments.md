## Resubmission
This is a minor release of an existing package. In this version I have:

*    fixed regression bug in the internal predictNormByRoots-function for R4.3.0
*    added new references
*    new results in printSubset and plotSubset: F-tests on consecutive models
*    internal improvements in calcPolyInLBase2 for retrieving regression function coefficients at specific
     age. This speeds up norm score retrieval by 40%, leading to vast performance improvements in 
     large datasets and in cross validation by cnorm.cv
*    Added WPS publisher as a funder. WPS helped financing the weighting procedure for post stratification 
     based on iterative proportional fitting ("Raking")


## Test environments
* local WIN10, 64Bit install, R 4.3.0
* winbuilder release, old release and development
* rhub: Windows Server 2022, R-devel, 64 bit
				Ubuntu Linux 20.04.1 LTS, R-release, GCC
				Fedora Linux, R-devel, clang, gfortran


## R CMD check results
There were no ERRORs or WARNINGs. There were NOTES on potential link and DOI issues
in DESCRIPTION, which are correct, however.

## Downstream dependencies
There are currently no downstream dependencies for this package.
