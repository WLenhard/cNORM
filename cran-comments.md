## Resubmission
This is a minor release of an existing package. It has undergone a code review 
with Anthropic Claude Opus 4.7 to harden the code and to identify edge cases 
and handle them gracefully. In this version I have:

Changes:

*    all examples set to dontrun
*    removed CDC NHANES URL, since data has been depublished
*    rawTable() and normTable() now enforce monotonicity of raw and norm scores
     outward from the median. This reduces problems with inconsistent or NA
     results at the extreme ends of the model and is robust against missing values
     in the predicted series.
*    predictRaw() is more robust for degenerate models
*    predictNormByRoots() now correctly honours the force argument. When a
     raw score is unreachable within [minNorm, maxNorm], the function returns
     NA (default) or clips to the appropriate boundary (force = TRUE, now default).
*    Direction-of-search is now based on the unclipped model prediction at the
     scale mean.
*    bestModel() selection by terms or R2 is now correct under the
     extensive consistency screening introduced in 3.3. The chosen model now
     reflects the requested number of terms (or the smallest model meeting the
     R² threshold) rather than the row index in the filtered regsubsets
     summary.
*    cnorm.cv(): corrected the maximum-model-size formula (k + 1) * (t + 1) - 1;
     pCutoff, weight handling, and norm-score SE follow Oosterhuis et al.
     (2016) more faithfully and use safer indexing throughout. Fix when using
     sliding window ranking.
*    screenSubset() cleaned up: defensive initialisation, drop = FALSE on
     matrix subsetting so single-row results survive, removal of dead code.
*    internal function plotCumulative added for conventional norming


## Test environments
* local WIN11, 64Bit install, R 4.5.1
* winbuilder win release, win old release, win development
* Automatic checks on GitHub: Ubuntu (old-rel1, devel, release), MacOS latest,
  Windows latest



## R CMD check results
There were no ERRORs, WARNINGs or NOTES


## Downstream dependencies
There are currently no downstream dependencies for this package.
