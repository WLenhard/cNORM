[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/cNORM)](https://cran.r-project.org/package=cNORM)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/cNORM)](https://cran.r-project.org/package=cNORM)

<img src="vignettes/logo.png" align=right style="border:0;">

# cNORM

 cNORM (W. Lenhard, Lenhard & Gary) is a package for the R environment for statistical computing that aims at generating continuous test norms in psychometrics and biometrics and to analyze the model fit. Originally, cNorm exclusively used an approach that makes no assumptions about the specific distribution of the raw data (A. Lenhard, Lenhard, Suggate & Segerer, 2016). Since version 3.2 (2024), however, the package also offers the option of parametric modeling using the beta-binomial distribution.

cNORM was developed specifically for achievement tests (e.g. vocabulary development: A. Lenhard, Lenhard, Segerer & Suggate, 2015; written language acquisition: W. Lenhard, Lenhard & Schneider, 2017). However, the package can be used wherever mental (e.g. reaction time), physical (e.g. body weight) or other test scores depend on continuous (e.g. age, duration of schooling) or discrete explanatory variables (e.g. sex, test form). In addition, the package can also be used for "conventional" norming based on individual groups, i.e. without including explanatory variables.

The package estimates percentiles as a function of the explanatory variable. This is done either parametrically on the basis of the beta-binomial or the Sinh-Arcsinh (ShaSh) distribution or distribution-free using Taylor polynomials. For an in-depth tutorial, visit the [project homepage](https://www.psychometrica.de/cNorm_en.html), try the [online demonstration](https://cnorm.shinyapps.io/cNORM/) and have a look at the vignettes.


## In a nutshell

A quick guide to distribution-free modeling with the essential cNORM functions:
```{r example}
## Basic example code for modeling the sample dataset
library(cNORM)

# Start the graphical user interface (needs shiny installed)
# The GUI includes the most important functions. For specific cases,
# please use cNORM on the console.
cNORM.GUI()

# Using the syntax on the console: The function 'cnorm' performs
# all steps automatically. Please specify the raw score and the
# grouping variable. The resulting object contains the ranked data
# via object$data and the model via object$model.
cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)

# ... and since we love pop music as much as Taylor polynomials, you
# can also use the taylorSwift function to swiftly compute a distribution-
# free Taylor polynomial model (which is however identical to cnorm). Here
# with the sample dataset 'ppvt':
model <- taylorSwift(ppvt$raw, ppvt$group)

# Plot different indicators of model fit depending on the number of
# predictors
plot(cnorm.elfe, "subset", type=0) # plot R2
plot(cnorm.elfe, "subset", type=3) # plot MSE

# NOTE! At this point, you usually select a good fitting model and rerun
# the process with a fixed number of terms, e. g. 4. Avoid models
# with a high number of terms:
cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group, terms = 4)

# Powers of age can be specified via the parameter 't'.
# Cubic modeling is usually sufficient, i.e., t = 3.
# In contrast, 'k' specifies the power of the person location.
# This parameter should be somewhat higher, e.g., k = 5.
cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group, k = 5, t = 3)

# Visual inspection of the percentile curves of the fitted model
plot(cnorm.elfe, "percentiles")

# Visual inspection of the observed and fitted raw and norm scores
plot(cnorm.elfe, "norm")
plot(cnorm.elfe, "raw")

# In order to compare different models, generate a series of percentile
# plots with an ascending number of predictors, in this example between
# 5 and 14 predictors.
plot(cnorm.elfe, "series", start=5, end=14)

# Cross validation of number of terms with 80% of the data for training
# and 20% for validation. Due to the time intensity, the maximum
# number of terms is restricted to 10 in this example
# with 3 repetitions.
cnorm.cv(cnorm.elfe$data, max=10, repetitions=3)

# Cross validation with prespecified terms of an already
# existing model
cnorm.cv(cnorm.elfe, repetitions=3)

# Print norm table (in this case: 0, 3 or 6 months at grade level 3)
normTable(c(3, 3.25, 3.5), cnorm.elfe)

# The other way round: Print raw table (grade level 3; 0 months) together
# with 90% confidence intervalls for a test with a reliability of .94
rawTable(3, cnorm.elfe, CI = .9, reliability = .94)
```


Modelling norm data using beta-binomial distributions:
```{r example}
library(cNORM)
# cNORM can as well model norm data using the beta-binomial
# distribution, which usually performs well on tests with
# a fixed number of dichotomous items.
model.betabinomial <- cnorm.betabinomial(ppvt$age, ppvt$raw)

# Adapt the power parameters for α and β to increase or decrease
# the fit:
model.betabinomial <- cnorm.betabinomial(ppvt$age, ppvt$raw, alpha = 4)

# Plot percentile curves and display manifest and modelled norm scores.
plot(model.betabinomial, ppvt$age, ppvt$raw)
plotNorm(model.betabinomial, ppvt$age, ppvt$raw, width = 1)

# Display fit statistics:
summary(model.betabinomial)

# Prediction of norm scores for new data and generating norm tables
predict(model.betabinomial, c(8.9, 10.1), c(153, 121))
tables <- normTable.betabinomial(model.betabinomial, c(2, 3, 4),
                                 reliability=0.9)
```

Modelling norm data using Sinh-Arcsinh (ShaSh) distributions:
```{r example}
library(cNORM)
# The Sinh-Arcsinh (ShaSh) distribution is a flexible approach.
# It can handle raw score value ranges including zeros and negative
# values, which pose a problem to Box Cox distributions.
# Shape parameters mu, sigma, epsilon and delta can be adjusted as well.
model.shash <- cnorm.shash(ppvt$age, ppvt$raw)

# Plot percentile curves and display manifest and modelled norm scores.
plot(model.shash, ppvt$age, ppvt$raw)

# Display fit statistics:
summary(model.shash, ppvt$age, ppvt$raw)

# Prediction of norm scores for new data and generating norm tables
predict(model.shash, c(8.9, 10.1), c(153, 121))
tables <- normTable.shash(model.shash, c(10, 15),
                                 reliability=0.9)
```

Conventional norming:
```{r example}
library(cNORM)

# cNORM can as well be used for conventional norming:
cnorm(raw=elfe$raw)
```


Start vignettes in cNORM:
```{r example}
library(cNORM)

vignette("cNORM-Demo", package = "cNORM")
vignette("WeightedRegression", package = "cNORM")
vignette("BetaBinomial", package = "cNORM")
vignette("ShaSh", package = "cNORM")
```



## Sample Data
The package includes data from two large test norming projects, namely ELFE 1-6 (Lenhard & Schneider, 2006) and German adaption of the PPVT4 (A. Lenhard, Lenhard, Suggate & Seegerer, 2015), which can be used to run the analysis. Furthermore, large samples from the Center of Disease Control (CDC) on growth curves in childhood and adolescence (for computing Body Mass Index 'BMI' curves), Type `?elfe`, `?ppvt` or `?CDC` to display information on the data sets.

## Terms of use, license and declaration of interest
cNORM is licensed under GNU Affero General Public License v3 (AGPL-3.0). This means that copyrighted parts of cNORM can be used free of charge for commercial and non-commercial purposes that run under this same license, retain the copyright notice, provide their source code and correctly cite cNORM. Copyright protection includes, for example, the reproduction and distribution of source code or parts of the source code of cNORM or of graphics created with cNORM. The integration of the package into a server environment in order to access the functionality of the software (e.g. for online delivery of norm scores) is also subject to this license. However, a regression function determined with cNORM, the norm tables ... are not subject to copyright protection and may be used freely without preconditions. If you want to apply cNORM in a way that is not compatible with the terms of the AGPL 3.0 license, please do not hesitate to contact us to negotiate individual conditions. If you want to use cNORM for scientific publications, we would also ask you to quote the source.

The authors would like to thank WPS (<https://www.wpspublish.com/>) for providing funding for developing, integrating and evaluating weighting and post stratification in the cNORM package. The research project was conducted in 2022. 

## References

*   Gary, S., Lenhard, W., Lenhard, A. et al. A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. Behav Res (2023a). https://doi.org/10.3758/s13428-023-02207-0
*   Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023b). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. Assessment, 10731911231153832. https://doi.org/10.1177/10731911231153832
*   Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary Test - Revision IV (Deutsche Adaption). Frankfurt a. M./Germany: Pearson Assessment.
*   Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. https://doi.org/10.1177/1073191116656437
*   Lenhard, A., Lenhard, W., Gary, S. (2018). Continuous Norming (cNORM). The Comprehensive R Network, Package cNORM, available: https://CRAN.R-project.org/package=cNORM
*   Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9),  e0222279. https://doi.org/10.1371/journal.pone.0222279
*   Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement(Online First), 1-33. https://doi.org/10.1177/0013164420928457

