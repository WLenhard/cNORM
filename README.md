<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/cNORM)](https://cran.r-project.org/package=cNORM)
[![R-CMD-check](https://github.com/WLenhard/cNORM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WLenhard/cNORM/actions/workflows/R-CMD-check.yaml)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/cNORM)](https://cran.r-project.org/package=cNORM)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/cNORM?color=blue)](https://r-pkg.org/pkg/cNORM)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->
<img src="vignettes/logo.png" align=right style="border:0;">

# cNORM

**cNORM** (A. Lenhard, W. Lenhard & S. Gary) is an R package for statistical computing that generates continuous test norms in psychometrics and biometrics and evaluates model fit. Originally, cNORM exclusively used a distribution-free approach based on Taylor polynomials that makes no parametric assumptions about the raw score distribution (A. Lenhard, Lenhard, Suggate & Segerer, 2016). 

The package currently features **both distribution-free and parametric continuous norming**:
1. **Distribution-free modeling** using bivariate Taylor polynomials (A. Lenhard et al., 2016).
2. **Beta-binomial modeling** (since v3.2): For bounded accuracy tests with a fixed number of dichotomous items without a time limit (e.g., 1PL IRT / Rasch-scaled scales).
3. **Sinh-Arcsinh (SHASH) modeling** (since v3.5): For flexible continuous distributions accommodating skewness and varying tail heaviness, including scores spanning zero and negative numbers.
4. **Conway-Maxwell-Poisson (CMP) modeling** (since v3.7): For open-ended count data and **speeded tests** (e.g., number of correctly processed items within a time limit) as well as truncated (bounded) scales. Unlike standard Poisson models, CMP allows modeling equi-dispersion ($\nu = 1$), over-dispersion ($\nu < 1$), and, notably, **under-dispersion** ($\nu > 1$, $\text{Var} < \text{Mean}$), which is typical for speeded cognitive performance tasks.

cNORM was developed specifically for psychometric and educational tests (e.g. vocabulary development: A. Lenhard, Lenhard, Segerer & Suggate, 2015; written language acquisition: W. Lenhard, Lenhard & Schneider, 2017). However, it applies wherever mental (e.g., processing speed, reaction time), physical (e.g., body weight, height), or behavioral scores depend on continuous (e.g., age, duration of schooling) or discrete explanatory variables (e.g., grade, sex). Conventional norming based on separate subsamples is supported as well.

The package estimates conditional percentiles as a function of the explanatory variable. For an in-depth tutorial, visit the [project homepage](https://www.psychometrica.de/cNorm_en.html), try the [online demonstration](https://cnorm.shinyapps.io/cNORM/) and explore the package vignettes.


## In a nutshell

### 1. Distribution-free modeling with Taylor polynomials

```r
library(cNORM)

# Launch graphical user interface (requires shiny)
cNORM.GUI()   # distribution-free modeling with Taylor polynomials
cNORM.GUI2()  # parametric modeling

# Automatic model fitting via 'cnorm'
cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)

# Swift modeling (pop-culture alias for cnorm):
model <- taylorSwift(ppvt$raw, ppvt$group)

# Model selection diagnostics
plot(cnorm.elfe, "subset", type = 0) # Adjusted R2 by terms
plot(cnorm.elfe, "subset", type = 3) # Raw score RMSE by terms

# Fix the model to a chosen number of terms (e.g., 4):
cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group, terms = 4)

# Visual inspection of model percentiles and fit
plot(cnorm.elfe, "percentiles")
plot(cnorm.elfe, "norm")
plot(cnorm.elfe, "raw")

# Cross-validation across term numbers (Monte Carlo 80/20 split):
cnorm.cv(cnorm.elfe$data, max = 10, repetitions = 3)

# Generate norm table (at grade 3; 0, 3, or 6 months into the school year)
normTable(c(3, 3.25, 3.5), cnorm.elfe)

# Inverted raw score table with 90% true-score confidence intervals:
rawTable(3, cnorm.elfe, CI = .9, reliability = .94)
```


### 2. Parametric modeling for accuracy tests: Beta-binomial distribution
```r
library(cNORM)
# cNORM can as well model norm data using the beta-binomial
# distribution, which usually performs well on tests with
# a fixed number of dichotomous items without time cutoff.
# Ideal use case: 1PL IRT scale / Rasch modelling
# The example uses the inbuilt ELFE demo dataset (reading comprehension test
# for elementary school with grade variable; 28 items).
model.betabinomial <- cnorm.betabinomial(elfe$group, elfe$raw)

# Adapt the power parameters for α and β to increase or decrease
# the fit:
model.betabinomial <- cnorm.betabinomial(elfe$group, elfe$raw, alpha = 4)

# Automatic grid search to determine the model with the lowest BIC
model.betabinomial <- autoselect.betabinomial(elfe$group, elfe$raw)


# Plot percentile curves and display manifest and modelled norm scores.
plot(model.betabinomial, elfe$group, elfe$raw)
plotNorm(model.betabinomial, elfe$group, elfe$raw, width = 1)

# Display fit statistics:
summary(model.betabinomial)

# Prediction of norm scores for new data
predict(model.betabinomial, c(2.0, 2.2, 2.4, 2.6), c(10, 15, 13, 22))

# generate norm tables
tables <- normTable.betabinomial(model.betabinomial, c(2, 3, 4),
                                 reliability=0.9)
```

### 3. Parametric modeling for speeded tests and counts: Conway-Maxwell-Poisson (CMP)
```r
library(cNORM)

# The Conway-Maxwell-Poisson (CMP) model is designed for count data and speeded tests
# where the raw score is the number of processed items within a time limit.
# It smoothly models location mu(age) and dispersion nu(age), naturally capturing
# under-dispersion (nu > 1; Var < Mean) common in speeded tasks.

# Basic fit - please provide max_score if scale is truncated
model.cmp <- cnorm.cmp(speed$age, speed$fluency, max_score=75)

# Automatic model selection over polynomial degrees via BIC:
model.cmp <- autoselect.cmp(speed$age, speed$fluency, max_score=75)

# Fit statistics and parameter estimates:
summary(model.cmp, age = speed$age, score = speed$fluency)

# Visual inspection: discrete step-function percentiles against manifest data
plot(model.cmp, speed$age, speed$fluency)
plotNorm(model.cmp, age = speed$age, score = speed$fluency, width = 1)

# Predict norm scores (using mid-p rank adjustment for discrete ties):
predict(model.cmp, age = c(7.5, 8.2, 9.0), score = c(18, 24, 30))

# Generate discrete norm tables:
tables <- normTable(c(7.5, 8.5), model.cmp, start = 0, end = 60, reliability = 0.88)

# Model-implied population moments (mean, variance, skewness, excess kurtosis):
predictMoments(model.cmp, age = seq(7, 10, by = 0.5))
```

### 4. Parametric modeling for continuous scores: Sinh-Arcsinh (SHASH)
```r
library(cNORM)
# The Sinh-Arcsinh (ShaSh) distribution is a flexible approach.
# It can handle raw score value ranges including zeros and negative
# values, which pose a problem to Box Cox distributions.
# Shape parameters mu, sigma, epsilon and delta can be adjusted as well.
# The following example uses the inbuilt PPVT4 demo dataset for receptive
# vocabulary development from 3 to 18.
model.shash <- cnorm.shash(ppvt$age, ppvt$raw)

# Automatic grid search to determine the model with the lowest BIC
model.shash <- autoselect.shash(ppvt$age, ppvt$raw)

# Plot percentile curves and display manifest and modelled norm scores.
plot(model.shash, ppvt$age, ppvt$raw)

# Display fit statistics:
summary(model.shash, ppvt$age, ppvt$raw)

# Prediction of norm scores for new data and generating norm tables
predict(model.shash, c(8.9, 10.1), c(153, 121))
tables <- normTable.shash(model.shash, c(10, 15),
                                 reliability=0.9)
```

### 5. Visual Model Comparison
```r
# Compare distribution-free Taylor polynomial with CMP count model:
model.taylor <- cnorm(raw = elfe$raw, group = elfe$group)
model.cmp    <- cnorm.cmp(age = elfe$group, score = elfe$raw)

compare(model.taylor, model.cmp, age = elfe$group, score = elfe$raw)
```

### 6. Conventional norming:
```r
library(cNORM)

# cNORM can as well be used for conventional norming:
cnorm(raw=elfe$raw)
```


Start vignettes in cNORM:
```r
library(cNORM)

vignette("cNORM-Demo", package = "cNORM")
vignette("WeightedRegression", package = "cNORM")
vignette("BetaBinomial", package = "cNORM")
vignette("sinh", package = "cNORM")
vignette("CMP", package = "cNORM")
```



## Sample Data
The package includes data from two large test norming projects, namely ELFE 1-6 (Lenhard & Schneider, 2006) and German adaption of the PPVT4 (A. Lenhard, Lenhard, Suggate & Seegerer, 2015), which can be used to run the analysis. Furthermore, large samples from the Center of Disease Control (CDC) on growth curves in childhood and adolescence (for computing Body Mass Index 'BMI' curves), Type `?elfe`, `?ppvt` or `?CDC` to display information on the data sets.

## Terms of use, license and declaration of interest
cNORM is licensed under GNU Affero General Public License v3 (AGPL-3.0). This means that copyrighted parts of cNORM can be used free of charge for commercial and non-commercial purposes that run under this same license, retain the copyright notice, provide their source code and correctly cite cNORM. Copyright protection includes, for example, the reproduction and distribution of source code or parts of the source code of cNORM. The integration of the package into a server environment in order to access the functionality of the software (e.g. for online delivery of norm scores) is also subject to this license. However, a regression function determined with cNORM, the norm tables and plots are not subject to copyright protection and may be used freely without preconditions. If you want to apply cNORM in a way that is not compatible with the terms of the AGPL 3.0 license, please do not hesitate to contact us to negotiate individual conditions. If you want to use cNORM for scientific publications, we would also ask you to quote the source.

The authors would like to thank WPS (<https://www.wpspublish.com/>) for providing funding for developing, integrating and evaluating weighting and post stratification in the cNORM package. The research project was conducted in 2022. 

## References

*   Gary, S., Lenhard, W., Lenhard, A. et al. A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. Behav Res (2023a). https://doi.org/10.3758/s13428-023-02207-0
*   Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023b). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. Assessment, 10731911231153832. https://doi.org/10.1177/10731911231153832
*   Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary Test - Revision IV (Deutsche Adaption). Frankfurt a. M./Germany: Pearson Assessment.
*   Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. https://doi.org/10.1177/1073191116656437
*   Lenhard, A., Lenhard, W., Gary, S. (2018). Continuous Norming (cNORM). The Comprehensive R Network, Package cNORM, available: https://CRAN.R-project.org/package=cNORM
*   Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9),  e0222279. https://doi.org/10.1371/journal.pone.0222279
*   Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement(Online First), 1-33. https://doi.org/10.1177/0013164420928457

