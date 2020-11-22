# Mapping of S3 methods to cNORM methods

#' S3 method for plotting percentiles of cnorm objects
#'
#' The function plots the norm curves based on the regression model against
#' the actual percentiles from the raw data.
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot
#' @seealso plotPercentiles
#' @family plot
#' @export
plot.cnorm <- function(object, ...) {  UseMethod("plot.cnorm") }

#' S3 method for plotting raw scores against fitted raw scores of cnorm objects
#'
#' The function plots the raw data against the fitted scores from
#' the regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' regression line.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.raw
#' @seealso plotRaw
#' @family plot
#' @export
plot.raw.cnorm <- function(object, ...) { UseMethod("plot.raw.cnorm") }

#' S3 method for plotting manifest against predicted norm sores
#' The function plots the manifest norm score against the fitted norm score from
#' the inverse regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' regression line. The computation of the standard error is based on Oosterhuis, van der
#' Ark and Sijtsma (2016).
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.norm
#' @seealso plotNorm
#' @family plot
#' @export
plot.norm.cnorm <- function(object, ...) { UseMethod("plot.norm.cnorm") }

#' S3 method for plotting information function of continuous norming model of cnorm objects
#'
#' Plots the information criterion - either Cp (default) or BIC - against
#' the adjusted R square of the feature selection in the modeling process.
#' Both BIC and Mallow's Cp are measures to avoid over-fitting. Please
#' choose the model that has a high information criterion, while modeling
#' the original data as close as possible.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.subset
#' @seealso plotSubset
#' @family plot
#' @export
plot.subset.cnorm <- function(object, ...) { UseMethod("plot.subset.cnorm") }

#' S3 method for plotting percentile curves of cnorm objects
#'
#' The function plots the norm curves based on the regression model.
#' Please check the function for inconsistent curves: The different
#' curves should not intersect. Violations of this assumption are a strong
#' indication for violations of model assumptions in modeling the relationship between raw
#' and norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.curves
#' @seealso plotNormCurves
#' @family plot
#' @export
plot.curves.cnorm <- function(object, ...) { UseMethod("plot.curves.cnorm") }

#' S3 method for plotting a series of percentile curves of cnorm objects
#'
#' This functions makes use of 'plotPercentiles' to generate a series of plots
#' with different number of predictors. It draws on the information provided by the model object
#' to determine the bounds of the modeling (age and standard score range). It can be used as an
#' additional model check to determine the best fitting model.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.series
#' @seealso plotPercentileSeries
#' @family plot
#' @export
plot.series.cnorm <- function(object, ...) { UseMethod("plot.series.cnorm") }

#' S3 method for plotting the density percentile curves of cnorm objects
#'
#' The function plots the density  curves based on the regression model against
#' the actual percentiles from the raw data. As in 'plotNormCurves',
#' please check for inconsistent curves, especially curves showing implausible shapes as f. e.
#' violations of biuniqueness.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.density
#' @seealso plotDensity
#' @family plot
#' @export
plot.density.cnorm <- function(object, ...) { UseMethod("plot.density.cnorm") }

#' S3 method for plotting derivatieves of the regression function of cnorm objects
#'
#' Plots the scores obtained via the first order derivative of the regression model
#' in dependence of the norm score. The results indicate the progression of the
#' norm scores within each age group. The regression based modeling approach
#' relies on the assumption of a linear progression of the norm scores.
#' Negative scores in the first order derivative indicate a violation of this
#' assumption. Scores near zero are typical for bottom and ceiling effects in the raw data.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname plot.derivative
#' @seealso plotDerivative
#' @family plot
#' @export
plot.derivative.cnorm <- function(object, ...) { UseMethod("plot.derivative.cnorm") }

#' S3 method for cross validation of cnorm objects
#'
#' This function helps in selecting the number of terms for the model by doing repeated
#' Monte Carlo cross validation with 80 percent of the data as training data and 20 percent as
#' the validation data. The cases are drawn randomly but stratified by norm group. Successive
#' models are retrieved with increasing number of terms and the RMSE of raw scores (fitted by
#' the regression model) is plotted for the training, validation and the complete dataset.
#' Additionally to this analysis on the raw score level, it is possible (default) to estimate
#' the mean norm score reliability and crossfit measures.
#' Please apply to the data (e. g. cv(model$data)) for model selection and to model itself
#' (cv(model)) to estimate precision of predefined regression function.
#' @param object cnorm object or ranked data
#' @param ... Additional parameters
#' @rdname cv
#' @seealso cnorm.cv
#' @family model
#' @export
cv.cnorm <- function(object, ...) {  UseMethod("cnorm.cv") }


#' S3 method for printing model summary of cnorm object
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname summary
#' @family model
#' @export
summary.cnorm <- function(object, ...) {  UseMethod("modelSummary") }

#' S3 method for printing subset information of cnorm object
#'
#' After conducting the model fitting procedure on the data set, the best fitting
#' model has to be chosen. The print function shows the R2 and other information criteria
#' on the different best fitting models with increasing number of predictors.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname print
#' @seealso printSubset
#' @family model
#' @export
print.cnorm <- function(object, ...) {  UseMethod("printSubset") }

#' S3 method for checking the consistency of a cnorm regression model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm scores always have to show a linear increase or decrease with increasing raw
#' scores. Violations of this assumption are a strong indication for problems
#' in modeling the relationship between raw and norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname check
#' @seealso checkConsistency
#' @family model
#' @export
check.cnorm <- function(object, ...) {  UseMethod("checkConsistency") }

#' S3 method for predicting single raw score for a given norm score and age
#'
#' Most elementary function to predict raw score based on Location (L, T score),
#' Age (grouping variable) and the coefficients from a regression model.
#'
#' @param raw A specific raw score
#' @param age The age within the continuous model
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname predict.raw
#' @seealso predictRaw
#' @family predict
#' @export
predict.raw.cnorm <- function(raw, age, object, ...) {  UseMethod("predictRaw") }

#' S3 method for predicting single norm score for a given raw score and age
#'
#' This function conducts this reverse
#' transformation via a numerical solution: A precise norm table is generated and
#' the closest fitting norm score for a raw score is returned.
#'
#' @param norm A specific raw score
#' @param age The age within the continuous model
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname predict.norm
#' @seealso predictNorm
#' @family predict
#' @export
predict.norm.cnorm <- function(norm, age, object, ...) {  UseMethod("predictNorm") }

#' S3 method for creating a raw score table for specific age(s) on the basis of a cnorm object
#'
#' This function generates a norm table for raw scores at specific age(s) based on the regression
#' model by assigning raw scores to norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname table.raw
#' @seealso rawTable
#' @family predict
#' @export
table.raw.cnorm <- function(object, ...) {  UseMethod("rawTable") }

#' S3 method for creating a norm table for specific age(s) on the basis of a cnorm object
#'
#' This function generates a norm table for specific age(s) based on the regression
#' model by assigning raw scores to norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @rdname table
#' @seealso normTable
#' @family predict
#' @export
table.cnorm <- function(object, ...) {  UseMethod("normTable") }

plot <- plotPercentiles
plot.series <- plotPercentileSeries
plot.raw <- plotRaw
plot.norm <- plotNorm
plot.curves <- plotNormCurves
plot.density <- plotDensity
plot.subset <- plotSubset
plot.derivative <- plotDerivative

cv <- cnorm.cv
summary <- summary.cnorm
print <- printSubset
check <- checkConsistency
predict.raw <- predictRaw
predict.norm <- predictNorm
table <- normTable
table.raw <- rawTable
