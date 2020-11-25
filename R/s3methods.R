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
#' @seealso cnorm.cv
#' @family model
#' @export
cv.cnorm <- function(object, ...) {  UseMethod("cv.cnorm") }



summary.cnorm <- function(object, ...) {  UseMethod("summary.cnorm") }
print.cnorm <- function(object, ...) {  UseMethod("print.cnorm") }

#' S3 method for checking the consistency of a cnorm regression model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm scores always have to show a linear increase or decrease with increasing raw
#' scores. Violations of this assumption are a strong indication for problems
#' in modeling the relationship between raw and norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @seealso checkConsistency
#' @family model
#' @export
check.cnorm <- function(object, ...) {  UseMethod("check.cnorm") }

#' S3 method for predicting single raw score for a given norm score and age
#'
#' Most elementary function to predict raw score based on Location (L, T score),
#' Age (grouping variable) and the coefficients from a regression model.
#'
#' @param raw A specific raw score
#' @param age The age within the continuous model
#' @param object cnorm object
#' @param ... Additional parameters
#' @seealso predictRaw
#' @family predict
#' @export
predict.raw.cnorm <- function(raw, age, object, ...) {  UseMethod("predict.raw.cnorm") }

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
#' @seealso predictNorm
#' @family predict
#' @export
predict.norm.cnorm <- function(norm, age, object, ...) {  UseMethod("predict.norm.cnorm") }

#' S3 method for creating a raw score table for specific age(s) on the basis of a cnorm object
#'
#' This function generates a norm table for raw scores at specific age(s) based on the regression
#' model by assigning raw scores to norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @seealso rawTable
#' @family predict
#' @export
table.raw.cnorm <- function(object, ...) {  UseMethod("raw.table.cnorm") }

#' S3 method for creating a norm table for specific age(s) on the basis of a cnorm object
#'
#' This function generates a norm table for specific age(s) based on the regression
#' model by assigning raw scores to norm scores.
#'
#' @param object cnorm object
#' @param ... Additional parameters
#' @seealso normTable
#' @family predict
#' @export
table.cnorm <- function(object, ...) {  UseMethod("table.cnorm") }

plot.cnorm <- plotPercentiles
plot.raw <- plotRaw
plot.norm <- plotNorm
plot.density <- plotDensity
plot.series <- plotPercentileSeries
plot.subset <- plotSubset
plot.derivative <- plotDerivative
plot.curves <- plotNormCurves
cv <- cnorm.cv
summary.cnorm <- summary
print.cnorm <- printSubset
check <- checkConsistency
predict.raw <- predictRaw
predict.norm <- predictNorm
table <- normTable
table.raw <- rawTable
