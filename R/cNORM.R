#' cNORM: Continuous Norming with R
#'
#' The package provides methods for generating regression based continuous norms, as f. e.
#' for psychometric test development, biological and physiological growth curves, and
#' screenings in the medical domain. Contrary to parametric approaches, it does not rely on
#' distribution assumptions of the initial norm data and is thus a very robust approach in
#' generating norm tables.
#'
#' Conventional methods for producing test norms are often plagued with "jumps" or "gaps"
#' (i.e., discontinuities) in norm tables and low confidence for assessing extreme scores.
#' cNORM addresses these problems and also has the added advantage of not requiring
#' assumptions about the distribution of the raw data: The norm values are established from
#' raw data by modeling the latter ones as a function  of both percentile scores and an
#' explanatory variable (e.g., age). The method minimizes
#' bias arising from sampling and measurement error, while handling marked deviations from
#' normality - such as are commonplace in clinical samples.
#'
#' Conducting the analysis consists of four steps:
#' \enumerate{
#'   \item Data preparation
#'   \item Establishing the regression model and selecting the parameters
#'   \item Validating the model
#'   \item Generating norm tables and plotting the results
#' }
#'
#' cNORM offers function for all of these steps, helps in selecting the best
#' fitting models and generating the norm tables.
#'
#' @section Functions:
#' \enumerate{
#'   \item Data preparation: rankByGroup, computePowers
#'   \item Regression models: bestModel, regressionFunction, derive
#'   \item Model validation: checkConsistency, plotSubset, plotPercentiles,
#'   plotNormCurves, plotValues, derivationTable, derivationPlot
#'   \item Norm tables: predictNormValue, predictRaw, normTable, getNormCurve
#' }
#' An example dataset is available for demonstration purposes ("elfe"; sample data
#' from the references). Use \code{prepareData()} to load and prepare the results
#' for the modeling. Use \code{vignette(cNORM-Demo)} for a comprehensive
#' explanation for conducting  the modeling.
#'
#' @references Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. doi: 10.1177/1073191116656437
#' @author Wolfgang Lenhard & Alexandra Lenhard
#' @docType package
#' @name cNORM
NULL