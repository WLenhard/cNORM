#' cNORM: Continuous Norming with R
#'
#' The package provides methods for generating regression based continuous standard
#' scores, as f. e. for psychometric test development, biometrics (e. g. physiological
#' growth curves), and screenings in the medical domain. Contrary to parametric
#' approaches, it does not rely on distribution assumptions of the initial norm data
#' and is thus a very robust approach in generating norm tables.
#'
#' Conventional methods for producing test norm score tables are often plagued with
#' "jumps" or "gaps" (i.e., discontinuities) in norm tables and low confidence for
#' assessing extreme scores. cNORM addresses these problems and also has the added
#' advantage of not requiring assumptions about the distribution of the raw data:
#' The norm scores are established from raw data by modeling the latter ones as a
#' function  of both percentile scores and an explanatory variable (e.g., age). The
#' method minimizes bias arising from sampling and measurement error, while handling
#' marked deviations from normality - such as are commonplace in clinical samples.
#'
#' Conducting the analysis consists of four steps and cNORM offers all according functions
#' for preparing data, conducting the  regression, selecting the best model and generating
#' norm tables (according functions in brackets):
#' \enumerate{
#'   \item Data preparation (\code{\link{rankByGroup}}, \code{\link{rankBySlidingWindow}},
#'   \code{\link{computePowers}})
#'   \item Establishing the regression model and selecting the parameters (\code{\link{bestModel}},
#'   \code{\link{printSubset}}, \code{\link{plotSubset}}, \code{\link{regressionFunction}},
#'   \code{\link{derive}})
#'   \item Validating the model (\code{\link{checkConsistency}}, \code{\link{plotPercentiles}},
#'   \code{\link{plotPercentileSeries}}, \code{\link{plotRaw}}, \code{\link{plotNorm}}, \code{\link{derivationTable}},
#'   \code{\link{plotDerivative}})
#'   \item Generating norm tables and predicting scores (\code{\link{predictNormValue}},
#'   \code{\link{predictRaw}}, \code{\link{normTable}}, \code{\link{getNormCurve}},
#'   \code{\link{plotNormCurves}})
#' }
#'
#' Example datasets with large cohorts are available for demonstration purposes ('elfe',
#' 'ppvt', 'CDC', 'life' and 'mortality' sample data from the references). Use
#' \code{data <- prepareData(elfe)} or \code{data <- prepareData(ppvt)} to load and prepare
#' example data for the modeling. Use  \code{vignette(cNORM-Demo)} for a walkthrough on
#' conducting  the modeling and \url{https://www.psychometrica.de/cNorm_en.html} for a
#' comprehensive tutorial.
#'
#' @references
#' \enumerate{
#'   \item CDC (2012). National Health and Nutrition Examination Survey: Questionaires, Datasets
#'   and Related Documentation. available: https://wwwn.cdc.gov/nchs/nhanes/OtherNhanesData.aspx.
#'   date of retrieval: 25/08/2018
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to
#'   the norming problem. Assessment, Online first, 1-14. doi: 10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary
#'   Test - Revision IV (German Adaption). Frankfurt a. M.: Pearson Assessment.
#'   \item Lenhard, W. & Schneider, W. (2006). ELFE 1-6 - Ein Leseverstaendnistest fuer Erst- bis
#'   Sechstklässler. Goettingen: Hogrefe.
#'   \item The World Bank (2018). Mortality rate, infant (per 1,000 live births). Data Source
#'   available https://data.worldbank.org/indicator/SP.DYN.IMRT.IN (date of retrieval: 02/09/2018)
#'   \item The World Bank (2018). Life expectancy at birth, total (years). Data Source World
#'   Development Indicators available https://data.worldbank.org/indicator/sp.dyn.le00.in
#'   (date of retrieval: 01/09/2018)
#' }
#' @author Wolfgang Lenhard, Alexandra Lenhard and Sebastian Gary
#' @keywords Psychometrics, Biometrics, Test Development, Regression Based Norming
#' @docType package
#' @name cNORM
#' @seealso cNORM.GUI
#' @examples
#' # Model internal 'elfe' dataset with the default k = 4 regression on T scores
#' data.elfe <- prepareData(elfe)
#' model.elfe <- bestModel(data.elfe)
#' plotPercentiles(data.elfe, model.elfe)
#'
#' # Show model fit of models with progressing number of predictors
#' printSubset(model.elfe)
#' plotSubset(model.elfe)
#'
#' # Plot manifest and predicted values, plot series of percentile charts
#' plotRaw(data.elfe, model.elfe)
#' \dontrun{
#' plotPercentileSeries(data.elfe, model.elfe)
#' }
#'
#' # Additional tests: Check model assumptions
#' checkConsistency(model.elfe)
#' plotDerivative(model.elfe)
#'
#' # Generate norm tables; predict values, here: grade 3.75 from T score 25
#' # to 75 and within the raw value range of this specific test (0 to 28)
#' normTable <- normTable(3.75, model.elfe, minNorm=25, maxNorm=75, step=0.5)
#' rawTable <- rawTable(3.75, model.elfe, minRaw = 0, maxRaw = 28, minNorm=25,
#'                      maxNorm=75)
#'
#' # Predict a specific norm score
#' score <- predictNormValue(raw = 21, A = 3.75,
#'                           model = model.elfe, minNorm=25, maxNorm=75)
#'
#' # Semi-parametric modelling with Box Cox power transformation for grade 3.75
#' bcParameters <- boxcox(model.elfe, 3.75)
#' # Print L, M and S
#' bcParameters$lambdaBC
#' bcParameters$meanBC
#' bcParameters$sdBC
#' # Plot density function of box cox versus regression model
#' plotBoxCox(model.elfe, bcParameters, type=2)
NULL


#' Launcher for the graphical user interface of cNORM
#'
#' @param launch.browser Default TRUE; automatically open browser for GUI
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch graphical user interface
#' cNORM.GUI()
#' }
cNORM.GUI <- function(launch.browser=TRUE){
  if (!requireNamespace(c("shiny", "foreign", "readxl", "shinythemes"), quietly = TRUE)) {
    stop("Packages \"shiny\", \"shinythemes\", \"foreign\" and \"readxl\" are needed for this function to work. Please install them.",
         call. = FALSE)
  }

  shiny::runApp(system.file('shiny', package='cNORM'),
                launch.browser=TRUE)
}
