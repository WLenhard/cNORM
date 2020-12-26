#' cNORM: Continuous Norming
#'
#' The package provides methods for generating regression based continuous standard
#' scores, as f. e. for psychometric test development, biometrics (e. g. physiological
#' growth curves), and screenings in the medical domain. Contrary to parametric
#' approaches, it does not rely on distribution assumptions of the initial norm data
#' and is thus a very robust approach in generating norm tables.
#'
#' Conventional methods for producing test norm score tables are often plagued with
#' "jumps" or "gaps" (i.e., discontinuities) in norm tables and low confidence for
#' assessing extreme scores.  The continuous norming method introduced by A. Lenhard et
#' al. (2016, <doi:10.1177/1073191116656437>; 2019, <doi:10.1371/journal.pone.0222279>)
#' addresses these problems and also has the added advantage of not requiring assumptions
#' about the distribution of the raw data: The norm scores are established from raw data
#' by modeling the latter ones as a function  of both percentile scores and an explanatory
#' variable (e.g., age). The method minimizes bias arising from sampling and measurement
#' error, while handling marked deviations from normality - such as are commonplace in
#' clinical samples.
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
#'   \item Generating norm tables and predicting scores (\code{\link{predictNorm}},
#'   \code{\link{predictRaw}}, \code{\link{normTable}}, \code{\link{getNormCurve}},
#'   \code{\link{plotNormCurves}})
#' }
#'
#' The function \link{cnorm}
#' For an easy start, you can use the graphical user interface by typing \code{cNORM.GUI()} on the console.
#' Example datasets with large cohorts are available for demonstration purposes ('elfe',
#' 'ppvt', 'CDC', 'life' and 'mortality' sample data from the references). Use
#' \code{model <- cnorm(raw = elfe$raw, group = elfe$group)} to get a first impression.
#' Use  \code{vignette(cNORM-Demo)} for a walk through on
#' conducting  the modeling and \url{https://www.psychometrica.de/cNorm_en.html} for a
#' comprehensive tutorial.
#'
#' @references
#' \enumerate{
#'   \item CDC (2012). National Health and Nutrition Examination Survey: Questionnaires, Datasets
#'   and Related Documentation. available: https://wwwn.cdc.gov/nchs/nhanes/OtherNhanesData.aspx.
#'   date of retrieval: 25/08/2018
#'   \item Harrel, F. (2020). Hmisc: Harrell Miscellaneous (v. 4.4-1). available https://CRAN.R-project.org/package=Hmisc
#'   (code for weighted ranking adapted from wtd.rank & wtd.table by courtesy of Frank Harrell)
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
#' @keywords Psychometrics Biometrics Test Development Regression Based Norming
#' @docType package
#' @name cNORM
#' @seealso cNORM.GUI
#' @examples
#' # Model internal 'elfe' dataset with the default k = 4 regression on T scores
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # Show model fit of models with progressing number of predictors
#' print(results)
#' plot(results, "subset")
#'
#' # Plot manifest and predicted values, plot series of percentile charts
#' plotRaw(results)
#' \dontrun{
#' plot(results, "series", start = 3, end = 9)
#' }
#'
#' # Additional tests: Check model assumptions
#' checkConsistency(results)
#' plot(results, "derivative")
#'
#' # Generate norm tables; predict values, here: grade 3.75 from T score 25
#' # to 75 and within the raw value range of this specific test (0 to 28)
#' normTable <- normTable(3.75, results, minNorm=25, maxNorm=75, step=0.5)
#' rawTable <- rawTable(3.75, results, minRaw = 0, maxRaw = 28, minNorm=25,
#'                      maxNorm=75)
#'
#' # Predict a specific norm score
#' score <- predictNorm(raw = 21, A = 3.75,
#'                           model = results, minNorm=25, maxNorm=75)
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
  packageList <- c("shiny", "shinycssloaders", "foreign", "readxl", "markdown")

  if (!requireNamespace(packageList, quietly = TRUE)) {
    cat("Additional packages are needed to start the user interface. Would you like to try to install them now?")
    installChoice <- menu(c("yes", "no"))
    if(installChoice == 1){
      utils::install.packages(packageList)
    } else {
      stop("Packages are missing. Unable to start the GUI")
    }
  }

  shiny::runApp(system.file('shiny', package='cNORM'),
                launch.browser=TRUE)
}

#' Continuous Norming
#'
#' Conducts continuous norming in one step and returns an object including ranked raw data and the continuous norming
#' model. Please consult the function description ' of 'rankByGroup', 'rankBySlidingWindow' and 'bestModel' for specifics
#' of the steps in the data preparation and modelling process. In addition to the raw scores, either provide
#' \itemize{
#'  \item{a numeric vector for the grouping information (group)}
#'  \item{a numeric vector for both grouping and age (group, age)}
#'  \item{a numeric age vector and the width of the sliding window (age, width)}
#' }
#' for the ranking of the raw scores. You can
#' adjust the grade of smoothing of the regression modell by setting the k and terms parameter. In general,
#' increasing k to more than 4 and the number of terms lead to a higher fit, while lower values lead to more
#' smoothing.
#' @param raw Numeric vector of raw scores
#' @param group Numeric vector of grouping variable, e. g. grade
#' @param age Numeric vector with chronological age, please additionally specify width of window
#' @param width Size of the moving window in case an age vector is used
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param weights Vector or variable name in the dataset with weights to compensate imbalances due to insufficient norm
#' data stratification. All weights have to be numerical and positive. The code to compute weighted percentiles originates from the
#' Hmisc package (functions) wtd.rank and wtd.table) and is provided by the courtesy of Frank Harrell. Please note, that this
#' feature is currently EXPERIMENTAL!
#' @param terms Selection criterion for model building. The best fitting model with
#' this number of terms is used
#' @param R2 Adjusted R square as a stopping criterion for the model building
#' (default R2 = 0.99)
#' @param k The power constant. Higher values result in more detailed approximations
#' but have the danger of over-fit (default = 4, max = 6)
#'
#' @return cnorm object including the ranked raw data and the regression model
#' @seealso rankByGroup, rankBySlidingWindow, computePowers, bestModel
#' @examples
#'
#' # Using this function with the example dataset 'elfe'
#' cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # return norm tables including 90% confidence intervals for a
#' # test with a reliability of r = .85; table are set to mean of quartal
#' # in grade 3 (children completed 2 years of schooling)
#' normTable(c(2.125, 2.375, 2.625, 2.875), cnorm.elfe, CI = .90, reliability = .95)
#'
#' # ... or instead of raw scores for norm scores, the other way round
#' rawTable(c(2.125, 2.375, 2.625, 2.875), cnorm.elfe, CI = .90, reliability = .95)
#'
#'
#' # Not really a plausible scenario, but just for demonstration purposes, we will
#' # use the PPVT dataset and sex as the weighting variable (1 = male, 2 = female),
#' # and consequently, females will get the double weight. This procedure can be used
#' # to correct imbalances in the dataset, but it is still experimental. Please use
#' # positive, non-zero numerics, preferably integers for this:
#' \dontrun{
#' cnorm.ppvt <- cnorm(raw = ppvt$raw, group = ppvt$group, weight = ppvt$sex)
#' }
#'
#' @export
#' @references
#' \enumerate{
#'   \item Gary, S. & Lenhard, W. (2021). In norming we trust. Diagnostica.
#'   \item Harrel, F. (2020). Hmisc: Harrell Miscellaneous (v. 4.4-1). available https://CRAN.R-project.org/package=Hmisc
#'   (code for weighted ranking adapted from wtd.rank & wtd.table by courtesy of Frank Harrell)
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. doi:10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2018). Continuous Norming (cNORM). The Comprehensive R Network, Package cNORM, available: https://CRAN.R-project.org/package=cNORM
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9),  e0222279. doi:10.1371/journal.pone.0222279
#'   \item Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement(Online First), 1-33. https://doi.org/10.1177/0013164420928457


#' }
cnorm <- function(raw = NULL,
                  group = NULL,
                  age = NULL,
                  width = NA,
                  weights = NULL,
                  scale = "T",
                  method = 4,
                  descend = FALSE,
                  k = 4,
                  terms = 0,
                  R2 = NULL){

  if(!is.null(weights)){
    warning("Weighting is still an experimental feature. It is currently not yet encouraged to use it.")
  }

  if(is.numeric(raw)&&is.numeric(group)){
    if(length(raw)!=length(group)){
      stop("Please provide numeric vectors of equal length for raw score and group data.")
    }

    if(is.numeric(age)&&!is.na(width)){
      if(length(raw)!=length(age)){
        stop("Please provide numeric vectors of equal length for raw score and group data.")
      }

      message("Ranking data with sliding window ...")
      data <- rankBySlidingWindow(raw=raw, age=age, scale=scale, weights=weights, descend = descend, width = width, method = method)
      data <- computePowers(data, k = k)
    }else{
      data <- rankByGroup(raw=raw, group=group, scale=scale, weights=weights, descend = descend, method = method)
    }
    if(is.numeric(age)){
      if(length(raw)!=length(age)){
        warning("Length of the age vector does not match the raw score vector, ignoring age information.")
        data <- computePowers(data, k = k)
      }else{
        data$age <- age
        data <- computePowers(data, k = k, age = age)
      }
    }else{
      data <- computePowers(data, k = k)
    }
  }

  # if no grouping variable is given
  else if(is.numeric(raw)&&is.numeric(age)&&!is.na(width)){
    if(length(raw)!=length(age)){
      stop("Please provide numeric vectors of equal length for raw score and group data.")
    }

    message("Ranking data with sliding window ...")
    data <- rankBySlidingWindow(raw=raw, age=age, scale=scale, weights=weights, descend = descend, width = width, method = method)
    data <- computePowers(data, k = k)
  }else{
    stop("Please provide a numerical vector for the raw scores and either a vector for grouping and/or age of the same length. If you use an age vector only, please specify the width of the window.")
  }

  model <- bestModel(data, R2=R2, terms=terms, weights = weights)
  result <- list(data = data, model = model)
  class(result) <- "cnorm"
  return(result)
}
