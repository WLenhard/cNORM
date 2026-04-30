#' cNORM: Continuous Norming
#'
#' The package provides methods for generating regression based continuous standard
#' scores, as f. e. for psychometric test development, biometrics (e. g. physiological
#' growth curves), and screenings in the medical domain. It includes a distribution free approach
#' on the basis of Taylor polynomials and parametric modelling with beta binomial distributions.
#' Both approaches can generate robust norm data models and alleviate the computation of norm scores
#' and norm tables.
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
#' clinical samples. For pre-requisites and use cases of the beta binomial modelling, please
#' consult the vignette 'Beta Binomial'.
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
#' 'ppvt', and 'CDC' sample data from the references). Use
#' \code{model <- cnorm(raw = elfe$raw, group = elfe$group)} to get a first impression.
#' Use  \code{vignette(cNORM-Demo)} for a walk through on
#' conducting  the modeling and \url{https://www.psychometrica.de/cNorm_en.html} for a
#' comprehensive tutorial.
#'
#' @references
#' \enumerate{
#'   \item Center for Disease Control and Prevention (2012).
#'   National Health and Nutrition Examination Survey: Questionnaires,
#'   datasets and related documentation. U.S. Department of Health and Human Services
#'   (original source not available anymore).
#'   \item Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. (2023). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. Behavior Research Methods. https://doi.org/10.3758/s13428-023-02207-0
#'   \item Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the Bias of Norm Scores in Non-Representative Samples: Weighting as an Adjunct to Continuous Norming Methods. Assessment, 30(8), 2491–2509. https://doi.org/10.1177/10731911231153832
#'   \item Lenhard, W., & Lenhard, A. (2021). Improvement of Norm Score Quality via Regression-Based
#'   Continuous Norming. Educational and Psychological Measurement, 81(2), 229–261.
#'   doi: 10.1177/0013164420928457
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests:
#'   A simulation study of parametric and semi-parametric approaches.
#'   PLoS ONE, 14(9),  e0222279. doi: 10.1371/journal.pone.0222279
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to
#'   the norming problem. Assessment, Online first, 1-14. doi: 10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary
#'   Test - Revision IV (German Adaption). Frankfurt a. M.: Pearson Assessment.
#'   \item Lenhard, W. & Schneider, W. (2006). ELFE 1-6 - Ein Leseverstaendnistest fuer Erst- bis
#'   Sechstklässler. Goettingen: Hogrefe.
#' }
#' @author Wolfgang Lenhard, Alexandra Lenhard and Sebastian Gary
#' @keywords Psychometrics Biometrics Test Development Regression Based Norming
#' @docType PACKAGE
#' @name cNORM
#' @seealso cNORM.GUI
#' @examples
#' \dontrun{
#' # Model internal 'elfe' dataset with the default k = 4 regression on T scores
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # Show model fit of models with progressing number of predictors
#' print(results)
#' plot(results, "subset")
#'
#' # Plot manifest and predicted values, plot series of percentile charts
#' plot(results, "raw")
#' plot(results, "series", start = 3, end = 9)
#'
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
#' }
NULL


#' Launcher for the graphical user interface of cNORM for distribution free
#' continuous norming
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
cNORM.GUI <- function(launch.browser = TRUE) {
  packageList <- c("shiny", "shinycssloaders", "foreign", "readxl", "markdown", "rmarkdown")

  # Check which packages are missing
  missing <- packageList[!sapply(packageList, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    cat("Additional packages are needed to start the user interface:\n")
    cat(paste("-", missing, collapse = "\n"), "\n")
    cat("\nWould you like to try to install them now?\n")
    installChoice <- menu(c("yes", "no"))

    if (installChoice == 1) {
      utils::install.packages(missing)
    } else {
      stop("Required packages are missing. Unable to start the GUI")
    }
  }

  appDir <- system.file("shiny", "app1", package = "cNORM")
  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = launch.browser)
}

#' Launch the cNORM Parametric Modeling Shiny Application
#'
#' @param launch.browser Logical, whether to launch browser automatically (default: TRUE)
#' @export
cNORM.GUI2 <- function(launch.browser = TRUE) {

  # Required packages for the parametric modeling GUI
  packageList <- c("shiny", "shinycssloaders", "DT", "ggplot2",
                   "foreign", "readxl", "haven")

  # Check which packages are missing
  missing <- packageList[!sapply(packageList, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    cat("Additional packages are needed to start the parametric modeling interface:\n")
    cat(paste("-", missing, collapse = "\n"), "\n")
    cat("\nWould you like to install them now?\n")
    installChoice <- utils::menu(c("Yes", "No"))

    if (installChoice == 1) {
      cat("\nInstalling packages...\n")
      utils::install.packages(missing)

      # Verify installation succeeded
      still_missing <- missing[!sapply(missing, requireNamespace, quietly = TRUE)]
      if (length(still_missing) > 0) {
        stop("Failed to install: ", paste(still_missing, collapse = ", "),
             "\nPlease install manually and try again.")
      }
      cat("Installation complete!\n\n")
    } else {
      stop("Required packages are missing. Unable to start the GUI.")
    }
  }

  # Locate the app directory
  appDir <- system.file("shiny", "app2", package = "cNORM")

  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `cNORM`.", call. = FALSE)
  }

  # Launch the app
  message("Launching cNORM Parametric Modeling GUI...")
  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = launch.browser)
}

#' Continuous Norming
#'
#' Conducts continuous norming in one step and returns an object including ranked
#' raw data and the continuous norming model. Please consult the function
#' description of 'rankByGroup', 'rankBySlidingWindow' and 'bestModel' for specifics
#' of the steps in the data preparation and modeling process. In addition to the
#' raw scores, either provide
#' \itemize{
#'  \item{a numeric vector for the grouping information (group)}
#'  \item{a numeric age vector and the width of the sliding window (age, width)}
#' }
#' for the ranking of the raw scores. You can
#' adjust the grade of smoothing of the regression model by setting the k and terms
#' parameter. In general, increasing k to more than 4 and the number of terms lead
#' to a higher fit, while lower values lead to more smoothing. The power parameter
#' for the age trajectory can be specified independently by 't'. If both parameters
#' are missing, cnorm uses k = 5 and t = 3 by default.
#'
#' @param raw Numeric vector of raw scores
#' @param group Numeric vector of grouping variable, e.g. grade. If no group
#'   or age variable is provided, conventional norming is applied.
#' @param age Numeric vector with chronological age. If used without `group`,
#'   please additionally specify `width`.
#' @param width Size of the sliding window in case an age vector is used.
#' @param scale Type of norm scale, either "T" (default), "IQ", "z" or
#'   "percentile" (= no transformation); a numeric vector with mean and SD can
#'   also be provided, e.g. `c(10, 3)` for Wechsler scale index points.
#' @param method Ranking method in case of ties; an integer index from 1
#'   (Blom 1958) through 7 (Yu & Huang 2001). Default is 4 (Rankit).
#' @param descend If TRUE, inverts the ranking order so that higher raw scores
#'   receive lower norm scores (e.g. for error scores).
#' @param weights Optional numeric vector of case weights for post-stratification.
#' @param terms If > 0, fix the model to this number of terms.
#' @param R2 Stopping criterion (adjusted R-squared) for model selection.
#' @param k Power degree for the location dimension (max 6).
#' @param t Power degree for the age dimension (max 6).
#' @param plot If TRUE (default), display percentile plot and report.
#' @param extensive If TRUE (default), screen models for monotonic consistency.
#' @param subsampling If TRUE (default), use 10-fold subsampled coefficient
#'   averaging in `bestModel`.
#'
#' @return cnorm object including the ranked raw data and the regression model.
#' @seealso rankByGroup, rankBySlidingWindow, computePowers, bestModel
#' @examples
#' \dontrun{
#' # Conventional norming
#' cnorm(raw = elfe$raw)
#'
#' # Continuous norming via group
#' m1 <- cnorm(raw = elfe$raw, group = elfe$group)
#'
#' # Continuous norming via continuous age + sliding window
#' m2 <- cnorm(raw = ppvt$raw, age = ppvt$age, width = 1)
#'
#' # Norm tables with confidence intervals
#' normTable(c(2.125, 2.375, 2.625), m1, CI = .9, reliability = .95)
#' rawTable (c(2.125, 2.375, 2.625), m1, CI = .9, reliability = .95)
#' }
#' @export
#' @references
#' \enumerate{
#'   \item Gary, S. & Lenhard, W. (2021). In norming we trust. Diagnostica.
#'   \item Gary, S., Lenhard, W. & Lenhard, A. (2021). Modelling Norm Scores with the cNORM Package in R. Psych, 3(3), 501-521. https://doi.org/10.3390/psych3030033
#'   \item Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. (2023). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. Behavior Research Methods. https://doi.org/10.3758/s13428-023-02207-0
#'   \item Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the Bias of Norm Scores in Non-Representative Samples: Weighting as an Adjunct to Continuous Norming Methods. Assessment, 30(8), 2491–2509. https://doi.org/10.1177/10731911231153832
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
                  k = NULL,
                  t = NULL,
                  terms = 0,
                  R2 = NULL,
                  plot = TRUE,
                  extensive = TRUE,
                  subsampling = TRUE) {

  # ------------------------------------------------------------------
  # 1.  Argument sanity
  # ------------------------------------------------------------------
  if (is.null(raw) || !is.numeric(raw)) {
    stop("Please provide a numeric vector for the raw scores.")
  }

  if (!is.null(group) && !is.numeric(group)) {
    stop("`group` must be a numeric vector.")
  }
  if (!is.null(age) && !is.numeric(age)) {
    stop("`age` must be a numeric vector.")
  }

  if (!is.null(group) && length(group) != length(raw)) {
    stop("`raw` and `group` must have the same length.")
  }
  if (!is.null(age) && length(age) != length(raw)) {
    stop("`raw` and `age` must have the same length.")
  }
  if (!is.null(weights) && length(weights) != length(raw)) {
    stop("`weights` must have the same length as `raw`.")
  }

  if (!is.null(group) && !is.null(age)) {
    warning("Specifying both 'group' and 'age' is discouraged; ",
            "the function will use group for ranking and keep age as a covariate.")
  }

  # Default smoothing parameters
  if (is.null(k) && is.null(t)) {
    k <- 5; t <- 3
  } else if (is.null(t)) {
    t <- k
  } else if (is.null(k)) {
    k <- t
  }

  silent <- !plot

  # ------------------------------------------------------------------
  # 2.  Build a single, length-aligned input frame and drop NAs once
  #     (this avoids the "data$age <- age" length-mismatch bug)
  # ------------------------------------------------------------------
  df_in <- data.frame(raw = raw)
  if (!is.null(group))   df_in$group   <- group
  if (!is.null(age))     df_in$age     <- age
  if (!is.null(weights)) df_in$weights <- weights

  df_in <- df_in[complete.cases(df_in), , drop = FALSE]

  if (nrow(df_in) == 0L) {
    stop("After removing missing values no observations remain.")
  }

  # ------------------------------------------------------------------
  # 3.  Decide ranking strategy
  #
  #     Conventional .... raw only
  #     By group .......  group provided                       (group OR age-as-group fallback)
  #     Sliding window .. age + width provided
  # ------------------------------------------------------------------
  conventional <- is.null(group) && is.null(age)
  use_window   <- is.null(group) && !is.null(age) && !is.na(width)
  by_group     <- !is.null(group) ||
    (is.null(group) && !is.null(age) && is.na(width))

  # ------------------------------------------------------------------
  # 3a. Conventional norming
  # ------------------------------------------------------------------
  if (conventional) {
    data <- rankByGroup(raw     = df_in$raw,
                        group   = FALSE,
                        scale   = scale,
                        weights = df_in$weights,
                        descend = descend,
                        method  = method)
    data <- computePowers(data, k = k, t = t, silent = silent)

    model <- bestModel(data,
                       k           = k,
                       t           = t,
                       terms       = terms,
                       R2          = R2,
                       weights     = data$weights,
                       plot        = FALSE,
                       extensive   = extensive,
                       subsampling = subsampling)

    result <- list(data = data, model = model)
    class(result) <- "cnorm"

    if (plot) {
      cat(model$report, sep = "\n")
      print(rawTable(0, result))
      plotPercentiles(result)
    }
    return(result)
  }

  # ------------------------------------------------------------------
  # 3b. Sliding-window ranking (age-driven, no group)
  # ------------------------------------------------------------------
  if (use_window) {
    if (plot) message("Ranking data with sliding window ...")

    data <- rankBySlidingWindow(raw     = df_in$raw,
                                age     = df_in$age,
                                scale   = scale,
                                weights = df_in$weights,
                                descend = descend,
                                width   = width,
                                method  = method)
    data <- data[complete.cases(data), , drop = FALSE]
    data <- computePowers(data, k = k, t = t, age = data$age, silent = silent)

    # ------------------------------------------------------------------
    # 3c. Group-based ranking
    #
    #     If the user supplied an age vector but no width, fall back to
    #     ranking by group. The "group" is either the user's group
    #     vector or a discretisation of age (via getGroups()) when only
    #     age was given.
    #
    #     Important: when the group is *synthesised* from age, ranking
    #     and modelling must operate on the same (discrete) scale,
    #     otherwise the polynomial sees normValue jumps at group
    #     boundaries while age varies smoothly.
    #     We therefore only pass continuous age into computePowers()
    #     when the user supplied a *real* grouping variable in addition
    #     to age (the "discouraged but allowed" combination).
    # ------------------------------------------------------------------
  } else if (by_group) {

    group_was_synthesised <- is.null(group)

    if (group_was_synthesised) {
      if (length(df_in$age) / length(unique(df_in$age)) > 50 &&
          min(table(df_in$age)) > 30) {
        if (plot) message("Width missing. Using age directly as grouping variable.")
        df_in$group <- df_in$age
      } else {
        if (plot) message("Width missing. Discretising age via getGroups().")
        df_in$group <- getGroups(df_in$age)
      }
    }

    data <- rankByGroup(raw     = df_in$raw,
                        group   = df_in$group,
                        scale   = scale,
                        weights = df_in$weights,
                        descend = descend,
                        method  = method)
    data <- data[complete.cases(data), , drop = FALSE]

    if (!group_was_synthesised &&
        !is.null(df_in$age) &&
        nrow(data) == nrow(df_in)) {
      # User supplied a real group AND a separate age vector:
      # rank within group, model with continuous age.
      data$age <- df_in$age
      data <- computePowers(data, k = k, t = t,
                            age = data$age, silent = silent)
    } else {
      # Either no age was provided, or the group is itself a
      # discretisation of age. Use the group column for both
      # ranking and the polynomial age axis.
      data <- computePowers(data, k = k, t = t, silent = silent)
    }
  } else {
    # Should be unreachable given the earlier validation, but guard anyway.
    stop("Please provide a numeric vector for the raw scores and either a ",
         "grouping vector, or an age vector together with a sliding window width.")
  }

  # ------------------------------------------------------------------
  # 4.  Fit and return
  # ------------------------------------------------------------------
  model <- bestModel(data,
                     k           = k,
                     t           = t,
                     R2          = R2,
                     terms       = terms,
                     weights     = data$weights,
                     plot        = FALSE,
                     extensive   = extensive,
                     subsampling = subsampling)

  result <- list(data = data, model = model)
  class(result) <- "cnorm"

  if (plot) {
    cat(model$report, sep = "\n")
    plotPercentiles(result)
  }
  return(result)
}

#' Swiftly compute Taylor regression models for distribution free continuous norming
#'
#' Conducts distribution free continuous norming and aims to find a fitting model. Raw data are modelled as a Taylor polynomial
#' of powers of age and location and their interactions. In addition to the
#' raw scores, either provide a numeric vector for the grouping information (group)
#' for the ranking of the raw scores. You can adjust the grade of smoothing of the regression model by setting the k, t and terms
#' parameter. In general, increasing k and t leads to a higher fit, while lower values lead to more smoothing. If both parameters
#' are missing, taylorSwift uses k = 5 and t = 3 by default.
#'
#' @param raw Numeric vector of raw scores
#' @param group Numeric vector of grouping variable, e. g. grade. If no group
#' or age variable is provided, conventional norming is applied
#' @param age Numeric vector with chronological age, please additionally specify
#' width of window
#' @param width Size of the sliding window in case an age vector is used
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param weights Vector or variable name in the dataset with weights for each
#' individual case. It can be used to compensate for moderate imbalances due to
#' insufficient norm data stratification. Weights should be numerical and positive.
#' @param terms Selection criterion for model building. The best fitting model with
#' this number of terms is used
#' @param R2 Adjusted R square as a stopping criterion for the model building
#' (default R2 = 0.99)
#' @param k The power constant. Higher values result in more detailed approximations
#' but have the danger of over-fit (max = 6). If not set, it uses t and if both
#' parameters are NULL, k is set to 5.
#' @param t The age power parameter (max = 6). If not set, it uses k and if both
#' parameters are NULL, k is set to 3, since age trajectories are most often well
#' captured by cubic polynomials.
#' @param plot Default TRUE; plots the regression model and prints report
#' @param extensive If TRUE, screen models for consistency and - if possible, exclude inconsistent ones
#' @param subsampling If TRUE (default), model coefficients are calculated using 10-folds and averaged across the folds.
#'                    This produces more robust estimates with a slight increase in bias.
#'
#' @return cnorm object including the ranked raw data and the regression model
#' @seealso rankByGroup, rankBySlidingWindow, computePowers, bestModel
#' @examples
#' \dontrun{
#' # Using this function with the example dataset 'ppvt'
#' # You can use the 'getGroups()' function to set up grouping variable in case,
#' # you have a continuous age variable.
#' model <- taylorSwift(raw = ppvt$raw, group = ppvt$group)
#'
#' # return norm tables including 90% confidence intervals for a
#' # test with a reliability of r = .85; table are set to mean of quartal
#' # in grade 3 (children completed 2 years of schooling)
#' normTable(c(5, 15), model, CI = .90, reliability = .95)
#'
#' # ... or instead of raw scores for norm scores, the other way round
#' rawTable(c(8, 12), model, CI = .90, reliability = .95)
#' }
#' @export
#' @references
#' \enumerate{
#'   \item Gary, S. & Lenhard, W. (2021). In norming we trust. Diagnostica.
#'   \item Gary, S., Lenhard, W. & Lenhard, A. (2021). Modelling Norm Scores with the cNORM Package in R. Psych, 3(3), 501-521. https://doi.org/10.3390/psych3030033
#'   \item Lenhard, A., Lenhard, W., Suggate, S. & Segerer, R. (2016). A continuous solution to the norming problem. Assessment, Online first, 1-14. doi:10.1177/1073191116656437
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2018). Continuous Norming (cNORM). The Comprehensive R Network, Package cNORM, available: https://CRAN.R-project.org/package=cNORM
#'   \item Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9),  e0222279. doi:10.1371/journal.pone.0222279
#'   \item Lenhard, W., & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement(Online First), 1-33. https://doi.org/10.1177/0013164420928457
#'
#' }
taylorSwift <- cnorm
