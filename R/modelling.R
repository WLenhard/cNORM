#' Retrieve the best fitting regression model based on powers of A, L and interactions
#'
#' The function computes a series of regressions with an increasing number of predictors and
#' takes the best fitting model per step. The aim is to find a model with as few predictors
#' as possible, which at the same time manages to explain as much variance as possible from
#' the original data. In psychometric test construction, this approach can be used to smooth
#' the data and eliminate noise from norm sample stratification, while preserving the overall
#' diagnostic information. Values around R2 = .99 usually show excellent results. The selection
#' of the model can either be based on the number of terms in the regression functions or the
#' share of explained variance of the model (R2). If both are specified, first the method tries
#' to select the model based on the number of terms and in case, this does not work, use R2
#' instead. Pushing R2 by setting the number of terms, the R2 cut off and k to high values
#' might lead to on over-fit, so be careful! These parameters depend on the distribution of
#' the norm data. As a rule of thumb, terms = 5 or R2 = .99 and k = 4 is a good starting point
#' for the analyses.
#' \code{plotSubset(model)} can be used to weigh up R2 and information criteria (Cp, an AIC like measure)
#' and fitted versus manifest scores can be plotted with 'plotRaw', 'plotNorm' and 'plotPercentiles'.
#' Use \code{checkConsistency(model)} to check the model for violations. \code{cnorm.cv} can help
#' in identifying the ideal number of predictors.
#'
#' @param data The preprocessed dataset, which should include the variables 'raw'
#'  and the powers and interactions of the norm score (L = Location; usually T scores)
#'  and an explanatory variably (usually age = A)
#' @param raw the name of the raw score variable (default raw)
#' @param terms Selection criterion for model building. The best fitting model with
#' this number of terms is used
#' @param R2 Adjusted R square as a stopping criterion for the model building
#' (default R2 = 0.99)
#' @param k The power constant. Higher values result in more detailed approximations
#' but have the danger of over-fit (default = 4, max = 6)
#' @param t the age power parameter (default NULL). If not set, cNORM automatically uses k. The age power parameter
#' can be used to specify the k to produce rectangular matrices and specify the course of scores per independently from k
#' @param predictors List of the names of predictors or regression formula to use for the model selection.
#' The parameter overrides the 'k' parameter and it can be used to preselect the
#' variables entering the regression, or even to add variables like sex, that are
#' not part of the original model building. Please note, that adding other variables
#' than those based on L and A, plotting, prediction and normTable function will most
#' likely not work, but at least the regression formula can be obtained that way.
#' The parameter as well accepts a formula object, f. e. when applying a pre computed
#' model to a new dataset. In this case, k is as well overridden. In order to include all
#' predictors in the regression, you might want to adjust the terms parameter to the number
#' of predictors as well.
#' @param force.in List of variable names forced into the regression function. This option
#' can be used to force the regression to include covariates like sex or other background
#' variables. This can be used to model separate norm scales for different groups in order
#' the sample. Variables specified here, that are not part of the initial regression function
#' resp. list of predictors, are ignored without further notice and thus do not show up in
#' the final result. Additionally, all other functions like norm table generation and plotting
#' are so far not yet prepared to handle covariates.
#' @param weights Optional vector with weights for the single cases. By default, if data has been
#' weighting in ranking, these weights are reused here as well. Please set to FALSE to deactivate
#' this behavior. All weights have to be positive and no missings are allowed. Otherwise
#' the weights will be ignored.
#' @param plot If set to TRUE (default), the percentile plot of the model is shown
#' @return The model meeting the R2 criteria with coefficients and variable selection
#' in model$coefficients. Use \code{plotSubset(model)} and
#' \code{plotPercentiles(data, model)} to inspect model
#' @examples
#' \dontrun{
#' # Standard example with sample data
#' normData <- prepareData(elfe)
#' model <- bestModel(normData)
#' plotSubset(model)
#' plotPercentiles(normData, model)
#'
#' # It is possible to specify the variables explicitly - useful to smuggle
#' # in variables like sex
#' preselectedModel <- bestModel(normData, predictors = c("L1", "L3", "L1A3", "A2", "A3"))
#' print(regressionFunction(preselectedModel))
#'
#' # Example for modeling based on continuous age variable and raw variable,
#' # based on the CDC data. We use the default k=4 parameter; raw variable has
#' # to be set to "bmi".
#' bmi.data <- prepareData(CDC, raw = "bmi", group = "group", age = "age")
#' bmi.model <- bestModel(bmi.data, raw = "bmi")
#' printSubset(bmi.model)
#'
#' # Use the formula of the pre calculated bmi data to compute models for girls and
#' # boys seperately
#' bmi.model.boys <- bestModel(bmi.data[bmi.data$sex == 1, ], predictors = bmi.model$terms)
#' bmi.model.girls <- bestModel(bmi.data[bmi.data$sex == 2, ], predictors = bmi.model$terms)
#'
#'
#' # Custom list of predictors (based on k = 3) and forcing in the sex variable
#' # While calculating the regression model works well, all other functions like
#' # plotting and norm table generation are not yet prepared to use covariates
#' bmi.sex <- bestModel(bmi.data, raw = "bmi", predictors = c(
#'   "L1", "L2", "L3",
#'   "A1", "A2", "A3", "L1A1", "L1A2", "L1A3", "L2A1", "L2A2",
#'   "L2A3", "L3A1", "L3A2", "L3A3", "sex"
#' ), force.in = c("sex"))
#' }
#' @seealso plotSubset, plotPercentiles, plotPercentileSeries, checkConsistency
#' @export
#' @family model
bestModel <- function(data,
                      raw = NULL,
                      R2 = NULL,
                      k = NULL,
                      t = NULL,
                      predictors = NULL,
                      terms = 0,
                      weights = NULL,
                      force.in = NULL,
                      plot = TRUE) {
  # retrieve attributes
  if (is.null(raw)) {
    raw <- attr(data, "raw")
  }

  if (!is.null(weights)) {
    if (is.numeric(weights) && length(weights) == nrow(data)) {
      data$weights <- weights
      attr(data, "weights") <- "weights"
    } else{
      weights <- NULL
    }
  }

  if (is.null(k)) {
    k <- attr(data, "k")
  } else if (k > attr(data, "k")) {
    warning(
      paste0(
        "k parameter exceeds the power degrees in the dataset. Setting to default of k = ",
        attr(data, "k")
      )
    )
    k <- attr(data, "k")
  }



  if (is.null(t)) {
    if (is.null(attr(data, "t"))) {
      t <- 3
    } else if (!is.null(attr(data, "t"))) {
      t <- attr(data, "t")
    }
  }


  # check variable range
  if (!is.null(R2) && (R2 <= 0 || R2 >= 1)) {
    warning("R2 parameter out of bounds. Setting to default R2 = .99")
    R2 <- .99
  }

  if (terms < 0) {
    warning("terms parameter out of bounds. The value has to be positive. Setting to 4.")
    terms <- 4
  }

  if ((k < 1 || k > 6) & is.null(predictors)) {
    warning(
      "k parameter out of bounds. Please specify a value between 1 and 6. Setting to default = 5."
    )
    k <- 5
  }

  if (!(raw %in% colnames(data)) &&
      (!inherits(predictors, "formula"))) {
    stop(paste(
      c(
        "ERROR: Raw value variable '",
        raw,
        "' does not exist in data object."
      ),
      collapse = ""
    ))
  }

  if ((!is.null(predictors)) &&
      (!inherits(predictors, "formula")) &&
      (!(predictors %in% colnames(data)))) {
    stop("ERROR: Missing variables from predictors variable. Please check variable list.")
  }

  # set up regression function
  if (is.null(predictors)) {
    useCOV <- !is.null(attr(data, "covariate"))
    useAge <- attr(data, "useAge")
    lmX <-
      buildFunction(
        raw = raw,
        k = k,
        t = t,
        age = useAge,
        covariates = useCOV
      )
  } else {
    if (inherits(predictors, "formula")) {
      lmX <- predictors
    } else {
      lmX <-
        formula(paste(raw, paste(predictors, collapse = " + "), sep = " ~ "))
    }
  }

  big <- FALSE
  nvmax <- (t + 1) * (k + 1) - 1 + length(predictors)

  if (nvmax > 25) {
    big <- TRUE
    message("The computation might take some time ...")
  }


  if (!is.null(force.in)) {
    c <- strsplit(format(paste0(lmX))[[3]], " \\+ ")
    index <- match(force.in, c[[1]])
  } else {
    index <- NULL
  }


  # rename variable, otherwise it would be automatically used
  if (is.null(weights) && !is.null(data$weights)) {
    data$weights.old <- data$weights
    data$weights <- NULL
  }

  # determine best subset
  if (is.null(weights))
    subsets <-
    regsubsets(
      lmX,
      data = data,
      nbest = 1,
      nvmax = nvmax,
      force.in = index,
      really.big = big
    )
  else
    subsets <-
    regsubsets(
      lmX,
      data = data,
      nbest = 1,
      nvmax = nvmax,
      force.in = index,
      really.big = big,
      weights = weights
    )

  results <- base::summary(subsets)
  results$numberOfTerms <- as.numeric(rowSums(results$which) - 1)

  i <- 1
  rAdj <- results$adjr2[i]

  if (is.null(R2) && (terms == 0)) {
    if (results$adjr[[length(results$adjr2)]] > .99) {
      R2 <- .99
    } else if (nvmax > 4) {
      R2 <- results$adjr[[5]]
    } else {
      R2 <- results$adjr[[nvmax]]
    }
  }

  if (terms > 0 && terms <= length(results$adjr2)) {
    i <- terms
    report <- paste0("User specified solution: ", i, " terms")
  } else {
    # check upper and lower bounds and cycle through R2 list
    if (terms > 0) {
      message("\n\nCould not determine best model based of number of terms, using R2 instead.")
    }
    if (R2 < results$adjr2[i]) {
      report <- paste0(
        "Specified R2 falls below the value of the most primitive model. Falling back to model 1."
      )
    } else if (results$adjr2[length(results$adjr2)] < R2) {
      i <- length(results$adjr2)
      report <- (
        paste0(
          "Specified R2 exceeds the R2 of the model with the highest fit. Consider reducing the R2 or fixing the number of terms (e.g. 4 to 10). You can use the plotSubset function to find a good balance between number of terms and R2. Look out for an 'elbow' in the information function or use the cnorm.cv function to determine the optimal number of terms. Falling back to model ",
          i
        )
      )
    } else {
      while (rAdj < R2) {
        i <- i + 1
        rAdj <- results$adjr2[i]
      }
      report <- paste0("Final solution: ", i, " terms")
    }
  }
  report[2] <-
    paste0("R-Square Adj. = ", round(results$adjr2[i], digits = 6))


  variables <- colnames(results$outmat)[results$outmat[i,] == "*"]
  text <-
    paste0(raw, " ~ ", paste(variables, collapse = " + ")) # build regression formula

  report[3] <- paste0("Final regression model: ", text)

  if (is.null(attr(data, "weights")))
    bestformula <- lm(text, data)
  else
    bestformula <- lm(text, data, weights = data$weights)

  if (!is.null(attr(data, "covariate"))) {
    if (length(grep("COV", names(bestformula$coefficients))) == 0)
      stop(
        "No covariate term included in the regression result. The covariates turned out to be irrelevant under the current configuration. No model generated on the basis of the current dataset. Either rerun the bestModel function with different numbers of terms or R2, or do not specify a covariate when ranking the data."
      )
  }

  # compute rmse
  tab <-
    data.frame(raw = data[, raw], fitted = bestformula$fitted.values)
  tab <- tab[complete.cases(tab), ]
  rmse <- sqrt(sum((tab$raw - tab$fitted) ^ 2) / length(tab$raw))

  # Model information
  bestformula$ideal.model <- i
  bestformula$cutoff <- R2
  bestformula$subsets <- results

  # add information for horizontal and vertical extrapolation
  if (attr(data, "useAge")) {
    bestformula$useAge <- TRUE
  } else{
    bestformula$useAge <- FALSE
  }

  # conventional norming
  if (is.null(data$A1)) {
    bestformula$minA1 <- 0
    bestformula$maxA1 <- 0

  }
  # continuous norming
  else{
    bestformula$minA1 <- min(data$A1)
    bestformula$maxA1 <- max(data$A1)
  }

  bestformula$minL1 <- min(data$L1)
  bestformula$maxL1 <- max(data$L1)
  bestformula$minRaw <- min(data[, raw])
  bestformula$maxRaw <- max(data[, raw])
  bestformula$raw <- raw
  bestformula$rmse <- rmse
  bestformula$scaleSD <- attributes(data)$scaleSD
  bestformula$scaleM <- attributes(data)$scaleM
  bestformula$descend <- attributes(data)$descend
  bestformula$group <- attributes(data)$group
  bestformula$age <- attributes(data)$age
  bestformula$k <- attributes(data)$k
  bestformula$A <- attributes(data)$A
  if (!is.null(attr(data, "covariate"))) {
    bestformula$covariate <- attributes(data)$covariate
  }

  # Print output
  report[4] <-
    paste0("Regression function: ",
           regressionFunction(bestformula, digits = 10))
  report[5] <- paste0("Raw Score RMSE = ", round(rmse, digits = 5))
  if (!is.null(weights)) {
    report[6] <-
      paste0(
        "Post stratification was applied. The weights range from ",
        round(min(weights), digits = 3),
        " to ",
        round(max(weights), digits = 3),
        " (m = ",
        round(mean(weights), digits = 3),
        ", sd = ",
        round(sd(weights), digits = 3),
        ")."
      )
  }

  bestformula$report <- report
  cat(report, sep = "\n")

  if (anyNA(bestformula$coefficients)) {
    warning(
      "The regression contains missing coefficients. No fitting model could be found. Please try a different number of terms."
    )
  }

  if (terms > 15) {
    message(
      "\nThe model includes a high number of terms. Simpler models are usually more robust. Cross validation with 'cv(model$data)' or an inspection of information functions with 'plot.subset' might help to identify a balanced number of terms. Consider fixing this parameter to a smaller number."
    )
  }

  if (!is.null(data$A1)) {
    message(
      "\nUse 'printSubset(model)' to get detailed information on the different solutions, 'plotPercentiles(model) to display percentile plot, plotSubset(model)' to inspect model fit."
    )
  } else{
    message(
      "\nConventional norming was applied. Use 'normTable(0, model)' or 'rawTable(0, model)' to retrieve norm scores. If you would like to achieve a closer fit, increase the terms parameter."
    )
  }

  plotPercentiles(data, bestformula)

  return(bestformula)
}


#' Convenience method for printing model selection information
#'
#' After conducting the model fitting procedure on the data set, the best fitting
#' model has to be chosen. The print function shows the R2 and other information
#' on the different best fitting models with increasing number of predictors.
#' @param x The model from the 'bestModel' function or a cnorm object
#' @param ... additional parameters
#' @return A table with information criteria
#' @export
#'
#' @examples
#' # Generate cnorm object from example data
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' printSubset(result)
#' @family model
printSubset <- function(x, ...) {
  if (inherits(x, "cnorm")) {
    x <- x$model
  }

  # compute F and significance
  RSS1 <- c(NA, x$subsets$rss)
  RSS2 <- c(x$subsets$rss, NA)
  k1 <- seq(from = 1, to = length(x$subsets$rss) + 1)
  k2 <- seq(from = 2, to = length(x$subsets$rss) + 2)
  df1 <- k2 - k1
  df2 <- length(x$fitted.values) - k2
  F <- ((RSS1 - RSS2) / df1) / (RSS2 / df2)
  p <- 1 - pf(F, df1, df2)
  table <- data.frame(
    R2adj = x$subsets$adjr2,
    BIC = x$subsets$bic,
    CP = x$subsets$cp,
    RSS = x$subsets$rss,
    RMSE = sqrt(x$subsets$rss / length(x$fitted.values)),
    DeltaR2adj = head(c(x$subsets$adjr2, NA) - c(NA, x$subsets$adjr2), -1),
    F = head(F, -1),
    p = head(p, -1),
    nr = seq(1, length(x$subsets$adjr2), by = 1)
  )
  return(table)
}

#' Check the consistency of the norm data model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm scores always have to show a linear increase or decrease with increasing raw
#' scores. Violations of this assumption are a strong indication for problems
#' in modeling the relationship between raw and norm scores. There are
#' several reasons, why this might occur:
#' \enumerate{
#'   \item Vertical extrapolation: Choosing extreme norm scores, e. g. values
#'   -3 <= x and x >= 3 In order to model these extreme values, a large sample
#'   dataset is necessary.
#'   \item Horizontal extrapolation: Taylor polynomials converge in a certain
#'   radius. Using the model values outside the original dataset may
#'   lead to inconsistent results.
#'   \item The data cannot be modeled with Taylor polynomials, or you need
#'   another power parameter (k) or R2 for the model.
#'  }
#'  In general, extrapolation (point 1 and 2) can carefully be done to a
#'  certain degree outside the original sample, but it should in general
#'  be handled with caution.
#'
#' @param model The model from the bestModel function or a cnorm object
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param stepAge Stepping parameter for the age check, usually 1 or 0.1; lower
#' values indicate higher precision / closer checks
#' @param minNorm Lower end of the norm value range
#' @param maxNorm Upper end of the norm value range
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param stepNorm Stepping parameter for the norm table check within age with lower
#' scores indicating a higher precision. The choice depends of the norm scale
#' used. With T scores a stepping parameter of 1 is suitable
#' @param warn If set to TRUE, already minor violations of the model assumptions
#' are displayed (default = FALSE)
#' @param silent turn off messages
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @return Boolean, indicating model violations (TRUE) or no problems (FALSE)
#' @examples
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' modelViolations <- checkConsistency(result,
#'   minAge = 2, maxAge = 5, stepAge = 0.1,
#'   minNorm = 25, maxNorm = 75, minRaw = 0, maxRaw = 28, stepNorm = 1
#' )
#' plotDerivative(result, minAge = 2, maxAge = 5, minNorm = 25, maxNorm = 75)
#' @export
#' @family model
checkConsistency <- function(model,
                             minAge = NULL,
                             maxAge = NULL,
                             minNorm = NULL,
                             maxNorm = NULL,
                             minRaw = NULL,
                             maxRaw = NULL,
                             stepAge = 1,
                             stepNorm = 1,
                             warn = FALSE,
                             silent = FALSE,
                             covariate = NULL) {
  if (inherits(model, "cnorm")) {
    model <- model$model
  }

  if (!is.null(covariate) && is.null(model$covariate)) {
    warning(
      "Covariate specified but no covariate available in the model. Setting covariate to NULL."
    )
    covariate = NULL
  } else if (is.null(covariate) && !is.null(model$covariate)) {
    stop("Covariate specified in the model, but no function parameter available.")
  }

  if (is.null(minAge)) {
    minAge <- model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- model$maxA1
  }

  if (is.null(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- model$maxL1
  }

  if (is.null(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- model$maxRaw
  }
  descend <- model$descend

  i <- minAge
  major <- 0
  results <- c()

  while (i <= maxAge) {
    norm <-
      normTable(
        i,
        model,
        minNorm = minNorm,
        maxNorm = maxNorm,
        minRaw = minRaw,
        maxRaw = maxRaw,
        step = stepNorm,
        covariate = covariate,
        monotonuous = FALSE
      )
    correct <- TRUE
    if (descend)
      correct <- !is.unsorted(-norm$raw)
    else
      correct <- !is.unsorted(norm$raw)

    if (!correct) {
      if (!silent) {
        message(paste0(
          "Violation of monotonicity at age ",
          round(i, digits = 1),
          "."
        ))
      }
      results <-
        c(results,
          paste0("Violation of monotonicity at age ", round(i, digits = 1), "."))
      major <- major + 1
    }

    i <- i + stepAge
  }

  if (major == 0) {
    if (!silent) {
      message("\nNo violations of model consistency found.")
    }
    return(FALSE)
  } else {
    if (!silent) {
      message(
        paste0(
          "\nAt least ",
          major,
          " violations of monotonicity found within the specified range of age and norm score.",
          "Use 'plotNormCurves' to visually inspect the norm curve or 'plotDerivative' to ",
          "identify regions violating the consistency. ",
          "Rerun the modeling with adjusted parameters or restrict the valid value range accordingly. ",
          "Be careful with horizontal and vertical extrapolation."
        )
      )
      message(rangeCheck(model, minAge, maxAge, minNorm, maxNorm))
    }
    return(TRUE)
  }
}



#' Regression function
#'
#' The method builds the regression function for the regression model,
#' including the beta weights.
#' It can be used to predict the raw scores based on age and location.
#' @param model The regression model from the bestModel function or a cnorm object
#' @param raw The name of the raw value variable (default 'raw')
#' @param digits Number of digits for formatting the coefficients
#' @return The regression formula as a string
#'
#' @examples
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' regressionFunction(result)
#' @export
#' @family model
regressionFunction <- function(model, raw = NULL, digits = NULL) {
  if (inherits(model, "cnorm")) {
    raw <- "raw"
    model <- model$model
  } else{
    if (is.null(raw)) {
      raw <- model$raw
    }
  }

  i <- 2
  if (is.null(digits)) {
    formulA <- paste(raw, model$coefficients[[1]], sep = " ~ ")
    while (i <= length(model$coefficients)) {
      formulA <- paste0(formulA,
                        " + (",
                        model$coefficients[[i]],
                        "*",
                        names(model$coefficients[i]),
                        ")")
      i <- i + 1
    }
  } else {
    formulA <-
      paste(raw, format(model$coefficients[[1]], digits = digits), sep = " ~ ")
    while (i <= length(model$coefficients)) {
      formulA <- paste0(
        formulA,
        " + (",
        format(model$coefficients[[i]], digits = digits),
        "*",
        names(model$coefficients[i]),
        ")"
      )
      i <- i + 1
    }
  }
  return(formulA)
}

#' Derivative of regression model
#'
#' Calculates the derivative of the location / norm value from the regression model with the first
#' derivative as the default. This is useful for finding violations of model assumptions and problematic
#' distribution features as f. e. bottom and ceiling effects, non-progressive norm scores within an
#' age group or in general #' intersecting percentile curves.
#' @param model The regression model or a cnorm object
#' @param order The degree of the derivate, default: 1
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @return The derived coefficients
#' @examples
#' normData <- prepareData(elfe)
#' m <- bestModel(normData)
#' derivedCoefficients <- derive(m)
#' @export
#' @family model
derive <- function(model,
                   order = 1,
                   covariate = NULL) {
  if (inherits(model, "cnorm")) {
    model <- model$model
  }

  if (!is.null(covariate) && is.null(model$covariate)) {
    warning(
      "Covariate specified but no covariate available in the model. Setting covariate to NULL."
    )
    covariate = NULL
  } else if (is.null(covariate) && !is.null(model$covariate)) {
    stop("Covariate specified in the model, but no function parameter available.")
  }

  coeff <- model$coefficients[grep("L", names(model$coefficients))]

  if (!is.null(covariate)) {
    coef <-
      simplifyCoefficients(coefficients = coeff, covariate = covariate)
  }

  for (o in 1:order) {
    if (o > 1) {
      coeff <- coeff[grep("L", names(coeff))]
    }
    i <- 1
    name <- names(coeff)
    # easy, straight forward derivation of betas and variable names
    while (i <= length(coeff)) {
      nam <- strsplit(name[[i]], "")

      if (nam[[1]][1] == "L") {
        coeff[[i]][1] <- coeff[[i]][1] * as.numeric(nam[[1]][2])
      }
      nam[[1]][2] <- as.numeric(nam[[1]][2]) - 1

      newString <- ""

      if (nchar(name[[i]]) == 2) {
        if (nam[[1]][2] > 0) {
          newString <- paste0(nam[[1]][1], nam[[1]][2])
        }
      } else {
        if (nam[[1]][2] > 0) {
          newString <-
            paste0(nam[[1]][1], nam[[1]][2], nam[[1]][3], nam[[1]][4])
        } else {
          newString <- paste0(nam[[1]][3], nam[[1]][4])
        }
      }
      name[[i]] <- newString

      i <- i + 1
    }

    names(coeff) <- name
  }
  return(coeff)
}

#' Prints the results and regression function of a cnorm model
#'
#' @param object A regression model or cnorm object
#' @param ... additional parameters
#' @return A report on the regression function, weights, R2 and RMSE
#' @export
#' @family model
modelSummary <- function(object, ...) {
  if (inherits(object, "cnorm")) {
    object <- object$model
  }

  cat(object$report, sep = "\n")
}

#' Check for horizontal and vertical extrapolation
#'
#' Regression model only work in a specific range and extrapolation horizontally (outside
#' the original range) or vertically (extreme norm scores) might lead to inconsistent
#' results. The function generates a message, indicating extrapolation and the range of the original data.
#' @param object The regression model or a cnorm object
#' @param minAge The lower age bound
#' @param maxAge The upper age bound
#' @param minNorm The lower norm value bound
#' @param maxNorm The upper norm value bound
#' @param digits The precision for rounding the norm and age data
#' @param ... additional parameters
#' @return the report
#' @export
#' @examples
#' normData <- prepareData(elfe)
#' m <- bestModel(normData)
#' rangeCheck(m)
#' @family model
rangeCheck <-
  function(object,
           minAge = NULL,
           maxAge = NULL,
           minNorm = NULL,
           maxNorm = NULL,
           digits = 3,
           ...) {
    if (inherits(object, "cnorm")) {
      object <- object$model
    }

    summary <-
      paste0(
        "The original data for the regression model spanned from age ",
        round(object$minA1, digits),
        " to ",
        round(object$maxA1, digits),
        ", with a norm score range from ",
        round(object$minL1, digits),
        " to ",
        round(object$maxL1, digits),
        ". The raw scores range from ",
        object$minRaw,
        " to ",
        object$maxRaw,
        "."
      )
    if (object$descend) {
      summary <-
        paste0(summary, " The ranking was done in descending order.")
    }
    reportOnly <-
      (is.null(minAge) ||
         is.null(maxAge) || is.null(minNorm) || is.null(maxNorm))
    if (!reportOnly &&
        (minAge < object$minA1 ||
         maxAge > object$maxA1) &&
        (minNorm < object$minL1 || maxNorm > object$maxL1)) {
      summary <-
        paste(
          "Horizontal and vertical extrapolation detected. Be careful using age groups and extreme norm scores outside the original sample.",
          summary,
          sep = "\n"
        )
    } else if (!reportOnly &&
               (minAge < object$minA1 || maxAge > object$maxA1)) {
      summary <-
        paste(
          "Horizontal extrapolation detected. Be careful using age groups outside the original sample.",
          summary,
          sep = "\n"
        )
    } else if (!reportOnly &&
               (minNorm < object$minL1 || maxNorm > object$maxL1)) {
      summary <-
        paste(
          "Vertical extrapolation detected. Be careful using extreme norm scores exceeding the scores of the original sample.",
          summary,
          sep = "\n"
        )
    }

    return(summary)
  }

#' Cross validation for term selection
#'
#' This function helps in selecting the number of terms for the model by doing repeated
#' Monte Carlo cross validation with 80 percent of the data as training data and 20 percent as
#' the validation data. The cases are drawn randomly but stratified by norm group. Successive
#' models are retrieved with increasing number of terms and the RMSE of raw scores (fitted by
#' the regression model) is plotted for the training, validation and the complete dataset.
#' Additionally to this analysis on the raw score level, it is possible (default) to estimate
#' the mean norm score reliability and crossfit measures. For this, please set the norms parameter
#' to TRUE. Due to the high computational load when computing norm scores, it takes time to finish
#' when doing repeated cv or comparing models up to the maximum number of terms. When using
#' the cv = "full" option, the ranking is done for the test and validation dataset
#' separately (always based on T scores), resulting in a complete cross validation. In
#' order to only validate the modeling, you as well can use a pre-ranked data set with
#' prepareData(elfe) already applied. In this case, the training and validation data is
#' drawn from the already ranked data and the scores for the validation set should improve.
#' It is however no independent test, as the ranking between both samples is interlinked.
#' In the output, you will get RMSE for the raw score models, norm score R2 and delta R2, the crossfit
#' and the norm score SE sensu Oosterhuis, van der Ark, & Sijtsma (2016).
#' For assessing, if a model over-fits the data and to what extent, we need cross-validation. We assumed
#' that an overfitting occurred when a model captures more variance of the observed norm scores of the
#' training sample compared to the captured variance of the norm scores of the validation sample. The
#' overfit can therefore be described as:
#' \deqn{CROSSFIT = R(Training; Model)^2 / R(Validation; Model)^2}
#' A CROSSFIT higher than 1 is a sign of overfitting. Value lower than 1 indicate an underfit due to a
#' suboptimal modeling procedure, i. e. the method may not have captured all the variance of the observed
#' data it could possibly capture. Values around 1 are ideal, as long as the raw score RMSE is low and the
#' norm score validation R2 reaches high levels. As a suggestion for real tests:
#' \itemize{
#'   \item Use visual inspection of the percentiles with plotPercentiles or plotPercentileSeries
#'   \item Combine the visual inspection of the percentiles with a repeated cross validation (e. g. 10 repetitions)
#'   \item Focus on low raw score RMSE, high norm score R2 in the validation dataset and
#' avoid a number of terms with a high overfit (e. g. crossfit > 1.1).
#' }
#'
#' @param data data frame of norm sample with ranking, powers and interaction of L and A or a cnorm object
#' @param formula prespecified formula, e. g. from an existing regression model; min and max functions will be ignored
#' In case a cnorm object is used, this functions automatically draws on the formula of the inbuilt regression
#' function
#' @param repetitions number of repetitions for cross validation
#' @param norms determine norm score crossfit and R2 (if set to TRUE). The option is
#' computationally intensive and duration increases with sample size, number of
#' repetitions and maximum number of terms (max option).
#' @param min Minimum number of terms to start from, default = 1
#' @param max Maximum number of terms in model up to (k + 1) * (t + 1) + 1
#' @param cv If set to full (default), the data is split into training and validation data and ranked afterwards,
#' otherwise, a pre ranked dataset has to be provided, which is then split into train and validation (and thus
#' only the modeling, but not the ranking is independent)
#' @param pCutoff The function checks the stratification for unbalanced data sampling.
#' It performs a t-test per group. pCutoff specifies the p-value per group that the test result
#' has to reach at least. To minimize beta error, the value is set to .2 per default
#' @param width If provided, ranking is done via rankBySlidingWindow, otherwise by group
#' @param raw Name of the raw variable
#' @param age Name of the age variable
#' @param group Name of the grouping variable
#' @param weights Name of the weighting parameter
#' @return table with results per term number, including RMSE for raw scores in training, validation and complete
#' sample, R2 for the norm scores and the crossfit measure (1 = ideal, <1 = underfit, >1 = overfit)
#' @export
#' @examples
#' # plot cross validation RMSE by number of terms up to 9 with three repetitions
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' cnorm.cv(result$data, min = 2, max = 9, repetitions = 3)
#'
#' # Applying the function to a cnorm object only investigates the already
#' # determined formula
#' cnorm.cv(result, repetitions = 1)
#'
#' # To use the cross validation without a cnorm model, please rank data first and
#' # compute powers:
#' data <- rankByGroup(data = elfe, raw = "raw", group = "group")
#' data <- computePowers(data)
#' cnorm.cv(data)
#'
#' # Formulae can be specified deliberately as well:
#' data <- rankByGroup(data = elfe, raw = "raw", group = "group")
#' data <- computePowers(data)
#' cnorm.cv(data, formula = formula(raw ~ L3 + L1A1 + L3A3 + L4 + L5))
#'
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016). Sample Size Requirements for Traditional and Regression-Based Norms. Assessment, 23(2), 191–202. https://doi.org/10.1177/1073191115580638
#' @family model
cnorm.cv <-
  function(data,
           formula = NULL,
           repetitions = 5,
           norms = TRUE,
           min = 1,
           max = 12,
           cv = "full",
           pCutoff = NULL,
           width = NA,
           raw = NULL,
           group = NULL,
           age = NULL,
           weights = NULL) {
    if (inherits(data, "cnorm")) {
      formula <- data$model$terms
      data <- data$data
    }

    if (is.null(pCutoff)) {
      if (nrow(data) < 10000)
        pCutoff = .2
      else
        pCutoff = .1
    }

    #if (!attr(data, "useAge")) {
    #  stop("Age variable set to FALSE in dataset. No cross validation possible.")
    #}

    ## TODO
    if (!is.null(attr(data, "covariate"))) {
      stop("This function is currently not ready for including covariates.")
    }

    d <- data

    if (is.null(raw)) {
      raw <- attr(d, "raw")
    }

    if (is.null(raw)) {
      stop(
        "Please provide a raw score variable name. It is neither available as a parameter nor a an attribute from data object."
      )
    }

    if(!is.null(raw) & is.null(data[, raw])){
      stop(paste0(
          "The specified raw score variable ", raw, " is not present in the dataset."
        )
      )
    }

    if (is.null(group)) {
      group <- attr(d, "group")
    }

    if (is.null(age)) {
      age <- attr(d, "age")
    }

    if (is.na(width) & !is.null(attr(d, "width"))) {
      width <- attr(d, "width")
    }

    if (is.null(group) || (is.null(age) & is.na(width))) {
      stop(
        "Please provide either a grouping variable or age and width. They are neither available as parameters nor as attributes from data object."
      )
    }

    if (is.null(weights)) {
      weights <- attr(d, "weights")
    }

    if(!is.null(weights) & is.null(data[, weights])){
      warning(
        "Name of the weighting variable provided, but not found in the dataset. Continuing without weighting ...\n"
      )

      weights <- NULL
    }else if(!is.null(weights) & !is.null(data[, weights])){
      cat(
        "Applying weighting ...\n"
      )
    }

    scaleM <- attr(d, "scaleMean")
    if (is.na(scaleM) || cv == "full") {
      scaleM <- 50
    }
    scaleSD <- attr(d, "scaleSD")
    if (is.na(scaleSD) || cv == "full") {
      scaleSD <- 10
    }


    k <- attr(d, "k")
    if (is.null(k)) {
      k <- 5
    }

    t <- attr(d, "t")
    if (is.null(t)) {
      t <- 3
    }

    n.models <- (t * k) ^ 2 - 1
    if (is.na(max) || max > n.models || max < 1) {
      max <- n.models
    }

    lmX <- NA
    # set up regression formulas (from bestModel function)
    if (is.null(formula)) {
      lmX <-
        buildFunction(
          raw = raw,
          k = k,
          t = t,
          age = TRUE,
          covariates = FALSE
        )
    } else {
      lmX <- formula
      min <- length(formula)
      max <- length(formula)
    }


    # set up vectors to store RMSE for training, test and complete dataset models
    val.errors <- rep(0, max)
    train.errors <- rep(0, max)
    complete.errors <- rep(0, max)

    # set up vectors to store norm score R2 and CROSSFIT
    r2.train <- rep(0, max)
    r2.test <- rep(0, max)
    delta <- rep(NA, max)
    crossfit <- rep(0, max)
    norm.rmse <- rep(0, max)
    norm.se <- rep(0, max)
    norm.rmse.min <- rep(0, max)
    Terms <- c()

    rankGroup <- TRUE
    if (!is.null(age) && !is.na(width)) {
      cat("Age and width parameters available, thus switching to rankBySlidingWindow() ...\n")
      rankGroup <- FALSE
    }

    # draw test and training data several times ('repetitions' parameter), model data and store MSE
    for (a in 1:repetitions) {
      # check for imbalances in data and repeat if stratification was unsatisfactory - usually never occurs
      p.value <- .01
      n <- 1 # to avoid a deadlock, define stop criterion

      train <- NA
      test <- NA

      while (p.value < pCutoff) {
        if (n > 100) {
          stop("Could not establish balanced data sets. Try to decrease pCutoff parameter.")
        }
        n <- n + 1

        #rankByGroup
        if (rankGroup) {
          # shuffle data and split into groups (for stratification)
          d <- d[sample(nrow(d)), ]
          d <- d[order(d[, group]), ]
          sp <- split(d, list(d[, group]))
          sp <- lapply(sp, function(x)
            x[sample(nrow(x)), ])

          # draw 8 tenth of data from each group for training and testing
          train <- lapply(sp, function(x)
            x[c(FALSE, rep(TRUE, 4)), ])
          test <- lapply(sp, function(x)
            x[c(TRUE, rep(FALSE, 4)), ])

          # test for significant differences to avoid extremely unbalanced data
          p <- rep(1, length(train))
          for (z in 1:length(train)) {
            p[z] <- t.test(train[[z]][, raw], test[[z]][, raw])$p.value
          }
          p.value <- min(p)
          if (p.value < pCutoff) {
            next
          }

          # combine lists to data frames
          train <- do.call(rbind, train)
          test <- do.call(rbind, test)

          if (cv == "full") {
            train <-
              prepareData(
                train,
                raw = raw,
                group = group,
                age = age,
                width = width,
                weights = weights,
                silent = TRUE
              )
            test <-
              prepareData(
                test,
                raw = raw,
                group = group,
                age = age,
                width = width,
                weights = weights,
                silent = TRUE
              )
          }
        } else{
          #rankBySlidingWindow
          d <- d[sample(nrow(d)), ]
          number <- nrow(d) / 10 * 8
          train <- d[1:number,]
          test <- d[(number + 1):nrow(d),]


          p.value <- t.test(train[, age], test[, age])$p.value
          if (p.value < pCutoff) {
            next
          }


          train <-
            rankBySlidingWindow(
              train,
              age = age,
              raw = raw,
              weights = weights,
              width = width,
              silent = TRUE
            )
          test <-
            rankBySlidingWindow(
              test,
              age = age,
              raw = raw,
              weights = weights,
              width = width,
              silent = TRUE
            )

          train <-
            computePowers(
              train,
              age = age,
              k = k,
              t = t,
              silent = TRUE
            )
        }
      }

      # compute leaps model
      subsets <- regsubsets(lmX, data = train, nbest = 1, nvmax = max, really.big = n.models > 25)


      if (norms && is.null(formula)) {
        cat(paste0("Cycle ", a, "\n"))
      }

      # retrieve models coefficients for each number of terms
      for (i in min:max) {
        variables <- names(coef(subsets, id = i))
        variables <-
          variables[2:length(variables)] # remove '(Intercept)' variable
        reg <-
          paste0(raw, " ~ ", paste(variables, collapse = " + ")) # build regression formula

        # run linear regression for specific model
        model <- lm(reg, train)
        model$k <- k
        model$minRaw <- min(train[, raw])
        model$maxRaw <- max(train[, raw])
        model$scaleM <- scaleM
        model$scaleSD <- scaleSD
        Terms <- c(Terms, attr(model$terms, "term.labels"))

        # predict values in test data
        test.fitted <- predict.lm(model, test)

        # store MSE for test and train data
        train.errors[i] <-
          train.errors[i] + mean((model$fitted.values - train[, raw]) ^ 2, na.rm = T)
        val.errors[i] <-
          val.errors[i] + mean((test.fitted - test[, raw]) ^ 2, na.rm = T)

        # compute R2 for test and training
        if (norms) {
          train$T <-
            predictNorm(train[, raw],
                        train[, age],
                        model,
                        min(train$normValue),
                        max(train$normValue),
                        silent = TRUE)
          test$T <-
            predictNorm(test[, raw],
                        test[, age],
                        model,
                        min(train$normValue),
                        max(train$normValue),
                        silent = TRUE)

          r2.train[i] <-
            r2.train[i] + (cor(train$normValue, train$T, use = "pairwise.complete.obs") ^
                             2)
          r2.test[i] <-
            r2.test[i] + (cor(test$normValue, test$T, use = "pairwise.complete.obs") ^
                            2)
          norm.rmse[i] <-
            norm.rmse[i] + sqrt(mean((test$T - test$normValue) ^ 2, na.rm = TRUE))
          norm.se[i] <-
            norm.se[i] + sum(sqrt((test$T - test$normValue) ^ 2), na.rm = TRUE) / (length(!is.na(test$T)) -
                                                                                     2)
        }
      }
    }

    # now for the complete data the same logic
    norm.rmse.min[1] <- NA
    complete <- regsubsets(lmX, data = d, nbest = 1, nvmax = n.models, really.big = n.models > 25)

    for (i in 1:max) {
      variables <- names(coef(complete, id = i))
      variables <- variables[2:length(variables)]
      reg <- paste0(raw, " ~ ", paste(variables, collapse = " + "))
      model <- lm(reg, d)

      # mse for the complete data based on number of terms
      complete.errors[i] <-
        sqrt(mean((model$fitted.values - d[, raw]) ^ 2, na.rm = T))

      # build the average over repetitions and the root
      train.errors[i] <- sqrt(train.errors[i] / repetitions)
      val.errors[i] <- sqrt(val.errors[i] / repetitions)

      if (norms) {
        r2.train[i] <- r2.train[i] / repetitions
        r2.test[i] <- r2.test[i] / repetitions
        norm.rmse[i] <- norm.rmse[i] / repetitions
        norm.se[i] <- norm.se[i] / repetitions

        if (i > min) {
          delta[i] <- r2.test[i] - r2.test[i - 1]
          if (norm.rmse[i] > 0) {
            norm.rmse.min[i] <- norm.rmse[i] - norm.rmse[i - 1]
          } else{
            norm.rmse.min[i] <- NA
          }
        }
      }

      if (i < min) {
        r2.train[i] <- NA
        r2.test[i] <- NA
        val.errors[i] <- NA
        train.errors[i] <- NA
        complete.errors[i] <- NA
        norm.rmse[i] <- NA
      }

      if (i <= min) {
        norm.rmse.min[i] <- NA
      }
    }

    if (norms) {
      par(mfrow = c(2, 2)) # set the plotting area into a 1*2 array
    } else {
      par(mfrow = c(1, 1))
    }
    tab <-
      data.frame(
        RMSE.raw.train = train.errors,
        RMSE.raw.test = val.errors,
        RMSE.raw.complete = complete.errors,
        R2.norm.train = r2.train,
        R2.norm.test = r2.test,
        Delta.R2.test = delta,
        Crossfit = r2.train / r2.test,
        RMSE.norm.test = norm.rmse,
        SE.norm.test = norm.se
      )

    if (is.null(formula)) {
      # plot RMSE
      graphics::plot(
        val.errors,
        pch = 19,
        type = "b",
        col = "blue",
        main = "Raw Score RMSE",
        ylab = "Root MSE",
        xlab = "Number of terms",
        ylim = c(
          min(train.errors, na.rm = TRUE),
          max(val.errors, na.rm = TRUE)
        )
      )
      points(
        complete.errors,
        pch = 19,
        type = "b",
        col = "black"
      )
      points(train.errors,
             pch = 19,
             type = "b",
             col = "red")
      legend(
        "topright",
        legend = c("Training", "Validation", "Complete"),
        col = c("red", "blue", "black"),
        pch = 19
      )

      if (norms) {
        # plot R2
        graphics::plot(
          r2.train,
          pch = 19,
          type = "b",
          col = "red",
          main = "Norm Score R2",
          ylab = "R Square",
          xlab = "Number of terms",
          ylim = c(min(r2.test, na.rm = TRUE), 1)
        )
        points(r2.test,
               pch = 19,
               type = "b",
               col = "blue")
        legend(
          "bottomright",
          legend = c("Training", "Validation"),
          col = c("red", "blue"),
          pch = 19
        )

        # plot CROSSFIT
        graphics::plot(
          tab$Crossfit,
          pch = 19,
          type = "b",
          col = "black",
          main = "Norm Score CROSSFIT",
          ylab = "Crossfit",
          xlab = "Number of terms",
          ylim = c(min(c(
            tab$Crossfit, .88
          ), na.rm = TRUE), max(c(
            tab$Crossfit, 1.12
          ), na.rm = TRUE))
        )
        abline(h = 1, col = 3, lty = 2)
        abline(h = .9, col = 2, lty = 3)
        text(
          max,
          .89,
          adj = c(1, 1),
          "underfit",
          col = 2,
          cex = .75
        )
        abline(h = 1.1, col = 2, lty = 3)
        text(
          max,
          1.11,
          adj = c(1, 0),
          "overfit",
          col = 2,
          cex = .75
        )

        # plot delta r2 test
        graphics::plot(
          tab$Delta.R2.test,
          pch = 19,
          type = "b",
          col = "black",
          main = "Norm Score Delta R2 in Validation",
          ylab = "Delta R2",
          xlab = "Number of terms",
          ylim = c(
            min(tab$Delta.R2.test, na.rm = TRUE),
            max(tab$Delta.R2.test, na.rm = TRUE)
          )
        )
        abline(h = 0, col = 3, lty = 2)
      } else{
        tab$R2.norm.train <- NULL
        tab$R2.norm.test <- NULL
        tab$Delta.R2.test <- NULL
        tab$Crossfit <- NULL
        tab$RMSE.norm.test <- NULL
      }

      cat("\n")
      cat("Occurance of selected terms, sorted by frequency:\n")
      print(sort(table(Terms), decreasing = T))

      cat("\n")
      cat("The simulation yielded the following optimal settings:\n")
      if (norms) {
        cat(paste0("\nNumber of terms with best crossfit: ", which.min((
          1 - tab$Crossfit
        ) ^ 2)))
      }
      cat(paste0(
        "\nNumber of terms with best raw validation RMSE: ",
        which.min(tab$RMSE.raw.test)
      ))
      if (norms) {
        best.norm <- which.max(r2.test)
        FirstNegative <- which(tab$Delta.R2.test <= 0)[1]

        cat(paste0(
          "\nNumber of terms with best norm validation R2: ",
          best.norm,
          "\n"
        ))

        cat(
          paste0(
            "Choosing a model with ",
            FirstNegative,
            " terms might be a good choice. For this, use the parameter 'terms = ",
            FirstNegative,
            "' in the bestModel-function.\n"
          )
        )
        cat(
          "\nPlease investigate the plots and the summary table, as the results might vary within a narrow range."
        )
        cat(
          "\nEspacially pay attention to RMSE.raw.test, r2.test, crossfit near 1 and where delta R2 stops to progress."
        )
      }

      cat("\n")
      cat("\n")
      return(tab[min:max, ])
    } else{
      cat("\n")
      cat("\n")

      cat(
        paste0(
          "Repeated cross validation with prespecified formula and ",
          repetitions,
          " repetitions yielded the following results:\n"
        )
      )
      cat("\n")
      tab$Delta.R2.test <- NULL
      return(tab[complete.cases(tab), ])
    }



  }



#' Calculates the standard error (SE) or root mean square error (RMSE) of the norm scores
#' In case of large datasets, both results should be almost identical
#'
#' @param model a cnorm object
#' @param type either '1' for the standard error senso Oosterhuis et al. (2016) or '2' for
#'             the RMSE (default)
#'
#' @return The standard error (SE) of the norm scores sensu Oosterhuis et al. (2016) or the RMSE
#' @export
#'
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016). Sample Size Requirements for Traditional and Regression-Based Norms. Assessment, 23(2), 191–202. https://doi.org/10.1177/1073191115580638
getNormScoreSE <- function(model, type = 2) {
  if (!inherits(model, "cnorm")) {
    stop("Please provide cnorm object as the model parameter")
  }

  if (type != 1 || type != 2) {
    type <- 2
  }

  data <- model$data
  model <- model$model
  minNorm <- model$minL1
  maxNorm <- model$maxL1
  d <- data
  raw <- data[[model$raw]]
  age <- data[[model$age]]
  if (!is.null(model$covariate))
    covariate <- data[[attr(data, "covariate")]]
  else
    covariate <- NULL

  d$fitted <-
    predictNorm(
      raw,
      age,
      model,
      minNorm = minNorm,
      maxNorm = maxNorm,
      covariate = covariate
    )

  diff <- d$fitted - data$normValue
  diff <- diff[!is.na(diff)]

  #return(sqrt(mean(diff^2)))
  if (type == 1)
    return(sqrt(sum(diff ^ 2) / (length(diff) - 2)))
  else
    return(sqrt(mean(diff ^ 2)))
}



#' Build regression function for bestModel
#'
#' @param raw name of the raw score variable
#' @param k the power degree for location
#' @param t the power degree for age
#' @param age use age
#' @param covariates use covariates
#'
#' @return reression function
buildFunction <- function(raw, k, t, age, covariates) {
  f <- paste0(raw, " ~ ")

  if (age) {
    f <- paste0(f, paste0(paste0("L", 1:k), collapse = " + "), " + ")
    f <-
      paste0(f, paste0(paste0("A", 1:t), collapse = " + "), " + ")

    for (i in 1:k) {
      for (j in 1:t) {
        f <- paste0(f, paste0("L", i), paste0("A", j), " + ")
      }
    }

    if (covariates) {
      return(formula(paste0(f, "COV + L1COV + A1COV + L1A1COV")))
    } else {
      return(formula(substr(f, 1, nchar(f) - 3)))
    }
  } else {
    f <- paste0(f, paste0(paste0("L", 1:k), collapse = " + "), " + ")

    if (covariates) {
      return(formula(paste0(f, "COV + L1COV")))
    } else {
      return(formula(substr(f, 1, nchar(f) - 3)))
    }
  }
}
