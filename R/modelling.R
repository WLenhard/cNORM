

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
#' might lead to on over-fit, so be careful! These paramters depend on the distribution of
#' the norm data. As a rule of thumb, terms = 5 or R2 = .99 and k = 4 is a good starting point
#' for the analyses.
#' \code{plotSubset(model)} can be used to weigh up R2 and information criteria (Cp, an AIC like measure)
#' and predicted versus actual values can be plotted with 'plotValues' and 'plotPercentiles'.
#'
#' @param data The preprocessed dataset, which should include the variables 'raw'
#'  and the powers and interactions of the norm value (L = Location; usually T values)
#'  and an explanatory variably (usually age = A)
#' @param raw the name of the raw value variable (default raw)
#' @param terms Selection criterion for model building. The best fitting model with
#' this number of terms is used
#' @param R2 Adjusted R square as a stopping criterion for the model building
#' (default R2 = 0.99)
#' @param k The power constant. Higher values result in more detailed approximations
#' but have the danger of over-fit (default = 4, max = 6)
#' @param predictors List of the names of predictor to use for the model selection.
#' The parameter overrides the 'k' parameter and it can be used to preselect the
#' variables entering the regression, or even to add variables like sex, that are
#' not part of the original model building. Please not, that adding other variables
#' than those based on L and A, plotting, prediction and normTable function will most
#' likely not work, but at least the regression formula can be obtained that way.
#' @return The model meeting the R2 criteria with coefficients and variable selection
#' in model$coefficients. Use \code{plotSubset(model)} and
#' \code{plotPercentiles(data, model)} to inspect model
#' @examples
#' # Standard example with sample data
#' normData <- prepareData()
#' model <- bestModel(normData)
#' plotSubset(model)
#' plotPercentiles(normData, model)
#'
#' # It is possible to specify the variables explicitely - usefull to smuggle in variables like sex
#' preselectedModel <- bestModel(normData, predictors = c("L1", "L3", "L1A3", "A2", "A3"))
#' print(regressionFunction(preselectedModel))
#' @seealso plotSubset, plotPercentiles, checkConsistency
#' @export
bestModel <- function(data,
                      raw = "raw",
                      R2 = 0.99,
                      k = 4,
                      predictors = NULL,
                      terms = 0) {
  if (R2 <= 0 || R2 >= 1) {
    stop("R2 parameter out of bounds.")
  }

  if (terms < 0) {
    stop("terms parameter out of bounds.")
  }

  if ((k < 1 || k > 6) & is.null(predictors)) {
    stop("k parameter out of bounds.")
  }

  if(!(raw %in% colnames(data))){
    stop(paste(c("ERROR: Raw value variable '", raw, "' does not exist in data object."), collapse = ""));
  }

  if ((!is.null(predictors))&&(!(predictors %in% colnames(data)))) {
    stop("ERROR: Missing variables from predictors variable. Please check variable list.");
  }

  if ((k > 4 & is.null(predictors)) || (!is.null(predictors) && length(predictors) > 25)) {
    message("The computation might take some time ...")
  }
  if (!is.null(predictors)) {
    lmX <- stats::formula(paste(raw, paste(predictors, collapse = " + "), sep = " ~ "))
  } else if (k == 1) {
    lmX <- stats::formula(paste(raw, "L1 + A1 + L1A1", sep = " ~ "))
  } else if (k == 2) {
    lmX <-
      stats::formula(paste(raw, "L1 + L2 + A1 + A2 + L1A1 + L1A2 + L2A1 + L2A2", sep = " ~ "))
  } else if (k == 3) {
    lmX <- stats::formula(paste(raw, "L1 + L2 + L3 + A1 + A2 + A3 + L1A1 + L1A2 + L1A3 + L2A1 + L2A2 + L2A3 + L3A1 + L3A2 + L3A3", sep = " ~ "))
  } else if (k == 4) {
    lmX <- stats::formula(paste(raw, "L1 + L2 + L3 + L4 + A1 + A2 + A3 + A4 + L1A1 + L1A2 + L1A3 + L1A4 + L2A1 + L2A2 + L2A3 + L2A4 + L3A1 + L3A2 + L3A3 + L3A4 + L4A1 + L4A2 + L4A3 + L4A4", sep = " ~ "))
  } else if (k == 5) {
    lmX <- stats::formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + A1 + A2 + A3 + A4 + A5 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5", sep = " ~ "))
  } else if (k == 6) {
    lmX <-
      stats::formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + L6 + A1 + A2 + A3 + A4 + A5 + A6 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L1A6 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L2A6 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L3A6 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L4A6 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5 + L5A6 + L6A1 + L6A2 + L6A3 + L6A4 + L6A5 + L6A6", sep = " ~ "))
  } else {
    message("Power parameter unknown, setting to k = 4")
    lmX <- stats::formula(paste(raw, "L1 + L2 + L3 + L4 + A1 + A2 + A3 + A4 + L1A1 + L1A2 + L1A3 + L1A4 + L2A1 + L2A2 + L2A3 + L2A4 + L3A1 + L3A2 + L3A3 + L3A4 + L4A1 + L4A2 + L4A3 + L4A4", sep = " ~ "))
  }

  subsets <- leaps::regsubsets(lmX, data = data, nbest = 1, nvmax = 2 * k + k * k, force.in = NULL)
  results <- summary(subsets)

  i <- 1
  rAdj <- results$adjr2[i]


  if (terms > 0 && terms <= length(results$adjr2)) {
    i <- terms
    finished <- TRUE
    message(paste0(
      "\nUser specified solution: ",
      i,
      "\nR-Square Adj. amounts to ",
      results$adjr2[i]
    ))
  } else {
    # check upper and lower bounds and cycle through R2 list
    if (terms > 0) {
      message("\n\nCould not determine best model based of number of terms, using R2 instead.")
    }
    if (R2 < results$adjr2[i]) {
      message(paste0(
        "\nSpecified R2 falls below the value of the most primitive model. Falling back to model 1.\nR-Square Adj. amounts to ",
        results$adjr2[i]
      ))
    } else if (results$adjr2[length(results$adjr2)] < R2) {
      i <- length(results$adjr2)
      message(paste0(
        "\nSpecified R2 exceeds the R2 of the model with the highest fit. Consider rerunning the analysis with higher k value. Falling back to model ", i, ".\nR-Square Adj. amounts to ",
        results$adjr2[i]
      ))
    } else {
      while (rAdj < R2) {
        i <- i + 1
        rAdj <- results$adjr2[i]
      }
      message(paste0(
        "\nFinal solution: ",
        i,
        "\nR-Square Adj. amounts to ",
        round(results$adjr2[i], digits = 5)
      ))
    }
  }

  text <- paste0(raw, " ~ ")
  names <- colnames(results$outmat)

  j <- 1
  nr <- 0
  while (j <= length(names)) {
    if (results$outmat[i, j] == "*") {
      text1 <- names[j]
      if (nr == 0) {
        text <- paste(text, text1, sep = "")
      } else {
        text <- paste(text, text1, sep = " + ")
      }

      nr <- nr + 1
    }
    j <- j + 1
  }

  message(paste0("Final regression model: ", text))
  message("Beta weights are accessible via 'model$coefficients':")
  bestformula <- stats::lm(text, as.data.frame(data))
  bestformula$ideal.model <- i
  bestformula$cutoff <- R2
  bestformula$subsets <- results
  print(bestformula$coefficients)
  # add information for horizontal and vertical extrapolation
  bestformula$minA1 <- min(data$A1)
  bestformula$maxA1 <- max(data$A1)
  bestformula$minL1 <- min(data$L1)
  bestformula$maxL1 <- max(data$L1)


  message("\nRegression formula:")
  print(regressionFunction(bestformula, digits=8))
  message("\nUse 'printSubset(model)' and 'plotSubset(model)' to inspect model fit.")

  return(bestformula)
}


#' Convenience method for printing model selection information
#'
#' After conducting the model fitting procedure on the data set, the best fitting
#' model has to be chosen. The print function shows the R2 and other information
#' on the different best fitting models with increasing number of predictors.
#' @param model The model from the 'bestModel' function
#'
#' @return A table with information criteria
#' @export
#'
#' @examples
#' model <- bestModel(prepareData())
#' printSubset(model)
#'
printSubset <- function(model){
  table <-
    do.call(rbind, Map(data.frame, R2 = model$subsets$rsq,
                       RSS = model$subsets$rss,
                       R2adj = model$subsets$adjr2,
                       Cp = model$subsets$cp,
                       BIC = model$subsets$bic))
  return(table)
}

#' Check the consistency of the norm data model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm values always have to show a linear increase with increasing raw
#' values. Violations of this assumption are a strong indication for problems
#' in modeling the relationship between raw and norm values. There are
#' several reasons, why this might occur:
#' \enumerate{
#'   \item Vertical extrapolation: Choosing extreme norm values, e. g. values
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
#' @param model The model from the bestModel function
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param stepAge Stepping parameter for the age check, usually 1 or 0.1; lower
#' values indicate higher precision / closer checks
#' @param minNorm Lower end of the norm value range
#' @param maxNorm Upper end of the norm value range
#' @param minRaw clipping parameter for the lower bound of raw values
#' @param maxRaw clipping parameter for the upper bound of raw values
#' @param stepNorm Stepping parameter for the norm table check within age with lower
#' values indicating a higher precision. The choice depends of the norm scale
#' used. With T values a stepping parameter of 1 is suitable
#' @param descend Reverse raw value order. If set to TRUE, lower raw values
#' indicate higher performance. Relevant f. e. in case of modelling errors
#' @param warn If set to TRUE, already minor violations of the model assumptions
#' are displayed (default = FALSE)
#' @examples
#' normData <- prepareData()
#' m <- bestModel(normData)
#' modelViolations <- checkConsistency(m, minAge=2, maxAge=5, stepAge=0.1,
#'                    minNorm=25, maxNorm=75, minRaw=0, maxRaw= 28, stepNorm=1)
#' plotDerivative(m, , minAge=2, maxAge=5, minNorm=25, maxNorm=75)
#' @export
checkConsistency <- function(model,
                             minAge,
                             maxAge,
                             minNorm,
                             maxNorm,
                             minRaw = -Inf,
                             maxRaw = Inf,
                             stepAge = 1,
                             stepNorm = 1,
                             descend = FALSE,
                             warn = FALSE) {

  i <- minAge
  j <- minNorm
  minor <- 0
  major <- 0
  results <- c()
  while (i <= maxAge) {
    norm <- cNORM::normTable(i, model,
      minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw,
      step = stepNorm, descend = descend
    )
    k <- 1
    maxR <- 0
    while (k < length(norm$raw)) {
      if (norm$raw[[k]] > maxR) {
        maxR <- norm$raw[[k]]
      }
      diff <- maxR - norm$raw[[k + 1]]
      if ((!descend && diff >= 1) || (descend && diff <= -1)) {
        message(paste0(
          "Considerable violation of consistency at age ",
          round(i, digits = 1), ", raw value ",
          round(norm$raw[[k]],
            digits = 1
          )
        ))
        results <- c(results, paste0(
          "Considerable violation of consistency at
                                       age ",
          round(i, digits = 1), ",
                                             raw value ",
          round(norm$raw[[k]],
            digits = 1
          )
        ))
        major <- major + 1
        k <- length(norm$raw) + 1
      } else if (warn & ((!descend && diff > 0) || (descend && diff < 0))) {
        message(paste0(
          "Neglectible violation of consistency at age ",
          round(i, digits = 1),
          ", raw value ",
          round(norm$raw[[k]],
            digits = 1
          )
        ))
        results <- c(results, paste0(
          results, "Neglectible violation of
                                       consistency at age ",
          round(i, digits = 1), ",
                                             raw value ",
          round(norm$raw[[k]],
            digits = 1
          )
        ))
        minor <- minor + 1
      }
      k <- k + 1
    }

    i <- i + stepAge
  }
  if (minor == 0 & major == 0) {
    message("\nNo violations of model consistency found.")
  } else if (major == 0) {
    message(paste0("\n", minor, " minor violations of model consistency found."))
    message(cNORM::rangeCheck(model, minAge, maxAge, minNorm, maxNorm))
  } else {
    message(paste0("\nAt least ", major, " major and ", minor, " minor violations of model consistency found."))
    message("Use 'plotNormCurves' to visually inspect the norm curve and restrict the valid value range accordingly.")
    message("Be careful with horizontal and vertical extrapolation.")
    message(cNORM::rangeCheck(model, minAge, maxAge, minNorm, maxNorm))

  }
}

#' Regression function
#'
#' The method builds the regression function for the regression model,
#' including the beta weights.
#' It can be used to predict the raw values based on age and location.
#' @param model The regression model from the bestModel function
#' @param raw The name of the raw value variable (default 'raw')
#' @param digits Number of digits for formatting the coefficients
#' @return The regression formula as a string
#'
#' @examples
#' normData <- prepareData()
#' model <- bestModel(normData)
#' regressionFunction(model)
#' @export
regressionFunction <- function(model, raw = "raw", digits=NULL) {


  i <- 2
  if(is.null(digits)){
    formulA <- paste(raw, model$coefficients[[1]], sep = " ~ ")
    while (i <= length(model$coefficients)) {
    formulA <- paste0(
      formulA, " + (", model$coefficients[[i]], "*",
      names(model$coefficients[i]), ")"
    )
    i <- i + 1
  }
}else{
  formulA <- paste(raw, format(model$coefficients[[1]], digits = digits), sep = " ~ ")
  while (i <= length(model$coefficients)) {
    formulA <- paste0(
      formulA, " + (", format(model$coefficients[[i]], digits = digits), "*",
      names(model$coefficients[i]), ")"
    )
    i <- i + 1
  }
  }
  return(formulA)
}

#' First derivation of regression model
#'
#' Calculates the first derivation of the location / norm value from the regression model. This
#' is useful for finding violations of model assumptions and problematic distribution features as
#' f. e. bottom and ceiling effects, non-progressive norm values within an age group or in general
#' intersecting percentile curves.
#' @param model The regression model
#' @return The derived coefficients
#' @examples
#' normData <- prepareData()
#' m <- bestModel(normData)
#' derivedCoefficients <- derive(m)
#' @export
derive <- function(model) {
  coeff <- model$coefficients[grep("L", names(model$coefficients))]
  i <- 1
  name <- names(coeff)

  vars <- coeff

  # easy, straight forward derivation of betas and variable names
  while (i <= length(coeff)) {
    j <- 1
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
        newString <- paste0(nam[[1]][1], nam[[1]][2], nam[[1]][3], nam[[1]][4])
      } else {
        newString <- paste0(nam[[1]][3], nam[[1]][4])
      }
    }
    name[[i]] <- newString

    i <- i + 1
  }

  names(coeff) <- name

  return(coeff)
}

#' Check for horizontal and vertical extrapolation
#'
#' Regression model only work in a specific range and extrapolation horizontally (outside
#' the original range) or vertically (extreme norm values) might lead to inconsistent
#' results. The function generates a message, indicating extrapolation and the range of the original data.
#' @param model The regression model
#' @param minA The lower age bound
#' @param maxA The upper age bound
#' @param minL The lower norm value bound
#' @param maxL The upper norm value bound
#' @param digits The precision for rounding the norm and age data
#' @return the report
#' @export
#' @examples
#' normData <- prepareData()
#' m <- bestModel(normData)
#' print(rangeCheck(m))
rangeCheck <- function(model, minA=NULL, maxA=NULL, minL=NULL, maxL=NULL, digits=3){
  summary <- paste0("The original data for the regression model spanned from age ", round(model$minA1, digits), " to ", round(model$maxA1, digits), ", with a norm value range from ", round(model$minL1, digits), " to ", round(model$maxL1, digits), ".")
  reportOnly <- (is.null(minA)||is.null(maxA)||is.null(minL)||is.null(maxL))
  if (!reportOnly&&(minA < model$minA1 || maxA > model$maxA1)&&(minL < model$minL1 || maxL > model$maxL1)) {
    summary <- paste("Horizontal and vertical extrapolation detected. Be careful using age groups and extreme norm values outside the original sample.", summary, sep="\n")
  } else if (!reportOnly&&(minA < model$minA1 || maxA > model$maxA1)) {
    summary <- paste("Horizontal extrapolation detected. Be careful using age groups outside the original sample.", summary, sep="\n")
  } else if (!reportOnly&&(minL < model$minL1 || maxL > model$maxL1)) {
    summary <- paste("Vertical extrapolation detected. Be careful using extreme norm values exceeding the values of the original sample.", summary, sep="\n")
  }

  return(summary)
}
