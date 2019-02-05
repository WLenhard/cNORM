

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
#' Use \code{checkConsistency(model)} to check the model for violations.
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
#' @param predictors List of the names of predictor to use for the model selection.
#' The parameter overrides the 'k' parameter and it can be used to preselect the
#' variables entering the regression, or even to add variables like sex, that are
#' not part of the original model building. Please not, that adding other variables
#' than those based on L and A, plotting, prediction and normTable function will most
#' likely not work, but at least the regression formula can be obtained that way.
#' @param force.in List of variable names forced into the regression function. This option
#' can be used to force the regression to include covariates like sex or other background
#' variables. This can be used to model separate norm scales for different groups in order
#' the sample. Variables specified here, that are not part of the initial regression function
#' resp. list of predictors, are ignored without further notice and thus do not show up in
#' the final result. Additionally, all other functions like norm table generation and plotting
#' are so far not yet prepared to handle covariates.
#' @return The model meeting the R2 criteria with coefficients and variable selection
#' in model$coefficients. Use \code{plotSubset(model)} and
#' \code{plotPercentiles(data, model)} to inspect model
#' @examples
#' \dontrun{
#' # Standard example with sample data
#' normData <- prepareData()
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
#' bmi.data <- prepareData(CDC, raw="bmi", group="group", age="age")
#' bmi.model <- bestModel(bmi.data, raw="bmi")
#' printSubset(bmi.model)
#'
#' # Custom list of predictors (based on k = 3) and forcing in the sex variable
#' # While calculating the regression model works well, all other functions like
#' # plotting and norm table generation are not yet prepared to use covariates
#' bmi.sex <- bestModel(bmi.data, raw="bmi", predictors = c("L1", "L2", "L3",
#'                      "A1", "A2", "A3", "L1A1", "L1A2", "L1A3", "L2A1", "L2A2",
#'                      "L2A3", "L3A1", "L3A2", "L3A3", "sex"), force.in = c("sex"))
#'}
#' @seealso plotSubset, plotPercentiles, plotPercentileSeries, checkConsistency
#' @export
bestModel <- function(data,
                      raw = NULL,
                      R2 = 0.99,
                      k = NULL,
                      predictors = NULL,
                      terms = 0,
                      force.in = NULL) {

  # retrieve attributes
  if (is.null(raw)) {
    raw <- attr(data, "raw")
  }

  if (is.null(k)) {
    k <- attr(data, "k")
  } else if (k > attr(data, "k")) {
    stop("k parameter exceeds the power degrees in the dataset. Please use computePowers with a higher degree in preparating the data. ")
  }


  # check variable range
  if (R2 <= 0 || R2 >= 1) {
    stop("R2 parameter out of bounds.")
  }

  if (terms < 0) {
    stop("terms parameter out of bounds. The value has to be positive.")
  }

  if ((k < 1 || k > 6) & is.null(predictors)) {
    stop("k parameter out of bounds. Please specify a value between 1 and 6 (default = 4).")
  }

  if (!(raw %in% colnames(data))) {
    stop(paste(c("ERROR: Raw value variable '", raw, "' does not exist in data object."), collapse = ""))
  }

  if ((!is.null(predictors)) && (!(predictors %in% colnames(data)))) {
    stop("ERROR: Missing variables from predictors variable. Please check variable list.")
  }

  if (!is.null(predictors)) {
    lmX <- formula(paste(raw, paste(predictors, collapse = " + "), sep = " ~ "))
  } else if (k == 1) {
    lmX <- formula(paste(raw, "L1 + A1 + L1A1", sep = " ~ "))
  } else if (k == 2) {
    lmX <-
      formula(paste(raw, "L1 + L2 + A1 + A2 + L1A1 + L1A2 + L2A1 + L2A2", sep = " ~ "))
  } else if (k == 3) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + A1 + A2 + A3 + L1A1 + L1A2 + L1A3 + L2A1 + L2A2 + L2A3 + L3A1 + L3A2 + L3A3", sep = " ~ "))
  } else if (k == 4) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + A1 + A2 + A3 + A4 + L1A1 + L1A2 + L1A3 + L1A4 + L2A1 + L2A2 + L2A3 + L2A4 + L3A1 + L3A2 + L3A3 + L3A4 + L4A1 + L4A2 + L4A3 + L4A4", sep = " ~ "))
  } else if (k == 5) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + A1 + A2 + A3 + A4 + A5 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5", sep = " ~ "))
  } else if (k == 6) {
    lmX <-
      formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + L6 + A1 + A2 + A3 + A4 + A5 + A6 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L1A6 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L2A6 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L3A6 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L4A6 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5 + L5A6 + L6A1 + L6A2 + L6A3 + L6A4 + L6A5 + L6A6", sep = " ~ "))
  } else {
    message("Power parameter unknown, setting to k = 4")
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + A1 + A2 + A3 + A4 + L1A1 + L1A2 + L1A3 + L1A4 + L2A1 + L2A2 + L2A3 + L2A4 + L3A1 + L3A2 + L3A3 + L3A4 + L4A1 + L4A2 + L4A3 + L4A4", sep = " ~ "))
  }

  big <- FALSE
  nvmax <- 2 * k + k * k + length(predictors)

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

  subsets <- leaps::regsubsets(lmX, data = data, nbest = 1, nvmax = nvmax, force.in = index, really.big = big)
  results <- summary(subsets)

  i <- 1
  rAdj <- results$adjr2[i]

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
      report <- (paste0(
        "Specified R2 exceeds the R2 of the model with the highest fit. Consider rerunning the analysis with higher k value. Falling back to model ", i
      ))
    } else {
      while (rAdj < R2) {
        i <- i + 1
        rAdj <- results$adjr2[i]
      }
      report <- paste0("Final solution: ", i, " terms")
    }
  }
  report[2] <- paste0("R-Square Adj. = ", round(results$adjr2[i], digits = 6))

  variables <- names(coef(subsets, id = i))
  variables <- variables[2:length(variables)] # remove '(Intercept)' variable
  text <- paste0(raw, " ~ ", paste(variables, collapse = " + ")) # build regression formula

  report[3] <- paste0("Final regression model: ", text)
  bestformula <- lm(text, as.data.frame(data))

  # compute rmse
  tab <- data.frame(raw = data[, raw], fitted = bestformula$fitted.values)
  tab <- tab[complete.cases(tab), ]
  rmse <- sqrt(sum((tab$raw - tab$fitted)^2) / length(tab$raw))

  # Model information
  bestformula$ideal.model <- i
  bestformula$cutoff <- R2
  bestformula$subsets <- results

  # add information for horizontal and vertical extrapolation
  bestformula$minA1 <- min(data$A1)
  bestformula$maxA1 <- max(data$A1)
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

  # Print output
  report[4] <- paste0("Regression function: ", regressionFunction(bestformula, digits = 10))
  report[5] <- paste0("Raw Score RMSE = ", round(rmse, digits = 5))

  bestformula$report <- report
  cat(report, sep = "\n")

  message("Use 'printSubset(model)' to get detailed information on the different solutions, 'plotSubset(model)' to inspect model fit and 'summary(model)' for statistics on the regression model.")
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
printSubset <- function(model) {
  table <-
    do.call(rbind, Map(data.frame,
      R2 = model$subsets$rsq,
      R2adj = model$subsets$adjr2,
      RSS = model$subsets$rss,
      RMSE = sqrt(model$subsets$rss / length(model$fitted.values)),
      Cp = model$subsets$cp,
      BIC = model$subsets$bic
    ))
  return(table)
}

#' Check the consistency of the norm data model
#'
#' While abilities increase and decline over age, within one age group, the
#' norm scores always have to show a linear increase with increasing raw
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
#' @param model The model from the bestModel function
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
#' @return Boolean, indicating model violations (TRUE) or no problems (FALSE)
#' @examples
#' normData <- prepareData()
#' m <- bestModel(normData)
#' modelViolations <- checkConsistency(m, minAge=2, maxAge=5, stepAge=0.1,
#'                    minNorm=25, maxNorm=75, minRaw=0, maxRaw= 28, stepNorm=1)
#' plotDerivative(m, , minAge=2, maxAge=5, minNorm=25, maxNorm=75)
#' @export
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
                             silent = FALSE) {
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
  minor <- 0
  major <- 0
  results <- c()
  while (i <= maxAge) {
    norm <- normTable(i, model, minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw, step = stepNorm)
    k <- 1
    maxR <- 0
    while (k < length(norm$raw)) {
      if (norm$raw[[k]] > maxR) {
        maxR <- norm$raw[[k]]
      }
      diff <- maxR - norm$raw[[k + 1]]
      if ((!descend && diff >= 1) || (descend && diff <= -1)) {
        if (!silent) {
          message(paste0(
            "Considerable violation of consistency at age ",
            round(i, digits = 1), ", raw value ",
            round(norm$raw[[k]],
              digits = 1
            )
          ))
        }
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
        if (!silent) {
          message(paste0(
            "Negligible violation of consistency at age ",
            round(i, digits = 1),
            ", raw value ",
            round(norm$raw[[k]],
              digits = 1
            )
          ))
        }
        results <- c(results, paste0(
          results, "Negligible violation of
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
    if (!silent) {
      message("\nNo violations of model consistency found.")
    }
    return(FALSE)
  } else if (major == 0) {
    if (!silent) {
      message(paste0("\n", minor, " minor violations of model consistency found."))
      message(rangeCheck(model, minAge, maxAge, minNorm, maxNorm))
    }
    return(TRUE)
  } else {
    if (!silent) {
      message(paste0("\nAt least ", major, " major and ", minor, " minor violations of model consistency found."))
      message("Use 'plotNormCurves' to visually inspect the norm curve and restrict the valid value range accordingly.")
      message("Be careful with horizontal and vertical extrapolation.")
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
regressionFunction <- function(model, raw = NULL, digits = NULL) {
  if (is.null(raw)) {
    raw <- model$raw
  }

  i <- 2
  if (is.null(digits)) {
    formulA <- paste(raw, model$coefficients[[1]], sep = " ~ ")
    while (i <= length(model$coefficients)) {
      formulA <- paste0(
        formulA, " + (", model$coefficients[[i]], "*",
        names(model$coefficients[i]), ")"
      )
      i <- i + 1
    }
  } else {
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

#' Derivative of regression model
#'
#' Calculates the derivative of the location / norm value from the regression model with the first
#' derivative as the default. This is useful for finding violations of model assumptions and problematic
#' distribution features as f. e. bottom and ceiling effects, non-progressive norm scores within an
#' age group or in general #' intersecting percentile curves.
#' @param model The regression model
#' @param order The degree of the derivate, default: 1
#' @return The derived coefficients
#' @examples
#' normData <- prepareData()
#' m <- bestModel(normData)
#' derivedCoefficients <- derive(m)
#' @export
derive <- function(model, order = 1) {
  coeff <- model$coefficients[grep("L", names(model$coefficients))]


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
          newString <- paste0(nam[[1]][1], nam[[1]][2], nam[[1]][3], nam[[1]][4])
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

#' Check for horizontal and vertical extrapolation
#'
#' Regression model only work in a specific range and extrapolation horizontally (outside
#' the original range) or vertically (extreme norm scores) might lead to inconsistent
#' results. The function generates a message, indicating extrapolation and the range of the original data.
#' @param model The regression model
#' @param minAge The lower age bound
#' @param maxAge The upper age bound
#' @param minNorm The lower norm value bound
#' @param maxNorm The upper norm value bound
#' @param digits The precision for rounding the norm and age data
#' @return the report
#' @export
#' @examples
#' normData <- prepareData()
#' m <- bestModel(normData)
#' print(rangeCheck(m))
rangeCheck <- function(model, minAge = NULL, maxAge = NULL, minNorm = NULL, maxNorm = NULL, digits = 3) {
  summary <- paste0("The original data for the regression model spanned from age ", round(model$minA1, digits), " to ", round(model$maxA1, digits), ", with a norm score range from ", round(model$minL1, digits), " to ", round(model$maxL1, digits), ".")
  reportOnly <- (is.null(minAge) || is.null(maxAge) || is.null(minNorm) || is.null(maxNorm))
  if (!reportOnly && (minAge < model$minA1 || maxAge > model$maxA1) && (minNorm < model$minL1 || maxNorm > model$maxL1)) {
    summary <- paste("Horizontal and vertical extrapolation detected. Be careful using age groups and extreme norm scores outside the original sample.", summary, sep = "\n")
  } else if (!reportOnly && (minAge < model$minA1 || maxAge > model$maxA1)) {
    summary <- paste("Horizontal extrapolation detected. Be careful using age groups outside the original sample.", summary, sep = "\n")
  } else if (!reportOnly && (minNorm < model$minL1 || maxNorm > model$maxL1)) {
    summary <- paste("Vertical extrapolation detected. Be careful using extreme norm scores exceeding the scores of the original sample.", summary, sep = "\n")
  }

  return(summary)
}

#' Cross validation for term selection
#'
#' This function helps in selecting the number of terms for the model by doing repeated
#' cross validation with 80 percent of the data as training data and 20 percent as the validation data.
#' The cases are drawn randomly but stratified by norm group. Successive models are retrieved
#' with increasing number of terms and the RMSE of raw scores (fitted by the regression model)
#' is plotted for the training, validation and the complete dataset. Additionally to this
#' analysis on the raw score level, it is possible (default) to estimate the mean norm score
#' reliability and crossfit measures. For this, please set the norms parameter to TRUE. Due
#' to the high computational load when computing norm scores, it takes time to finish
#' when doing repeated cv or comparing models up to the maximum number of terms. When using
#' the cv = "full" option, the ranking is done for the test and validation dataset
#' separately (always based on T scores), resulting in a complete cross validation. In
#' order to only validate the modeling, you as well can use a pre ranked data set with
#' prepareData() already applied. In this case, the training and validation data is
#' drawn from the already ranked data and the scores for the validation set should improve.
#' It is however no independent test, as the ranking between both samples is interlinked.
#'
#' In the output, you will get RMSE for the raw score models, norm score R2 and delta R2 and the crossfit.
#' For assessing, if a model overfits the data and to what extent, we need cross-validation. We assumed
#' that an overfitting occurred when a model captures more variance of the observed norm scores of the
#' training sample compared to the captured variance of the norm scores of the validation sample. The
#' overfit can therefore be described as:
#'
#' CROSSFIT = R(Training; Model)^2 / R(Validation; Model)^2
#'
#' A CROSSFIT higher than 1 is a sign of overfitting. Value lower than 1 indicate an underfit due to a
#' suboptimal modelling procedure, i. e. the method may not have captured all the variance of the observed
#' data it could possibly capture. Values around 1 are ideal, as long as the raw score RMSE is low and the
#' norm score validation R2 reaches high levels. As a suggestion for real tests: Combine visual inspection
#' of the percentiles with a repeated cross validation (e. g. 10 repetitions). Fokus on low raw score RMSE,
#' high norm score R2 in the validation dataset and avoid models with a high overfit (e. g. crossfit > 1.1).
#'
#' @param data data frame of norm sample with ranking, powers and interaction of L and A
#' @param repetitions number of repetitions for cross validation
#' @param norms determine norm score crossfit and R2 (if set to TRUE). The option is
#' computationally intensive and duration increases with sample size, number of
#' repetitions and maximum number of terms (max option).
#' @param min Minimum number of terms to start from, default = 1
#' @param max Maximum number of terms in model up to 2*k + k^2
#' @param cv If set to full (default), the data is split into training and validation data and ranked afterwards,
#' otherwise, a pre ranked dataset has to be provided, which is then split into train and validation (and thus
#' only the modelling, but not the ranking is independent)
#' @param pCutoff The function checks the stratification for unbalanced data sampling.
#' It performs a t-test per group . pCutoff specifies the p-value per group that the test result
#' has to reach at least. To minimize beta error, the value is set to .2 per default
#' @param width If provided, ranking is done via rankBySlidingWindow, otherwise by group
#' @param raw Name of the raw variable
#' @param age Name of the age variable
#' @param group Name of the grouping variable
#' @return table with results per term number, including RMSE for raw scores in training, validationand complete
#' sample, R2 for the norm scores and the crossfit measure (1 = ideal, <1 = underfit, >1 = overfit)
#' @export
#' @examples
#' # plot cross validation RMSE by number of terms up to 9 with three repetitions
#' data <- prepareData()
#' cnorm.cv(data, 3, max=7, norms=FALSE)
cnorm.cv <- function(data, repetitions = 1, norms = TRUE, min = 1, max = 12, cv = "full", pCutoff = .2, width = NA, raw = NA, group = NA, age = NA) {

  d <- data

  if(is.na(raw)||is.na(group)||is.na(age)){
      raw <- attr(d, "raw")
      age <- attr(d, "age")
      group <- attr(d, "group")
  }

  if(is.na(raw)||is.na(group)||is.na(age)){
    stop("Variables raw, age and group neither available as function parameters nor as attributes from data object. Please provide according information.")
  }

  scaleM <- attr(d, "scaleMean")
  if(is.na(scaleM) || cv=="full"){
    scaleM <- 50
  }

  scaleSD <- attr(d, "scaleSD")
  if(is.na(scaleSD) || cv=="full"){
    scaleSD <- 10
  }

  k <- attr(d, "k")
  if(is.na(k)){
    k <- 4
  }

  n.models <- 2 * k + k * k
  if(is.na(max) || max > n.models || max < 1){
    max <- n.models
  }

  # set up regression formulas (from bestModel function)
  if (k == 1) {
    lmX <- formula(paste(raw, "L1 + A1 + L1A1", sep = " ~ "))
  } else if (k == 2) {
    lmX <-
      formula(paste(raw, "L1 + L2 + A1 + A2 + L1A1 + L1A2 + L2A1 + L2A2", sep = " ~ "))
  } else if (k == 3) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + A1 + A2 + A3 + L1A1 + L1A2 + L1A3 + L2A1 + L2A2 + L2A3 + L3A1 + L3A2 + L3A3", sep = " ~ "))
  } else if (k == 4) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + A1 + A2 + A3 + A4 + L1A1 + L1A2 + L1A3 + L1A4 + L2A1 + L2A2 + L2A3 + L2A4 + L3A1 + L3A2 + L3A3 + L3A4 + L4A1 + L4A2 + L4A3 + L4A4", sep = " ~ "))
  } else if (k == 5) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + A1 + A2 + A3 + A4 + A5 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5", sep = " ~ "))
  } else if (k == 6) {
    lmX <-
      formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + L6 + A1 + A2 + A3 + A4 + A5 + A6 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L1A6 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L2A6 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L3A6 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L4A6 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5 + L5A6 + L6A1 + L6A2 + L6A3 + L6A4 + L6A5 + L6A6", sep = " ~ "))
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

  # draw test and training data several times ('repetitions' parameter), model data and store MSE
  for (a in 1:repetitions) {

    # check for imbalances in data and repeat if stratification was unsatisfactory - usually never occurs
    p.value <- .01
    n <- 1  # to avoid a deadlock, define stop criterion

    while (p.value < pCutoff) {
      if(n > 100){
        stop("Could not establish balanced data sets. Try to decrease pCutoff parameter.")
      }
      n <- n + 1

      # shuffle data and split into groups (for stratification)
      d <- d[sample(nrow(d)), ]
      d <- d[order(d[, group]), ]
      sp <- split(d, list(d[, group]))
      sp <- lapply(sp, function(x) x[sample(nrow(x)), ])

      # draw 9 tenth of data from each group for training and testing
      train <- lapply(sp, function(x) x[c(FALSE, rep(TRUE, 4)), ])
      test <- lapply(sp, function(x) x[c(TRUE, rep(FALSE, 4)), ])

      # test for significant differences to avoid extremely unbalanced data
      p <- rep(1, length(train))
      for(z in 1:length(train)){
        p[z] <- t.test(train[[z]][, raw], test[[z]][, raw])$p.value
      }
      p.value <- min(p)
      if(p.value < pCutoff){
        next
      }

      # combine lists to data frames
      train <- do.call(rbind, train)
      test <- do.call(rbind, test)

      if(cv=="full"){
        train <- prepareData(train, raw=raw, group=group, age=age)
        test <- prepareData(test, raw=raw, group=group, age=age)
      }
      # test for overall significant differences between groups, restart stratification if necessary
      #p.value <- t.test(train[, raw], test[, raw])$p.value
    }

    # compute leaps model
    subsets <- leaps::regsubsets(lmX, data = train, nbest = 1, nvmax = max, really.big = n.models > 25)

    # retrieve models coefficients for each number of terms
    for (i in min:max) {
      cat(paste0("Repetition " , a, ", cycle ", i, "\n"))
      variables <- names(coef(subsets, id = i))
      variables <- variables[2:length(variables)] # remove '(Intercept)' variable
      reg <- paste0(raw, " ~ ", paste(variables, collapse = " + ")) # build regression formula

      # run linear regression for specific model
      model <- lm(reg, train)
      model$k <- k
      model$minRaw <- min(train[, raw])
      model$maxRaw <- max(train[, raw])
      model$scaleM <- scaleM
      model$scaleSD <- scaleSD


      # predict values in test data
      test.fitted <- predict.lm(model, test)

      # store MSE for test and train data
      train.errors[i] <- train.errors[i] + mean((model$fitted.values - train[, raw])^2)
      val.errors[i] <- val.errors[i] + mean((test.fitted - test[, raw])^2)

      # compute R2 for test and training
      if(norms){
        train$T <- predictNorm(train[, raw], train[, age], model, min(train$normValue), max(train$normValue))
        test$T <- predictNorm(test[, raw], test[, age], model, min(train$normValue), max(train$normValue))

        r2.train[i] <- r2.train[i] + (cor(train$normValue, train$T, use = "pairwise.complete.obs")^2)
        r2.test[i] <- r2.test[i] + (cor(test$normValue, test$T, use = "pairwise.complete.obs")^2)
      }
    }
  }

  # now for the complete data the same logic
  complete <- leaps::regsubsets(lmX, data = d, nbest = 1, nvmax = n.models, really.big = n.models > 25)
  for (i in 1:max) {
    variables <- names(coef(complete, id = i))
    variables <- variables[2:length(variables)]
    reg <- paste0(raw, " ~ ", paste(variables, collapse = " + "))
    model <- lm(reg, d)

    # mse for the complete data based on number of terms
    complete.errors[i] <- sqrt(mean((model$fitted.values - d[, raw])^2))

    # build the average over repetitions and the root
    train.errors[i] <- sqrt(train.errors[i] / repetitions)
    val.errors[i] <- sqrt(val.errors[i] / repetitions)

    if(norms){
      r2.train[i] <- r2.train[i] / repetitions
      r2.test[i] <- r2.test[i] / repetitions

      if(i > min){
        delta[i] <- r2.test[i] - r2.test[i - 1]
      }
    }

    if(i < min){
      r2.train[i] <- NA
      r2.test[i] <- NA
      val.errors[i] <- NA
      train.errors[i] <- NA
      complete.errors[i] <- NA
    }
  }

  if(norms){
    par(mfrow = c(2, 2)) # set the plotting area into a 1*2 array
  }else{
    par(mfrow = c(1, 1))
  }
  tab <- data.frame(RMSE.raw.train = train.errors, RMSE.raw.test = val.errors, RMSE.raw.complete = complete.errors, r2.train = r2.train, r2.test = r2.test, delta.r2.test = delta, crossfit = r2.train / r2.test)

  # plot RMSE
  plot(val.errors, pch = 19, type = "b", col = "blue", main = "Raw Score RMSE", ylab = "Root MSE", xlab = "Number of terms", ylim=c(min(train.errors, na.rm = TRUE),max(val.errors, na.rm = TRUE)))
  points(complete.errors, pch = 19, type = "b", col = "black")
  points(train.errors, pch = 19, type = "b", col = "red")
  legend("topright", legend = c("Training", "Validation", "Complete"), col = c("red", "blue", "black"), pch = 19)

  if(norms){
    # plot R2
    plot(r2.train, pch = 19, type = "b", col = "red", main = "Norm Score R2", ylab = "R Square", xlab = "Number of terms", ylim=c(min(r2.test, na.rm = TRUE),1))
    points(r2.test, pch = 19, type = "b", col = "blue")
    legend("bottomright", legend = c("Training", "Validation"), col = c("red", "blue"), pch = 19)

    # plot CROSSFIT
    plot(tab$crossfit, pch = 19, type = "b", col = "black", main = "Norm Score CROSSFIT", ylab = "Crossfit", xlab = "Number of terms", ylim=c(min(c(tab$crossfit, .88), na.rm = TRUE),max(c(tab$crossfit, 1.12), na.rm = TRUE)))
    abline(h = 1, col = 3, lty = 2)
    abline(h = .9, col = 2, lty = 3)
    text(max, .89, adj = c(1,1), "underfit", col=2, cex = .75)
    abline(h = 1.1, col = 2, lty = 3)
    text(max, 1.11, adj = c(1,0), "overfit", col=2, cex = .75)

    # plot delta r2 test
    plot(tab$delta.r2.test, pch = 19, type = "b", col = "black", main = "Norm Score Delta R2 in Validation", ylab = "Delta R2", xlab = "Number of terms", ylim=c(min(tab$delta.r2.test, na.rm = TRUE),max(tab$delta.r2.test, na.rm = TRUE)))
    abline(h = 0, col = 3, lty = 2)
  }
  cat("The simulation yielded the following optimal settings:\n")
  cat(paste0("\nNumber of terms with best crossfit: ", which.min((1-tab$crossfit)^2)))
  cat(paste0("\nNumber of terms with best raw validation RMSE: ", which.min(tab$RMSE.raw.test)))
  cat(paste0("\nNumber of terms with best norm validation R2: ", which.max(r2.test)))
  cat("\nPlease investiate the plots and the summary table, as the results might vary within a narrow range.")
  cat("\nEspacially pay attention to RMSE.raw.test, r2.test, crossfit near 1 and where delta R2 stops to progress.")
  cat("\n")
  return(tab)
}
