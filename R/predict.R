#' Computes the curve for a specific T value
#'
#' As with this continuous norming regression approach, raw scores are modeled as a function of age and norm score
#' (location), getNormCurve is a straightforward approach to show the raw score development over
#' age, while keeping the norm value constant. This way, e. g. academic performance or intelligence development
#' of a specific ability is shown.
#' @param norm The specific norm score, e. g. T value
#' @param model The model from the regression modeling
#' @param minAge Age to start from
#' @param maxAge Age to stop at
#' @param step Stepping parameter for the precision when retrieving of the values, lower
#' values indicate higher precision (default 0.1).
#' @param minRaw lower bound of the range of raw scores (default = 0)
#' @param maxRaw upper bound of raw scores
#' @return data.frame of the variables raw, age and norm
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' getNormCurve(35, m)
#' @export
getNormCurve <-
  function(norm,
             model,
             minAge = NULL,
             maxAge = NULL,
             step = 0.1,
             minRaw = NULL,
             maxRaw = NULL) {
    if (is.null(minAge)) {
      minAge <- model$minA1
    }

    if (is.null(maxAge)) {
      maxAge <- model$maxA1
    }

    if (is.null(minRaw)) {
      minRaw <- model$minRaw
    }

    if (is.null(maxRaw)) {
      maxRaw <- model$maxRaw
    }
    raw <- vector("list", (maxAge - minAge) / step)
    age <- vector("list", (maxAge - minAge) / step)
    normList <- vector("list", (maxAge - minAge) / step)

    i <- 1
    while (minAge <= maxAge) {
      r <-
        predictRaw(norm, minAge, model$coefficients, minRaw, maxRaw)

      raw[[i]] <- r
      age[[i]] <- minAge
      normList[[i]] <- paste(norm, "T")
      minAge <- minAge + step
      i <- i + 1
    }
    curve <-
      do.call(
        rbind,
        Map(
          data.frame,
          norm = normList,
          age = age,
          raw = raw
        )
      )
    return(curve)
  }

#' Predict single raw value
#'
#' Most elementary function to predict raw score based on Location (L, T score),
#' Age (grouping variable) and the coefficients from a regression model.
#' WARNING! This function, and all functions  depending on it, only works with regression
#' functions including L, A and interactions. Manually adding predictors to bestModel via the
#' predictors parameter is currently incompatible.
#' In that case, and if you are primarily interested on fitting a complete data set,
#' rather user the predict function of the stats:lm package on the ideal model solution.
#' You than have to provide a prepared data frame with the according input variables.
#' @param norm The norm score, e. g. a specific T score
#' @param age The age value
#' @param coefficients The coefficients from the regression model
#' @param minRaw Minimum score for the results; can be used for clipping unrealistic outcomes,
#' usually set to the lower bound of the range of values of the test (default: 0)
#' @param maxRaw Maximum score for the results; can be used for clipping unrealistic outcomes
#' usually set to the upper bound of the range of values of the test
#' @return the predicted raw score
#' @examples
#' # Prediction of single scores
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' predictRaw(35, 3.5, m$coefficients)
#'
#' # Fitting complete data sets
#' fitted.values <- predict(m)
#'
#' # break up contribution of each predictor variable
#' fitted.partial <- predict(m, type = "terms")
#' @export
predictRaw <-
  function(norm,
             age,
             coefficients,
             minRaw = -Inf,
             maxRaw = Inf) {

    # first intercept
    coef <- coefficients
    predict <- 0

    i <- 1
    while (i <= length(coef)) {
      nam <- strsplit(names(coef[i]), "")
      p <- coef[[i]][1]

      # first variable, either L or A
      if (length(nam[[1]]) < 2 | length(nam[[1]]) > 4) {
        # nothing to do
      } else if (nam[[1]][1] == "L") {
        j <- 1
        while (j <= nam[[1]][2]) {
          p <- p * norm
          j <- j + 1
        }
      } else if (nam[[1]][1] == "A") {
        j <- 1
        while (j <= nam[[1]][2]) {
          p <- p * age
          j <- j + 1
        }
      }

      # in case, second factor is present
      if (length(nam[[1]]) == 4) {
        j <- 1
        while (j <= nam[[1]][4]) {
          p <- p * age
          j <- j + 1
        }
      }

      # add up
      predict <- predict + p
      i <- i + 1
    }

    # check bounds
    if (is.na(predict)) {
      warning(paste0("NA value occured in predictValue with norm score ", norm, " and age ", age))
    } else if (predict < minRaw) {
      predict <- minRaw
    } else if (predict > maxRaw) {
      predict <- maxRaw
    }

    return(predict)
  }

#' Create a norm table based on model for specific age
#'
#' This function generates a norm table for a specific age based on the regression
#' model by assigning raw scores to norm scores. Please specify the
#' range of norm scores, you want to cover. A T value of 25 corresponds to a percentile
#' of .6. As a consequence, specifying a rang of T = 25 to T = 75 would cover 98.4 % of
#' the population. Please be careful when extrapolating vertically (at the lower and
#' upper end of the age specific distribution). Depending on the size of your standardization
#' sample, extreme values with T < 20 or T > 80 might lead to inconsistent results.
#' @param A the age
#' @param model The regression model
#' @param minNorm The lower bound of the norm score range
#' @param maxNorm The upper bound of the norm score range
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param step Stepping parameter with lower values indicating higher precision
#' @param descend Reverse raw value order. If set to TRUE, lower raw values
#' indicate higher performance. Relevant f. e. in case of modeling errors
#' @return data.frame with norm scores and the predicted raw scores
#' @seealso rawTable
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' norms <- normTable(3.5, m, minNorm=25, maxNorm=75, step=0.5)
#' @export
normTable <- function(A,
                      model,
                      minNorm = NULL,
                      maxNorm = NULL,
                      minRaw = NULL,
                      maxRaw = NULL,
                      step = 0.1,
                      descend = NULL) {
  if (is.null(minNorm) || is.null(maxNorm)) {
    stop("ERROR: Please specify minimum and maximum norm score")
  }

  if (is.null(descend)) {
    descend <- model$descend
  }

  if (is.null(minRaw)) {
    warning("Minimum raw score not specified. Taking value from original dataset.")
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)) {
    warning("Maximum raw score not specified. Taking value from original dataset.")
    maxRaw <- model$maxRaw
  }

  norm <- vector("list", (maxNorm - minNorm) / step + 1)
  raw <- vector("list", (maxNorm - minNorm) / step + 1)
  i <- 1
  l <- length(norm)
  if (!descend) {
    while (i <= l) {
      r <- predictRaw(minNorm, A, model$coefficients, minRaw = minRaw, maxRaw = maxRaw)

      norm[[i]] <- minNorm
      raw[[i]] <- r

      minNorm <- minNorm + step
      i <- i + 1
    }
  } else {
    while (maxNorm >= minNorm) {
      r <- predictRaw(maxNorm, A, model$coefficients)

      norm[[i]] <- maxNorm
      raw[[i]] <- r

      maxNorm <- maxNorm - step
      i <- i + 1
    }
  }

  normTable <-
    do.call(rbind, Map(data.frame, norm = norm, raw = raw))

  if(!is.na(model$scaleM)&&!is.na(model$scaleSD)){
    normTable$percentile <- pnorm((normTable$norm - model$scaleM)/model$scaleSD) * 100
  }

  return(normTable)
}

#' Create a table with norm scores assigned to raw scores for a specific age based on the regression model
#'
#' This function is comparable to 'normTable', despite it reverses the assignment:
#' A table with raw scores and the according norm scores for a specific age based on the regression
#' model is generated. This way, the inverse function of the regression model is solved numerically with
#' brute force. Please specify the range of raw values, you want to cover. With higher precision
#' and smaller stepping, this function becomes computational intensive.
#' @param A the age
#' @param model The regression model
#' @param minRaw The lower bound of the raw score range
#' @param maxRaw The upper bound of the raw score range
#' @param minNorm Clipping parameter for the lower bound of norm scores (default 25)
#' @param maxNorm Clipping parameter for the upper bound of norm scores (default 25)
#' @param step Stepping parameter for the raw scores (default 1)
#' @param descend Reverse raw score order. If set to TRUE, lower raw scores
#' indicate higher performance. Relevant f. e. in case of modeling errors
#' @return data.frame with raw scores and the predicted norm scores
#' @seealso normTable
#' @examples
#' # generate a norm table for the raw value range from 0 to 28 for month 7 of grade 3
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' table <- rawTable(3 + 7/12, m, minRaw = 0, maxRaw = 28,
#'                   minNorm = 25, maxNorm = 75)
#' @export
rawTable <- function(A,
                     model,
                     minRaw = NULL,
                     maxRaw = NULL,
                     minNorm = NULL,
                     maxNorm = NULL,
                     step = 1,
                     descend = NULL) {
  if (is.null(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(descend)) {
    descend <- model$descend
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

  norm <- vector("list", (maxRaw - minRaw) / step)
  raw <- vector("list", (maxRaw - minRaw) / step)
  i <- 1
  if (!descend) {
    while (minRaw <= maxRaw) {
      i <- i + 1
      n <-
        predictNorm(minRaw, A, model, minNorm, maxNorm)
      norm[[i]] <- n
      raw[[i]] <- minRaw

      minRaw <- minRaw + step
    }
  } else {
    while (maxRaw >= minRaw) {
      i <- i + 1
      n <-
        predictNorm(minRaw, A, model, minNorm, maxNorm)
      norm[[i]] <- n
      raw[[i]] <- maxRaw

      maxRaw <- maxRaw - step
    }
  }

  table <-
    do.call(rbind, Map(data.frame, raw = raw, norm = norm))

  if(!is.na(model$scaleM)&&!is.na(model$scaleSD)){
    table$percentile <- pnorm((table$norm - model$scaleM)/model$scaleSD) * 100
  }

  # checking consistency
  k <- 1
  SUCCESS <- TRUE
  while (k < nrow(table)) {
    if (table$norm[k] > table$norm[k + 1]) {
      # inconsistent results -> warning
      SUCCESS <- FALSE
      message(paste("Raw ", table$raw[k], " value with inconsistent norm value", sep = ""))
    }

    k <- k + 1
  }

  if (!SUCCESS) {
    message("The raw table generation yielded inconsistent entries. Please check model consistency.")
    print(rangeCheck(model, A, A, minNorm, maxNorm))
  }

  return(table)
}



#' Create a table based on first order derivative of the regression model for specific age
#'
#' In order to check model assumptions, a table of the first order derivative of the model
#' coefficients is created.
#' @param A the age
#' @param model The regression model
#' @param minNorm The lower bound of the norm value range
#' @param maxNorm The upper bound of the norm value range
#' @param step Stepping parameter with lower values indicating higher precision
#' @return data.frame with norm scores and the predicted scores based on the
#' derived regression function
#' @seealso plotDerivative, derive
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' d <- derivationTable(6, m, step=0.5)
#' @export
derivationTable <-
  function(A,
             model,
             minNorm = NULL,
             maxNorm = NULL,
             step = 0.1) {
    if (is.null(minNorm)) {
      minNorm <- model$minL1
    }

    if (is.null(maxNorm)) {
      maxNorm <- model$maxL1
    }

    norm <- vector("list", 1 + (maxNorm - minNorm) / step)
    raw <- vector("list", 1 + (maxNorm - minNorm) / step)
    i <- 1
    coeff <- derive(model)
    while (minNorm <= maxNorm) {
      i <- i + 1
      r <- predictRaw(minNorm, A, coeff)

      norm[[i]] <- minNorm
      raw[[i]] <- r

      minNorm <- minNorm + step
    }
    normTable <-
      do.call(rbind, Map(data.frame, norm = norm, raw = raw))
    return(normTable)
  }

#' Retrieve norm value for raw score at a specific age
#'
#' In real test scenarios, usually the results are available as raw values, for
#' which norm scores have to be looked up. This function conducts this reverse
#' transformation via a numerical solution: A precise norm table is generated and
#' the closest fitting norm score for a raw score is returned.
#' @param raw The raw value, either single numeric or list of values
#' @param A the age, either single numeric or list of values
#' @param model The regression model
#' @param minNorm The lower bound of the norm score range
#' @param maxNorm The upper bound of the norm score range
#' @return The predicted norm score for a raw score, either single value or list of results
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#'
#' # return norm value for raw value 21 for grade 2, month 9
#' # Use 'as.list(normData$raw)' and 'as.list(normData$group)' for raw scores
#' # and age to calculate predicted norm values for original data.
#' specificNormValue <- predictNorm(raw = 21, A = 2.75, model = m, minNorm=25, maxNorm=75)
#'
#' @export
predictNorm <-
  function(raw,
             A,
             model,
             minNorm = NULL,
             maxNorm = NULL) {
    if (is.null(minNorm) || is.null(maxNorm)) {
      stop("ERROR: Please specify minimum and maximum norm score")
    }


    # determine single norm value by optimization
    if (length(raw) == 1 && length(A) == 1 && is.numeric(raw) && is.numeric(A)) {
      startNormScore <- minNorm
      currentRawValue <- predictRaw(norm = minNorm, age = A, coefficients = model$coefficients)

      functionToMinimize <- function(norm) {
        currentRawValue <- predictRaw(norm = norm, age = A, coefficients = model$coefficients)
        functionValue <- (currentRawValue - raw)^2
      }

      optimum <- optimize(functionToMinimize, lower = minNorm, upper = maxNorm, tol = .Machine$double.eps)
      return(optimum$minimum)
    } else if (is.vector(raw) && is.vector(A)) {
      if (length(raw) != length(A)) {
        stop("'A' and 'raw' need to have the same length.")
      }
      print("This might take some time. Please stand by ... ")
      #  initialize vectors and starting values
      n <- length(raw)
      values <- rep(NA, n)

      # iterate through cases and increase precision by factor 2 in each step
      for (i in 1:n) {
        v <- predictNormByRoots(raw[[i]], A[[i]], model, minNorm, maxNorm)
        if (length(v) == 0) {
          v <- NA
        }
        values[[i]] <- v
      }
      return(values)
    } else {
      stop("Please check raw and A value. Both have to be either single values or vectors of the same length.")
    }
  }
