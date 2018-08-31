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

    if(is.null(minAge)){
      minAge <- model$minA1
    }

    if(is.null(maxAge)){
      maxAge <- model$maxA1
    }

    if(is.null(minRaw)){
      minRaw <- model$minRaw
    }

    if(is.null(maxRaw)){
      maxRaw <- model$maxRaw
    }
    raw <- vector("list", (maxAge - minAge) / step)
    age <- vector("list", (maxAge - minAge) / step)
    normList <- vector("list", (maxAge - minAge) / step)

    i <- 1
    while (minAge <= maxAge) {
      r <-
        cNORM::predictRaw(norm, minAge, model$coefficients, minRaw, maxRaw)

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
             minRaw = 0,
             maxRaw = 1000) {

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
    if (predict < minRaw) {
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
#' indicate higher performance. Relevant f. e. in case of modelling errors
#' @return data.frame with norm scores and the predicted raw scores
#' @seealso rawTable
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' norms <- normTable(3.5, m, step=0.5)
#' @export
normTable <- function(A,
                      model,
                      minNorm = NULL,
                      maxNorm = NULL,
                      minRaw = NULL,
                      maxRaw = NULL,
                      step = 0.1,
                      descend = FALSE) {

  if(is.null(minNorm)){
    minNorm <- model$minL1
  }

  if(is.null(maxNorm)){
    maxNorm <- model$maxL1
  }

  if(is.null(minRaw)){
    minRaw <- model$minRaw
  }

  if(is.null(maxRaw)){
    maxRaw <- model$maxRaw
  }

  norm <- vector("list", (maxNorm - minNorm) / step)
  raw <- vector("list", (maxNorm - minNorm) / step)
  i <- 1
  if (!descend) {
    while (minNorm <= maxNorm) {
      i <- i + 1
      r <- cNORM::predictRaw(minNorm, A, model$coefficients, min = minRaw, max = maxRaw)

      norm[[i]] <- minNorm
      raw[[i]] <- r

      minNorm <- minNorm + step
    }
  } else {
    while (maxNorm >= minNorm) {
      i <- i + 1
      r <- cNORM::predictRaw(maxNorm, A, model$coefficients)

      norm[[i]] <- maxNorm
      raw[[i]] <- r

      maxNorm <- maxNorm - step
    }
  }

  normTable <-
    do.call(rbind, Map(data.frame, norm = norm, raw = raw))
  return(normTable)
}

#' Create a table with norm scores assigned to raw scores for a specific age based on the regression model
#'
#' This function is comparable to 'normTable', despite it reverses the assignment:
#' A table with raw scores and the according norm scores for a specific age based on the regression
#' model is generated. This way, the inverse function of the regression model is solved numerically with
#' brute force. Please specify the range of raw values, you want to cover. With higher precision
#' and smaller stepping, this function becomes computational intensive, especially when quick is set
#' to FALSE for a thorough search.
#' @param A the age
#' @param model The regression model
#' @param minRaw The lower bound of the raw score range
#' @param maxRaw The upper bound of the raw score range
#' @param minNorm Clipping parameter for the lower bound of norm scores (default 25)
#' @param maxNorm Clipping parameter for the upper bound of norm scores (default 25)
#' @param step Stepping parameter for the raw scores (default 1)
#' @param precision Precision for the norm score estimation. Lower values indicate
#' higher precision (default .01)
#' @param descend Reverse raw score order. If set to TRUE, lower raw scores
#' indicate higher performance. Relevant f. e. in case of modelling errors
#' @param quick Forces the use of a shotgun method to quickly find the norm scores
#' with the desired precision. Reproduces the same results as the thorough search in
#' case the model assumptions are met.
#' @return data.frame with raw scores and the predicted norm scores
#' @seealso normTable
#' @examples
#' # generate a norm table for the raw value range from 0 to 28 for month 7 of grade 3
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' table <- rawTable(3 + 7/12, m, 0, 28, precision=.01)
#' @export
rawTable <- function(A,
                     model,
                     minRaw = NULL,
                     maxRaw = NULL,
                     minNorm = NULL,
                     maxNorm = NULL,
                     step = 1,
                     precision = .01,
                     descend = FALSE,
                     quick = TRUE) {

  if(is.null(minNorm)){
    minNorm <- model$minL1
  }

  if(is.null(maxNorm)){
    maxNorm <- model$maxL1
  }

  if(is.null(minRaw)){
    minRaw <- model$minRaw
  }

  if(is.null(maxRaw)){
    maxRaw <- model$maxRaw
  }

  if (quick) {
    table <- rawTableQuick(A, model, minRaw, maxRaw, minNorm, maxNorm, step, precision, descend)
    # consistency check
    k <- 1
    SUCCESS <- TRUE
    while (k < nrow(table)) {
      if (table$norm[k] > table$norm[k + 1]) {
        # inconsistent results -> warning
        SUCCESS <- FALSE
      }

      k <- k + 1
    }

    if (SUCCESS) {
      return(table)
    } else {
      message("Inconsistent norms found in fast iteration, reruning analysis with quick set to FALSE ...")
    }
  }
  norm <- vector("list", (maxRaw - minRaw) / step)
  raw <- vector("list", (maxRaw - minRaw) / step)
  i <- 1
  if (!descend) {
    while (minRaw <= maxRaw) {
      i <- i + 1
      n <-
        cNORM::predictNormValue(minRaw, A, model, minNorm, maxNorm, precision)
      norm[[i]] <- n
      raw[[i]] <- minRaw

      minRaw <- minRaw + step
    }
  } else {
    while (maxRaw >= minRaw) {
      i <- i + 1
      n <-
        cNORM::predictNormValue(minRaw, A, model, minNorm, maxNorm, precision)
      norm[[i]] <- n
      raw[[i]] <- maxRaw

      maxRaw <- maxRaw - step
    }
  }

  table <-
    do.call(rbind, Map(data.frame, raw = raw, norm = norm))
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
    if (quick) {
      message("... still getting inconsistent entries.")
      message("Please check model consistency and/or rerun table creation with quick set to FALSE and higher precision.")
    } else {
      message("The raw table generation yielded inconsistent entries. Please check model consistency and/or rerun table creation with quick set to FALSE and higher precision.")
    }
    print(cNORM::rangeCheck(model, A, A, minNorm, maxNorm))
  } else if (quick) {
    message("... done! Everything worked fine.")
  }

  return(table)
}

#' Internal method for two step shotgun search of raw -> norm value table
#'
#' This function is comparable to 'rawTable', but uses an optimization to reduce computational
#' resources. It is the default method in the 'rawTable' function. TODO: It still has to be checked for
#' descending values.
#'
#' @param A the age
#' @param model The regression model
#' @param minRaw The lower bound of the raw score range
#' @param maxRaw The upper bound of the raw score range
#' @param minNorm Clipping parameter for the lower bound of norm scores (default 25)
#' @param maxNorm Clipping parameter for the upper bound of norm scores (default 75)
#' @param step Stepping parameter for the raw values (default 1)
#' @param precision Precision for the norm score estimation. Lower values indicate
#' higher precision (default .1)
#' @param descend Reverse raw value order. If set to TRUE, lower raw scores
#' indicate higher performance. Relevant f. e. in case of modelling errors
#' @return data.frame with raw scores and the predicted norm scores
rawTableQuick <- function(A,
                          model,
                          minRaw = NULL,
                          maxRaw = NULL,
                          minNorm = NULL,
                          maxNorm = NULL,
                          step = 1,
                          precision = .1,
                          descend = FALSE) {

  if(is.null(minNorm)){
    minNorm <- model$minL1
  }

  if(is.null(maxNorm)){
    maxNorm <- model$maxL1
  }

  if(is.null(minRaw)){
    minRaw <- model$minRaw
  }

  if(is.null(maxRaw)){
    maxRaw <- model$maxRaw
  }

  norm <- vector("list", (maxRaw - minRaw) / step)
  raw <- vector("list", (maxRaw - minRaw) / step)
  i <- 0
  if (!descend) {
    # first path, low precision
    normTab <-
      cNORM::normTable(A, model, minNorm, maxNorm, precision * 10)
    rows <- nrow(normTab)
    lowestRaw <- normTab$raw[[1]]
    highestRaw <- normTab$raw[[rows]]

    lowestNorm <- normTab$norm[[1]]
    highestNorm <- normTab$norm[[rows]]

    while (minRaw <= maxRaw) {
      i <- i + 1
      if(minRaw < lowestRaw) {
        norm[[i]] <- lowestNorm
      } else if (minRaw > highestRaw) {
        norm[[i]] <- highestNorm
      } else {
        # second path with high precision
        index <- which.min(abs(normTab$raw - minRaw))
        if (index <= 2) {
          mi <- lowestNorm
          ma <- normTab$norm[[3]]
        } else if (index >= rows - 2) {
          mi <- rows - 2
          ma <- rows
        } else {
          mi <- index - 1
          ma <- index + 1
        }

        n <-
          cNORM::predictNormValue(minRaw, A, model, normTab$norm[[mi]], normTab$norm[[ma]], precision)
        norm[[i]] <- n
      }

      raw[[i]] <- minRaw
      minRaw <- minRaw + step
    }
  } else {
    while (maxRaw >= minRaw) {
      i <- i + 1
      n <- cNORM::predictNormValue(minRaw, A, model, minNorm, maxNorm, precision)
      norm[[i]] <- n
      raw[[i]] <- maxRaw

      maxRaw <- maxRaw - step
    }

    # first step, low precision
    normTab <-
      cNORM::normTable(A, model, minNorm, maxNorm, precision * 10, descend = TRUE)
    rows <- nrow(normTab)
    lowestRaw <- normTab$raw[[1]]
    highestRaw <- normTab$raw[[rows]]

    lowestNorm <- normTab$norm[[1]]
    highestNorm <- normTab$norm[[rows]]

    while (maxRaw >= minRaw) {
      i <- i + 1
      if (maxRaw > lowestRaw) {
        norm[[i]] <- lowestNorm
      } else if (maxRaw < highestRaw) {
        norm[[i]] <- highestNorm
      } else {
        # second step with high precision
        index <- which.min(abs(normTab$raw - minRaw))
        if (index <= 2) {
          mi <- rows - 2
          ma <- rows
        } else if (index >= rows - 2) {
          mi <- lowestNorm
          ma <- normTab$norm[[3]]
        } else {
          mi <- index - 1
          ma <- index + 1
        }

        n <-
          cNORM::predictNormValue(minRaw, A, model, normTab$norm[[ma]], normTab$norm[[mi]], precision)
        norm[[i]] <- n
      }

      raw[[i]] <- maxRaw
      maxRaw <- maxRaw - step
    }
  }

  table <-
    do.call(rbind, Map(data.frame, raw = raw, norm = norm))
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

    if(is.null(minNorm)){
      minNorm <- model$minL1
    }

    if(is.null(maxNorm)){
      maxNorm <- model$maxL1
    }

        norm <- vector("list", 1 + (maxNorm - minNorm) / step)
    raw <- vector("list", 1 + (maxNorm - minNorm) / step)
    i <- 1
    coeff <- cNORM::derive(model)
    while (minNorm <= maxNorm) {
      i <- i + 1
      r <- cNORM::predictRaw(minNorm, A, coeff, min = -1000, max = 1000)

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
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param precision The precision for the norm score generation with lower values
#' indicating a higher precision. In case of T scores, precision = 0.1 is sufficient.
#' @return The predicted norm score for a raw score, either single value or list of results
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#'
#' # return norm value for raw value 21 for grade 2, month 9
#' # Use 'as.list(normData$raw)' and 'as.list(normData$group)' for raw scores
#' # and age to calculate predicted norm values for original data.
#' specificNormValue <- predictNormValue(raw = 21, A = 2.75, model = m)
#'
#' @export
predictNormValue <-
  function(raw,
             A,
             model,
             minNorm = NULL,
             maxNorm = NULL,
             minRaw = NULL,
             maxRaw = NULL,
             precision = 0.1) {

    if(is.null(minNorm)){
      minNorm <- model$minL1
    }

    if(is.null(maxNorm)){
      maxNorm <- model$maxL1
    }

    if(is.null(minRaw)){
      minRaw <- model$minRaw
    }

    if(is.null(maxRaw)){
      maxRaw <- model$maxRaw
    }

    if(length(raw)==1&&length(A)==1&&is.numeric(raw)&&is.numeric(A)){
    norms <-
      cNORM::normTable(A,
        model,
        minNorm = minNorm,
        maxNorm = maxNorm,
        minRaw = minRaw,
        maxRaw = maxRaw,
        step = precision
      )
    index <- which.min(abs(norms$raw - raw))
    return(norms$norm[index])
    } else if(is.vector(raw)&&is.vector(A)){
      if(length(raw)!=length(A)){
        stop("'A' and 'raw' need to have the same length.")
      }
      message("This might take some time. Processing case ... ")
      n <- length(raw)
      values <- rep(NA, n)
      i <- 1
      while(i <= n){

        if(i%%10==0){
          message(i)
        }

        norms <-
          cNORM::normTable(A[[i]],
                           model,
                           minNorm = minNorm,
                           maxNorm = maxNorm,
                           minRaw = minRaw,
                           maxRaw = maxRaw,
                           step = precision
          )
        index <- which.min(abs(norms$raw - raw[[i]]))
        values[[i]] <- norms$norm[index]
        i <- i + 1
      }
      return(values)
    }else {
      stop("Please check raw and A value.")
    }
  }
