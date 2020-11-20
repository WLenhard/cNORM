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
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate or
#' the specific value here.
#' @return data.frame of the variables raw, age and norm
#' @examples
#' normData <- prepareData(elfe)
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
             maxRaw = NULL,
           covariate = NULL) {
    if(!is.null(covariate)&&is.null(model$covariate)){
      warning("Covariate specified but no covariate available in the model. Setting covariate to NULL.")
      covariate = NULL
    }else if(is.null(covariate)&&!is.null(model$covariate)){
      stop("Covariate specified in the model, but no function parameter available.")
    }

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
        predictRaw(norm, minAge, model$coefficients, minRaw, maxRaw, covariate)

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
#' @param norm The norm score, e. g. a specific T score or a vector of scores
#' @param age The age value or a vector of scores
#' @param coefficients The coefficients from the regression model
#' @param minRaw Minimum score for the results; can be used for clipping unrealistic outcomes,
#' usually set to the lower bound of the range of values of the test (default: 0)
#' @param maxRaw Maximum score for the results; can be used for clipping unrealistic outcomes
#' usually set to the upper bound of the range of values of the test
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @return the predicted raw score or a data.frame of scores in case, lists of norm scores or age is used
#' @examples
#' # Prediction of single scores
#' normData <- prepareData(elfe)
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
             maxRaw = Inf,
           covariate = NULL) {

    score.matrix <- matrix(nrow = length(norm), ncol = length(age), dimnames = list(norm, age))

    for(no in 1:nrow(score.matrix)){
    # first intercept
    coef <- coefficients
    if(!is.null(covariate)){
      coef <- simplifyCoefficients(coef, covariate)
    }
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
          p <- p * norm[[no]]
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

    score.matrix[no,] <- predict
    }

    if(nrow(score.matrix)==1&&ncol(score.matrix)==1)
      return(score.matrix[1, 1])
    else
      return(score.matrix)


  }

#' Create a norm table based on model for specific age
#'
#' This function generates a norm table for a specific age based on the regression
#' model by assigning raw scores to norm scores. Please specify the
#' range of norm scores, you want to cover. A T value of 25 corresponds to a percentile
#' of .6. As a consequence, specifying a rang of T = 25 to T = 75 would cover 98.4 % of
#' the population. Please be careful when extrapolating vertically (at the lower and
#' upper end of the age specific distribution). Depending on the size of your standardization
#' sample, extreme values with T < 20 or T > 80 might lead to inconsistent results. In case a confidence coefficient
#' (CI) and the reliability is specified, confidence intervalls are computed as well, including a correction
#' for regression to the mean.
#' @param A the age as single value or a vector of age values
#' @param model The regression model
#' @param minNorm The lower bound of the norm score range
#' @param maxNorm The upper bound of the norm score range
#' @param minRaw clipping parameter for the lower bound of raw scores
#' @param maxRaw clipping parameter for the upper bound of raw scores
#' @param step Stepping parameter with lower values indicating higher precision
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @param monotonuous corrects for decreasing norm scores in case of model inconsistencies (default)
#' @param CI confidence coefficent, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1
#' @return either data.frame with norm scores, predicted raw scores and percentiles in case of simple A
#' value or a list #' of norm tables if vector of A values was provided
#' @seealso rawTable
#' @examples
#' normData <- prepareData(elfe)
#' m <- bestModel(data = normData)
#'
#' # create single norm table
#' norms <- normTable(3.5, m, minNorm = 25, maxNorm = 75, step = 0.5)
#'
#' # create list of norm tables
#' norms <- normTable(c(2.5, 3.5, 4.5), m,
#'   minNorm = 25, maxNorm = 75,
#'   step = 1, minRaw = 0, maxRaw = 26
#' )
#' @export
normTable <- function(A,
                      model,
                      minNorm = NULL,
                      maxNorm = NULL,
                      minRaw = NULL,
                      maxRaw = NULL,
                      step = NULL, covariate = NULL,
                      monotonuous = TRUE,
                      CI = .9,
                      reliability = NULL) {

  if(!is.null(covariate)&&is.null(model$covariate)){
    warning("Covariate specified but no covariate available in the model. Setting covariate to NULL.")
    covariate = NULL
  }else if(is.null(covariate)&&!is.null(model$covariate)){
    stop("Covariate specified in the model, but no function parameter available.")
  }

  if (model$useAge&&!is.numeric(A)){
    stop("Please specify age.")
  }

  if (is.null(model)){
    stop("No model specified")
  }

  if(is.null(CI)||is.na(CI)){
    reliability <- NULL
  }else if (CI > .99999 || CI < .00001){
    stop("Confidence coefficient (CI) out of range. Please specify value between 0 and 1.")
  }

  rel <- FALSE
  if(!is.null(reliability)){
    if(reliability > .9999 || reliability < .0001){
    stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
  }else{
      se <- qnorm(1 - ((1 - CI)/2)) * sqrt(reliability * (1 - reliability));
      rel <- TRUE
  }
  }

  if (is.null(minNorm)||is.na(minNorm)){
    minNorm <- model$scaleM - 2 * model$scaleSD
  }

  if (is.null(maxNorm)||is.na(maxNorm)){
    maxNorm <- model$scaleM + 2 * model$scaleSD
  }

  # in case it still fails
  if (is.null(minNorm)||is.null(maxNorm)){
    stop("Please specify minNorm and maxNorm.")
  }

  if(is.null(step)||is.na(step)){
    step <- model$scaleSD / 10
  }

  descend <- model$descend


  if (is.null(minRaw)||is.na(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)||is.na(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  tables <- vector("list", length(A))

  for (x in 1:length(A)) {
    maxn <- maxNorm
    minn <- minNorm
    norm <- vector("list", (maxn - minn) / step + 1)
    raw <- vector("list", (maxn - minn) / step + 1)
    i <- 1
    l <- length(norm)

      while (i <= l) {
        r <- predictRaw(minn, A[[x]], model$coefficients, minRaw = minRaw, maxRaw = maxRaw, covariate = covariate)

        norm[[i]] <- minn
        raw[[i]] <- r

        minn <- minn + step
        i <- i + 1
      }


    normTable <-
      do.call(rbind, Map(data.frame, norm = norm, raw = raw))

    if (!is.na(model$scaleM) && !is.na(model$scaleSD)) {
      normTable$percentile <- pnorm((normTable$norm - model$scaleM) / model$scaleSD) * 100
    }

    if(monotonuous){
      minRaw <- normTable$raw[[1]]

      for(y in 1:length(normTable$raw)){
        if(normTable$raw[[y]] >= minRaw){
          minRaw <- normTable$raw[[y]]
        }else{
          normTable$raw[[y]] <- NA
        }
      }
    }

    if(rel){
      zPredicted <- reliability * (normTable$norm - model$scaleM)/model$scaleSD
      normTable$lowerCI <- (zPredicted - se) * model$scaleSD + model$scaleM
      normTable$upperCI <- (zPredicted + se) * model$scaleSD + model$scaleM
      normTable$lowerCI_PR <- pnorm(zPredicted - se) * 100
      normTable$upperCI_PR <- pnorm(zPredicted + se) * 100

    }

    tables[[x]] <- normTable
  }

  names(tables) <- A

  if (length(A) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
}

#' Create a table with norm scores assigned to raw scores for a specific age based on the regression model
#'
#' This function is comparable to 'normTable', despite it reverses the assignment:
#' A table with raw scores and the according norm scores for a specific age based on the regression
#' model is generated. This way, the inverse function of the regression model is solved numerically with
#' brute force. Please specify the range of raw values, you want to cover. With higher precision
#' and smaller stepping, this function becomes computational intensive. In case a confidence coefficient
#' (CI) and the reliability is specified, confidence intervalls are computed as well, including a correction
#' for regression to the mean.
#' @param A the age, either single value or vector with age values
#' @param model The regression model
#' @param minRaw The lower bound of the raw score range
#' @param maxRaw The upper bound of the raw score range
#' @param minNorm Clipping parameter for the lower bound of norm scores (default 25)
#' @param maxNorm Clipping parameter for the upper bound of norm scores (default 25)
#' @param step Stepping parameter for the raw scores (default 1)
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @param monotonuous corrects for decreasing norm scores in case of model inconsistencies (default)
#' @param CI confidence coefficent, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1
#' @return either data.frame with raw scores and the predicted norm scores in case of simple A value or a list
#' of norm tables if vector of A values was provided
#' @seealso normTable
#' @examples
#' normData <- prepareData(elfe)
#' m <- bestModel(data = normData)
#' # generate a norm table for the raw value range from 0 to 28 for month 7 of grade 3
#' table <- rawTable(3 + 7 / 12, m, minRaw = 0, maxRaw = 28)
#'
#' # generate several raw tables
#' table <- rawTable(c(2.5, 3.5, 4.5), m, minRaw = 0, maxRaw = 28)
#'
#' # additionally compute confidence intervalls
#' table <- rawTable(c(2.5, 3.5, 4.5), m, minRaw = 0, maxRaw = 28, CI = .9, reliability = .94)
#' @export
rawTable <- function(A,
                     model,
                     minRaw = NULL,
                     maxRaw = NULL,
                     minNorm = NULL,
                     maxNorm = NULL,
                     step = 1, covariate = NULL,
                     monotonuous = TRUE,
                     CI = .9,
                     reliability = NULL) {

  if(!is.null(covariate)&&is.null(model$covariate)){
    warning("Covariate specified but no covariate available in the model. Setting covariate to NULL.")
    covariate = NULL
  }else if(is.null(covariate)&&!is.null(model$covariate)){
    stop("Covariate specified in the model, but no function parameter available.")
  }

  if (model$useAge&&!is.numeric(A)){
    stop("Please specify age.")
  }

  if (is.null(step)||is.na(step)){
    step <- 1
  }

  if (is.null(model)){
    stop("No model specified")
  }

  if (is.null(minNorm)||is.na(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)||is.na(maxNorm)) {
    maxNorm <- model$maxL1
  }

  if (is.null(minRaw)||is.na(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)||is.na(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  rel <- FALSE
  if(!is.null(reliability)){
    if(reliability > .9999 || reliability < .0001){
      stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
    }else{
      se <- qnorm(1 - ((1 - CI)/2)) * sqrt(reliability * (1 - reliability));
      rel <- TRUE
    }
  }

  tables <- vector("list", length(A))

  for (x in 1:length(A)) {
    maxn <- maxNorm
    minn <- minNorm
    maxr <- maxRaw
    minr <- minRaw
    norm <- vector("list", (maxr - minr) / step)
    raw <- vector("list", (maxr - minr) / step)
    i <- 1
      while (minr <= maxr) {
        i <- i + 1
        n <-
          predictNorm(minr, A[[x]], model, minNorm, maxNorm, covariate = covariate)
        norm[[i]] <- n
        raw[[i]] <- minr

        minr <- minr + step
      }


    table <-
      do.call(rbind, Map(data.frame, raw = raw, norm = norm))

    if (!is.na(model$scaleM) && !is.na(model$scaleSD)) {
      table$percentile <- pnorm((table$norm - model$scaleM) / model$scaleSD) * 100
    }

    if(model$descend){
      table <- table[order(table$raw, decreasing = TRUE),]
    }
    # checking consistency
    k <- 1
    SUCCESS <- TRUE
    errorText <- ""

    if(monotonuous){
      minScore <- table$norm[[1]]
      minPR <- table$percentile[[1]]

      for(y in 1:length(table$norm)){
        if(table$norm[[y]] >= minScore){
          minScore <- table$norm[[y]]
          minPR <- table$percentile[[y]]
        }else{
          table$norm[[y]] <- minScore
          table$percentile[[y]] <- minPR

          SUCCESS <- FALSE
          errorText <- paste0(errorText, table$raw[y], ",")
        }
      }
    }

    if (!SUCCESS) {
      message(paste0("The raw table generation yielded indications of inconsistent raw score results: ", errorText, ". Please check the model consistency."))
    }

    if(rel){
      zPredicted <- reliability * (table$norm - model$scaleM)/model$scaleSD
      table$lowerCI <- (zPredicted - se) * model$scaleSD + model$scaleM
      table$upperCI <- (zPredicted + se) * model$scaleSD + model$scaleM
      table$lowerCI_PR <- pnorm(zPredicted - se) * 100
      table$upperCI_PR <- pnorm(zPredicted + se) * 100

    }

    tables[[x]] <- table
  }

  names(tables) <- A

  if (length(A) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
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
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @return data.frame with norm scores and the predicted scores based on the
#' derived regression function
#' @seealso plotDerivative, derive
#' @examples
#' normData <- prepareData(elfe)
#' m <- bestModel(data = normData)
#' d <- derivationTable(6, m, step = 0.5)
#' @export
derivationTable <-
  function(A,
             model,
             minNorm = NULL,
             maxNorm = NULL,
             step = 0.1, covariate = NULL) {

    if(!is.null(covariate)&&is.null(model$covariate)){
      warning("Covariate specified but no covariate available in the model. Setting covariate to NULL.")
      covariate = NULL
    }else if(is.null(covariate)&&!is.null(model$covariate)){
      stop("Covariate specified in the model, but no function parameter available.")
    }

    if (is.null(minNorm)) {
      minNorm <- model$minL1
    }

    if (is.null(maxNorm)) {
      maxNorm <- model$maxL1
    }

    norm <- vector("list", 1 + (maxNorm - minNorm) / step)
    raw <- vector("list", 1 + (maxNorm - minNorm) / step)
    i <- 1
    coeff <- derive(model, covariate = covariate)
    while (minNorm <= maxNorm) {
      i <- i + 1
      r <- predictRaw(minNorm, A, coeff, covariate = covariate)

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
#' @param force Try to resolve missing norm scores in case of inconsistent models
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here.
#' @return The predicted norm score for a raw score, either single value or list of results
#' @examples
#' normData <- prepareData(elfe)
#' m <- bestModel(data = normData)
#'
#' # return norm value for raw value 21 for grade 2, month 9
#' # Use 'as.list(normData$raw)' and 'as.list(normData$group)' for raw scores
#' # and age to calculate predicted norm values for original data.
#' specificNormValue <- predictNorm(raw = 21, A = 2.75, model = m, minNorm = 25, maxNorm = 75)
#' @export
predictNorm <-
  function(raw,
             A,
             model,
             minNorm = NULL,
             maxNorm = NULL, force = FALSE, covariate = NULL) {
    if (is.null(minNorm) || is.null(maxNorm)) {
      stop("ERROR: Please specify minimum and maximum norm score")
    }

    if(!is.null(covariate)&&is.null(model$covariate)){
      warning("Covariate specified but no covariate available in the model. Setting covariate to NULL.")
      covariate = NULL
    }else if(is.null(covariate)&&!is.null(model$covariate)){
      stop("Covariate specified in the model, but no function parameter available.")
    }


    # determine single norm value by optimization
    if (length(raw) == 1 && length(A) == 1 && is.numeric(raw) && is.numeric(A)) {
      coef <- model$coefficients
      if(!is.null(covariate)){
        coef <- simplifyCoefficients(coef, covariate)
      }
      startNormScore <- minNorm
      currentRawValue <- predictRaw(norm = minNorm, age = A, coefficients = coef)

      functionToMinimize <- function(norm) {
        currentRawValue <- predictRaw(norm = norm, age = A, coefficients = coef)
        functionValue <- (currentRawValue - raw)^2
      }

      optimum <- optimize(functionToMinimize, lower = minNorm, upper = maxNorm, tol = .Machine$double.eps)
      return(optimum$minimum)
    } else if (is.vector(raw) && is.vector(A)) {
      if (length(raw) != length(A)) {
        stop("'A' and 'raw' need to have the same length.")
      } else if (anyNA(A)) {
        stop(paste0("NAs are present in 'A' vector. Please exclude missing values first."))
      } else if (anyNA(raw)) {
        stop(paste0("NAs are present in 'raw' vector. Please exclude missing values first."))
      }

      if(!is.vector(covariate))
        cov <- rep(covariate, times = length(raw))
      else
        cov <- covariate

      # initialize vectors and starting values
      # of retrieved norm scores to specific cases; needed for later matching
      # create simple identifier based on combining string of raw and age
      hash <- paste0(raw, "_", A)

      # build norm table and use this as a lookup table
      # delete duplicates
      normTable <- data.frame(A = A, raw = raw, hash = hash)
      normTable <- normTable[!duplicated(normTable[,c('hash')]),]

      if(nrow(normTable)>500){
        cat("Retrieving norm scores, please stand by ...\n")
      }
      raw2 <- normTable$raw
      A2 <- normTable$A
      n <- length(raw2)
      values <- rep(NA, n)

      # iterate through cases
      for (i in 1:n) {
        v <- predictNormByRoots(raw2[[i]], A2[[i]], model, minNorm, maxNorm, force = force, covariate = cov[[i]])
        if (length(v) == 0) {
          v <- NA
        }
        values[[i]] <- v
      }

      # project values on original data
      values <- values[match(hash, normTable$hash)]

      # n <- length(raw)
      # values <- rep(NA, n)
      #
      # # iterate through cases
      # for (i in 1:n) {
      #   v <- predictNormByRoots(raw[[i]], A[[i]], model, minNorm, maxNorm)
      #   if (length(v) == 0) {
      #     v <- NA
      #   }
      #   values[[i]] <- v
      # }

      return(values)
    } else {
      stop("Please check raw and A value. Both have to be either single values or vectors of the same length.")
    }
  }
