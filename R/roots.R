#' Internal function for retrieving regression function coefficients at specific age
#'
#' The function is an inline for searching zeros in the inverse regression
#' function. It collapses the regression function at a specific age and simplifies
#' the coefficients.
#' @param raw The raw value (subtracted from the intercept)
#' @param age The age
#' @param model The cNORM regression model
#'
#' @return The coefficients
calcPolyInL <- function(raw, age, model) {
  k <- model$k
  coeff <- model$coefficients
  return(calcPolyInLBase(raw, age, model, coeff, k))
}

#' Internal function for retrieving regression function coefficients at specific age
#'
#' The function is an inline for searching zeros in the inverse regression
#' function. It collapses the regression function at a specific age and simplifies
#' the coefficients.
#' @param raw The raw value (subtracted from the intercept)
#' @param age The age
#' @param model The cNORM regression model
#'
#' @return The coefficients
calcPolyInLBase <- function(raw, age, coeff, k) {
  nam <- names(coeff)

  coeff_L <- coeff[grep("L", nam)]

  coeff_without_L <- coeff[setdiff(c(1:length(coeff)), grep("L", names(coeff)))]

  coefficientPolynom <- c()

  # Intercept is written without L and A
  currentCoeff <- coeff_without_L[[1]]

  # Sum of the powers of A times coefficients for terms without L
  if (length(coeff_without_L) > 1) {
    for (i in c(2:length(coeff_without_L))) {
      potA <- as.numeric((strsplit(names(coeff_without_L)[i], ""))[[1]][2])
      currentCoeff <- as.numeric(currentCoeff) + as.numeric(age)^potA * as.numeric(coeff_without_L[[i]])
    }
  }
  coefficientPolynom <- c(coefficientPolynom, currentCoeff)

  currentCoeff <- 0

  # For from 1 to k; for each i the i-th coefficient of the one dimensional polynomial will be calculated
  for (i in c(1:k)) {
    coeff_L_i <- coeff_L[grep(paste("L", i, sep = ""), names(coeff_L))]
    n_coeff_L_i <- length(coeff_L_i)

    if (n_coeff_L_i > 0) {
      index_coeff_L_i_with_A <- grep("A", names(coeff_L_i))
      coeff_L_i_with_A <- coeff_L_i[index_coeff_L_i_with_A]
      n_coeff_L_i_with_A <- length(coeff_L_i_with_A)

      index_coeff_L_i_without_A <- setdiff(c(1:n_coeff_L_i), index_coeff_L_i_with_A)
      coeff_L_i_without_A <- coeff_L_i[index_coeff_L_i_without_A]

      currentCoeff <- 0

      n_coeff_L_i_without_A <- length(coeff_L_i_without_A)
      if (n_coeff_L_i_without_A > 0) {
        for (j in c(1:length(coeff_L_i_without_A))) {
          currentCoeff <- currentCoeff + as.numeric(coeff_L_i_without_A[[j]])
        }
      }


      if (n_coeff_L_i_with_A > 0) {
        for (j in c(1:n_coeff_L_i_with_A)) {
          potA <- as.numeric((strsplit(names(coeff_L_i_with_A)[j], ""))[[1]][4])
          currentCoeff <- as.numeric(currentCoeff) + as.numeric(age)^potA * as.numeric(coeff_L_i_with_A[[j]])
        }
      }

      coefficientPolynom <- c(coefficientPolynom, currentCoeff)
    }
    else {
      coefficientPolynom <- c(coefficientPolynom, 0)
    }
  }

  coefficientPolynom[1] <- coefficientPolynom[1] - raw
  return(coefficientPolynom)
}

predictNormByRoots <- function(raw, age, model, minNorm, maxNorm, polynom = NULL, force = FALSE, covariate = NULL) {

  if(!is.null(covariate)){
    if(is.null(model$coefficients)){
      stop("Covariate specified, but model does not include covariate")
    }
    model$coefficients <- simplifyCoefficients(model$coefficients, covariate)
  }

  if (is.null(polynom)) {
    polynomForPrediction <- calcPolyInL(
      raw = raw,
      age = age,
      model = model
    )
  } else {
    polynomForPrediction <- polynom
    polynomForPrediction[1] <- polynomForPrediction[1] - raw
  }

  roots <- polyroot(polynomForPrediction)
  output <- Re(roots[abs(Im(roots)) < 10^(-7)])

  # only one real part as a solution within correct range
  if (length(output) == 1 && output >= minNorm && output <= maxNorm) {
    return(output)
  } else {
    # not exactly one plausible solution, search for alternative on correct side of distribution
    median <- predictRaw(model$scaleM, age, model$coefficients, minRaw = model$minRaw, maxRaw = model$maxRaw)
    if (raw > median) {
      output <- output[output > model$scaleM && output <= maxNorm]
    } else if (raw < median) {
      output <- output[output < model$scaleM && output >= minNorm]
    } else {
      return(model$scaleM)
    }

    if (length(output) == 1) {
      return(output)
    } else if (length(output) > 1) {
      # fetch the solution closest to median
      # warning(paste0("Multiple roots found for ", raw, " at age ", age, "; returning most plausible norm score."))
      return(output[which.min((output - model$scaleM)^2)])
    } else {
      # nothing worked, apply numerical searching strategy
      startNormScore <- minNorm
      currentRawValue <- predictRaw(norm = minNorm, age = age, coefficients = model$coefficients)

      functionToMinimize <- function(norm) {
        currentRawValue <- predictRaw(norm = norm, age = age, coefficients = model$coefficients)
        functionValue <- (currentRawValue - raw)^2
      }

      optimum <- optimize(functionToMinimize, lower = minNorm, upper = maxNorm, tol = .Machine$double.eps)

      if(optimum$minimum >= minNorm && optimum$minimum <= maxNorm){
        return(optimum$minimum)
      }else if (!force&&(optimum$minimum < minNorm || optimum$minimum > maxNorm)) {
        # everything failed, return NA
        warning(paste0("No plausible norm score available for ", raw, " at age ", age, "; returning NA"))

        return(NA)
      }else if(force && optimum$minimum < minNorm){
        warning(paste0("No plausible norm score available for ", raw, " at age ", age, "; returning lower boundary of the norms."))
        return(minNorm)
      }else if(force && optimum$minimum > maxNorm){
        warning(paste0("No plausible norm score available for ", raw, " at age ", age, "; returning upper boundary of the norms."))
        return(maxNorm)
      }
    }
  }
}

simplifyCoefficients <- function(coefficients = coefficients, covariate = covariate){

  #simplify coefficients in case of covariates
  names <- names(coefficients)
  indexCOV <- NA
  indexL1COV <- NA
  indexA1COV <- NA
  indexL1A1COV <- NA

  if("COV" %in% names){
    indexCOV <- which(names == "COV")
    coefficients[[1]] <- coefficients[[1]] + (coefficients[[indexCOV]]*covariate)
  }

  if("L1COV" %in% names){
    indexL1COV <- which(names == "L1COV")
    tmp <- coefficients[[indexL1COV]]*covariate

    if(!("L1" %in% names)){
      names <- append(names, "L1")
      coefficients <- append(coefficients, tmp)
    }else{
      coefficients[[which(names == "L1")]] <- coefficients[[which(names == "L1")]] + tmp
    }
  }


  if("A1COV" %in% names){
    indexA1COV <- which(names == "A1COV")
    tmp <- coefficients[[indexA1COV]]*covariate

    if(!("A1" %in% names)){
      names <- append(names, "A1")
      coefficients <- append(coefficients, tmp)
    }else{
      coefficients[[which(names == "A1")]] <- coefficients[[which(names == "A1")]] + tmp
    }
  }

  if("L1A1COV" %in% names){
    indexL1A1COV <- which(names == "L1A1COV")
    tmp <- coefficients[[indexL1A1COV]]*covariate

    if(!("L1A1" %in% names)){
      names <- append(names, "L1A1")
      coefficients <- append(coefficients, tmp)
    }else{
      coefficients[[which(names == "L1A1")]] <- coefficients[[which(names == "L1A1")]] + tmp
    }
  }

  names(coefficients) <- names
  coefficients <- na.omit(coefficients[-na.omit(c(indexCOV, indexL1COV, indexA1COV, indexL1A1COV))])

  return(coefficients)
}
