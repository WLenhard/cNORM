#' Internal function for retrieving regression function coefficients at specific age
#'
#' The function is an inline for searching zeros in the inverse regression
#' function. It collapses the regression function at a specific age and simplifies
#' the coefficients.
#' @param raw The raw value (substracted from the intercept)
#' @param age The age
#' @param model The cNORM regression model
#'
#' @return The coefficients
calcPolyInL <- function(raw, age, model){

  k <- model$k
  coeff <- model$coefficients
  nam <- names(coeff)
  n_coeff <- length(coeff)

  coeff_L <- coeff[grep("L", nam)]

  coeff_without_L <- coeff[setdiff(c(1:length(coeff)), grep("L", names(coeff)))]

  coefficientPolynom <- c()

  # Intercept is written without L and A
  currentCoeff <- coeff_without_L[[1]]

  # Sum of the powers of A times coefficients for terms without L
  if(length(coeff_without_L)>1){
    for(i in c(2:length(coeff_without_L))){

      potA <- as.numeric((strsplit(names(coeff_without_L)[i], ""))[[1]][2])
      currentCoeff <- as.numeric(currentCoeff) + as.numeric(age)^potA*as.numeric(coeff_without_L[[i]])

    }
  }
  coefficientPolynom <- c(coefficientPolynom, currentCoeff)

  currentCoeff <- 0

  # For from 1 to k; for each i the i-th coefficient of the one dimensional polynom will be calculated
  for(i in c(1:k)){

    coeff_L_i <- coeff_L[grep(paste("L", i, sep = ""), names(coeff_L))]
    n_coeff_L_i <- length(coeff_L_i)

    if(n_coeff_L_i > 0 )
    {
      index_coeff_L_i_with_A <- grep("A", names(coeff_L_i))
      coeff_L_i_with_A <- coeff_L_i[index_coeff_L_i_with_A]
      n_coeff_L_i_with_A <- length(coeff_L_i_with_A)

      index_coeff_L_i_without_A <- setdiff(c(1:n_coeff_L_i), index_coeff_L_i_with_A)
      coeff_L_i_without_A <- coeff_L_i[index_coeff_L_i_without_A]

      currentCoeff <- 0

      n_coeff_L_i_without_A <- length(coeff_L_i_without_A)
      if(n_coeff_L_i_without_A>0)
      {
        for(j in c(1:length(coeff_L_i_without_A))){
          currentCoeff <- currentCoeff + as.numeric(coeff_L_i_without_A[[j]])
        }
      }


      if(n_coeff_L_i_with_A > 0){
        for(j in c(1:n_coeff_L_i_with_A)){

          potA <- as.numeric((strsplit(names(coeff_L_i_with_A)[j], ""))[[1]][4])
          currentCoeff <- as.numeric(currentCoeff) + as.numeric(age)^potA*as.numeric(coeff_L_i_with_A[[j]])
        }
      }

      coefficientPolynom <- c(coefficientPolynom, currentCoeff)
    }
    else{
      coefficientPolynom <- c(coefficientPolynom, 0)
    }
  }

  coefficientPolynom[1] <- coefficientPolynom[1] - raw
  return(coefficientPolynom)

}

predictNormByRoots <- function(raw, age, model, minNorm, maxNorm){

  polynomForPrediction <- calcPolyInL(raw = raw,
                                      age = age,
                                      model = model)
  roots <- polyroot(polynomForPrediction)
  output <- Re(roots[abs(Im(roots))<10^(-7)])

  # only one real part as a solution within correct range
  if(length(output)==1 && output>= minNorm && output<=maxNorm)  {
    return(output)
  } else{
    # not exactly one plausible solution, search for alternative on correct side of distribution
    median <- predictRaw(model$scaleM, age, model$coefficients, minRaw=model$minRaw, maxRaw=model$maxRaw)
    if(raw>median){
      output <- output[output > model$scaleM && output <= maxNorm]
    }else if(raw<median){
      output <- output[output < model$scaleM && output >= minNorm]
    }else{
      return(model$scaleM)
    }

    if(length(output)==1){
      return(output)
    } else if(length(output)>1){
      # fetch the solution closest to median
      warning(paste0("Multiple roots found for ", raw, " at age ", age, "; returning most plausible normscore."))
      return(output[which.min((output-model$scaleM)^2)])
    }else{
      # nothing worked, apply numerical searching strategy
      startNormScore <- minNorm
      currentRawValue <- predictRaw(norm = minNorm, age = age, coefficients = model$coefficients)

      functionToMinimize <- function(norm){
        currentRawValue <- predictRaw(norm = norm, age = age, coefficients = model$coefficients)
        functionValue <- (currentRawValue - raw)^2
      }

      optimum <- optimize(functionToMinimize, lower = minNorm, upper = maxNorm, tol = .Machine$double.eps)

      if(optimum$minimum < minNorm || optimum$minimum > maxNorm){
        # everything failed, return NA
        return(NA)
      }

      return(optimum$minimum)
    }
  }
}