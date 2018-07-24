#' Computes the curve for a specific T value
#'
#' As with this continuous norming regression approach, raw values are modeled as a function of age and norm value
#' (location), getNormCurve is a straight forward approach to show the raw value development over
#' age, while keeping the norm value constant. This way, e. g. academic performance or intelligence development
#' of a specific ability is shown.
#' @param normValue The norm value, e. g. T value
#' @param model The model from the regression modeling
#' @param minAge Age to start from
#' @param maxAge Age to stop at
#' @param step Stepping parameter for the precision when retrieving of the values, lower
#' values indicate higher precision (default 0.1).
#' @param minRaw lower bound of the range of raw values (default = 0)
#' @param maxRaw upper bound of raw values
#' @return data.frame of the values raw, age and norm
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' getNormCurve(35, m)
#' @export
getNormCurve <- function(normValue, model, minAge = 2, maxAge = 5, step = 0.1,
                         minRaw = 0, maxRaw = 1000) {
    raw <- base::vector("list", (maxAge - minAge)/step)
    age <- base::vector("list", (maxAge - minAge)/step)
    normList <- base::vector("list", (maxAge - minAge)/step)

    i <- 1
    while (minAge <= maxAge) {
        r <- cNORM::predictRaw(normValue, minAge, model$coefficients, minRaw, maxRaw)

        raw[[i]] <- r
        age[[i]] <- minAge
        normList[[i]] <- base::paste(normValue, "T")
        minAge <- minAge + step
        i <- i + 1

    }
    curve <- do.call(base::rbind, base::Map(data.frame, norm = normList, age = age, raw = raw))
    return(curve)
}

#' Predict single raw value
#'
#' Most elementary function to predict raw value based on Location (L, T value),
#' Age (grouping variable) and the coefficients from a regression model
#' @param normValue The norm value, e. g. T value
#' @param age The age value
#' @param coefficients The coefficients from the regression model
#' @param min Minimum value for the results; can be used for clipping unrealistic outcomes,
#' usually set to the lower bound of the range of values of the test (default: 0)
#' @param max Maximum value for the results; can be used for clipping unrealistic outcomes
#' usually set to the upper bound of the range of values of the test
#' @return the predicted raw value
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' predictRaw(35, 3.5, m$coefficients)
#' @export
predictRaw <- function(normValue, age, coefficients, min = 0, max = 1000) {
    # first intercept
    coef <- coefficients
    predict <- 0

    i <- 1
    while (i <= base::length(coef)) {
        nam <- base::strsplit(names(coef[i]), "")
        p <- coef[[i]][1]

        # first variable, either L or A
        if(base::length(nam[[1]])<2|base::length(nam[[1]])>4){
          # nothing to do
        } else if (nam[[1]][1] == "L") {
            j <- 1
            while (j <= nam[[1]][2]) {
                p <- p * normValue
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
        if (base::length(nam[[1]]) == 4) {
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
    if (predict < min) {
        predict <- min
    } else if (predict > max) {
        predict <- max
    }

    return(predict)
}

#' Create a norm table based on model for specific age
#'
#' This function generates a norm table for a specific age based on the regression
#' model. Please specify the range of norm values, you want to cover. A T value of
#' 25 corresponds to a percentile of .6. As a consequence, specifying a rang of
#' T = 25 to T = 75 would cover 98.4 % of the population. Please be careful when
#' extrapolating horizontally. Extreme values with T < 20 or T > 80 might lead to
#' inconsistent results.
#' @param A the age
#' @param model The regression model
#' @param min The lower bound of the norm value range
#' @param max The upper bound of the norm value range
#' @param step Stepping parameter with lower values indicating higher precision
#' @return data.frame with norm values and the predicted raw value
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' norms <- normTable(3.5, m, step=0.5)
#' @export
normTable <- function(A, model, min = 25, max = 75, step = 0.1) {
    norm <- base::vector("list", (max - min)/step)
    raw <- base::vector("list", (max - min)/step)
    i <- 1
    while (min <= max) {
        i <- i + 1
        r <- cNORM::predictRaw(min, A, model$coefficients)

        norm[[i]] <- min
        raw[[i]] <- r

        min <- min + step
    }
    normTable <- do.call(base::rbind, base::Map(data.frame, norm = norm, raw = raw))
    return(normTable)
}

#' Create a table based on first derivation of the regression model for specific age
#'
#' In order to check model assumptions, a table of the first derivation of the model
#' coefficients is created.
#' TODO additional information necessary
#' @param A the age
#' @param model The regression model
#' @param min The lower bound of the norm value range
#' @param max The upper bound of the norm value range
#' @param step Stepping parameter with lower values indicating higher precision
#' @return data.frame with norm values and the predicted value based on the
#' derived regression function
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' d <- derivationTable(6, m, step=0.5)
#' @export
derivationTable <- function(A, model, min = 25, max = 75, step = 0.1) {
  norm <- base::vector("list", 1 + (max - min)/step)
  raw <- base::vector("list", 1 + (max - min)/step)
  i <- 1
  coeff <- cNORM::derive(model)
  while (min <= max) {
    i <- i + 1
    r <- cNORM::predictRaw(min, A, coeff, min = -1000, max = 1000)

    norm[[i]] <- min
    raw[[i]] <- r

    min <- min + step
  }
  normTable <- do.call(base::rbind, base::Map(data.frame, norm = norm, raw = raw))
  return(normTable)
}

#' Retrieve norm value for raw score at a specific age
#'
#' In real test scenarios, usually the results are available as raw values, for
#' which norm values have to be looked up. This function conducts this reverse
#' transformation via a numerical solution: A precise norm table is generated and
#' the closest fitting norm value for a raw value is returned.
#' @param raw The raw value
#' @param A the age
#' @param model The regression model
#' @param min The lower bound of the norm value range
#' @param max The upper bound of the norm value range
#' @param precision The precision for the norm value generation with lower values
#' indicating a higher precision. In case of T values, precision = 0.1 is sufficient.
#' @return The predicted norm value for a raw value
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data=normData)
#' specificNormValue <- predictNormValue(21, 2.8, m)
#' @export
predictNormValue <- function(raw, A, model, min = 25, max = 75, precision = 0.1) {
    norms <- cNORM::normTable(A, model, min = min, max = max, step = precision)
    index <- which.min(abs(norms$raw - raw))
    return(norms$norm[index])
}
