#' Generate box cox power function for regression model at specific age
#'
#' Applies a curve fitting for the regression model with the Box-Cox power transformation via the
#' LMS method of Cole and Green (1992) at a specific age. Therefore, it simulates a data set and
#' applies the transformation. It iteratively determines the power transformation lambda
#' parameter with a precision up to 10^E-5 with a lambda value of 1 indicating normal
#' distribution, values between 0 and 1 representing negative skew and values above 1 positive
#' skewness of the distribution. The function is an optional step following the non-parametric
#' modeling in order to conduct a parametric fitting of the percentiles.
#' @param model The regression model
#' @param age The specific age
#' @param n Number of simulated observations, used to span a percentile range from
#' .5/n to (n-.5)/n with equally distanced percentiles
#' @param m Scale mean of norm scale (default 50)
#' @param sd Scale sd of norm scale (default 10)
#'
#' @return a list including the data.frame with percentiles, norm scores, fitted raw scores of the regression
#' model and the fitted scores of the box cox curve fitting (indicated by variable names 'BC'), as well as the
#' parameters for the box cox function (mean, sd, lambda) for the specified the age:
#' \item{median}{The median of the raw value distribution, estimated by the regression model}
#' \item{meanBC}{The mean of the box cox function}
#' \item{sdBC}{The standard deviation of the box cox function}
#' \item{lambdaBC}{The skewness parameter of the box cox function}
#' \item{age}{The age for which the power function was modeled}
#' \item{data}{The data frame including the generated percentiles, the according norm scores,
#' the fitted raw scores according to the regression model, the retrieved norm scores by the box cox transformation,
#' the according density and percentile}
#' @references
#' Cole, T. J., & Green, P. J. (1992). Smoothing reference percentile curves: the LMS method and penalized
#' likelihood. Statistics in medicine, 11(10), 1305-1319.
#' @references
#' Box, G. E., & Cox, D. R. (1964). An analysis of transformations. Journal of the Royal Statistical
#' Society. Series B (Methodological), 211-252.
#' @export
#'
#' @examples
#' # model sample data set
#' model <- bestModel(prepareData(elfe))
#'
#' # fitting scores of regression model box cox power function at specific age and retrieving
#' # the parameters for the box cox power function
#' bcParameters <- boxcox(model, 3)
#' @seealso predictNormBC, predictRawBC
boxcox <- function(model, age, n = 250, m = 50, sd = 10) {

  # prepare simulated data and predicted scores
  data <- seq(from = 0.5 / n, to = (n - 0.5) / n, length.out = n)
  perc <- data
  data <- qnorm(data, mean = m, sd = sd)
  y <- rep(NA, n)
  i <- 1
  while (i <= n) {
    y[[i]] <- predictRaw(data[[i]], age, model$coefficients, minRaw = 0)
    i <- i + 1
  }

  md <- predictRaw(m, age, model$coefficients)

  # determine optimal lambda
  # setting start values for iteration
  sdlambda <- Inf
  mlambda <- 0
  lambda <- 0
  i <- .001
  pow <- -1
  step <- 1
  end <- 10

  # iteratively estimate parameters with increasing precision up to stepping 10^E-05
  while (pow > -5) {
    sdlambda <- Inf

    if (pow < -1) {
      step <- 10^pow
      end <- lambda + step
      i <- lambda - step
      if (i <= 0) {
        i <- step / 2
      }
    }

    while (i <= end) {
      x <- ((y / md)^i - 1) / i
      sdX <- sd(x, na.rm = TRUE)
      if (sdX < sdlambda) {
        sdlambda <- sdX
        mlambda <- mean(x, na.rm = TRUE)
        lambda <- i
        i <- i + 0.001
      } else {
        # optimum reach, break
        i <- Inf
      }
    }

    pow <- pow - 1
  }

  # print results
  message(paste0("BoxCox function parameters for age ", age, ":"))
  message(paste0("m: ", mlambda))
  message(paste0("sd: ", sdlambda))
  message(paste0("lambda: ", lambda))

  x <- ((x - mlambda) / sdlambda) * sd + m
  percentile <- pnorm(x, mean = m, sd = sd)
  density <- dnorm(x, mean = m, sd = sd)

  table <-
    do.call(rbind, Map(data.frame,
      percentile = perc, normvalue = data,
      fittedRaw = y,
      normValueBC = x,
      densityBC = density,
      percentileBC = percentile
    ))
  return(list(n = n, age = age, median = md, mean = m, sd = sd, meanBC = mlambda, sdBC = sdlambda, lambdaBC = lambda, data = table, regressionCoefficients = model$coefficients))
}


#' Calculate the raw score for a given percentile based on the parametric box cox distribution
#'
#' In addition of the numeric solution to the regression function on 'predictRaw', this function
#' can be used retrieving the raw values at a specific age via the parametric box cox power
#' transformation. Please provide the box cox parameters retrieved via the 'boxcox'-function and
#' a percentile.
#' @param boxcoxParameters The parameters of the box cox power function, calculated via 'boxcox'
#' @param percentile The percentile (ranging from >0 to <1)
#'
#' @return the predicted raw value
#' @references
#' Cole, T. J., & Green, P. J. (1992). Smoothing reference percentile curves: the LMS method and penalized
#' likelihood. Statistics in medicine, 11(10), 1305-1319.
#' @references
#' Box, G. E., & Cox, D. R. (1964). An analysis of transformations. Journal of the Royal Statistical
#' Society. Series B (Methodological), 211-252.
#' @export
#' @seealso boxcox, predictRaw
#'
#' @examples
#' \dontrun{
#' # model sample data set
#' model <- bestModel(prepareData(elfe))
#'
#' # fitting scores of regression model box cox power function at specific age and retrieving
#' # the parameters for the box cox power function
#' bcParameters <- boxcox(model, 3)
#'
#' # define percentile and according t value
#' percentile <- .4
#' tValue <- qnorm(percentile)*10 + 50
#'
#' # predict raw value based on the regression model and via box cox
#' predictRawBC(bcParameters, percentile)
#' predictRaw(tValue, 3, model$coefficients)
#' }
predictRawBC <- function(boxcoxParameters, percentile) {
  if (percentile <= 0 || percentile >= 1) {
    stop("Percentile out of range. Use values between 0 and 1.")
  }
  # convert percentile to standardized z value
  z <- qnorm(percentile)

  # compute box cox power function x for z
  x <- z * boxcoxParameters$sdBC + boxcoxParameters$meanBC

  # compute raw value based on BC function
  raw <- ((x * boxcoxParameters$lambdaBC + 1)^(1 / boxcoxParameters$lambdaBC)) * boxcoxParameters$median

  return(raw)
}


#' Calculate the norm value for a given raw value based on the parametric box cox distribution
#'
#' In addition to the numeric solution of the inversion of the regression function applied in
#' 'predictNorm', this function
#' can be used retrieving the norm scores at a specific age via the parametric box cox power
#' transformation. Please provide the box cox parameters retrieved via the 'boxcox'-function and
#' a raw value.
#' @param boxcoxParameters The parameters of the box cox power function, calculated via 'boxcox'
#' @param raw The raw value (>0)
#' @param scale type of norm scale, either T, IQ, z or percentile (= no
#' transformation; default); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @return the predicted raw value
#' @references
#' Cole, T. J., & Green, P. J. (1992). Smoothing reference centile curves: the LMS method and penalized
#' likelihood. Statistics in medicine, 11(10), 1305-1319.
#' @references
#' Box, G. E., & Cox, D. R. (1964). An analysis of transformations. Journal of the Royal Statistical
#' Society. Series B (Methodological), 211-252.
#' @export
#' @seealso boxcox, predictNorm, predictRawBC
#'
#' @examples
#' # model sample data set
#' model <- bestModel(prepareData(elfe))
#'
#' # fitting scores of regression model box cox power function at specific age and retrieving
#' # the parameters for the box cox power function
#' bcParameters <- boxcox(model, 3)
#'
#' # predict norm value for raw value 15 at age 3 based on the regression model and via box cox
#' predictNormBC(bcParameters, 15, scale="T")
#' predictNorm(15, 3, model, minNorm=25, maxNorm=75)
predictNormBC <- function(boxcoxParameters, raw, scale = "percentile") {
  if (raw < 0) {
    stop("Box Cox cannot handle negative raw scores")
  }

  # compute box cox power function x for raw value
  x <- (((raw / boxcoxParameters$median)^boxcoxParameters$lambdaBC) - 1) / boxcoxParameters$lambdaBC

  # compute z for x variable
  z <- (x - boxcoxParameters$meanBC) / boxcoxParameters$sdBC

  if ((typeof(scale) == "double" && length(scale) == 2)) {
    return(z * scale[2] + scale[1])
  } else if (scale == "z") {
    return(z)
  } else if (scale == "T") {
    return(z * 10 + 50)
  } else if (scale == "IQ") {
    return(z * 15 + 100)
  } else {
    return(pnorm(z))
  }
  return(raw)
}

#' Plot regression model versus box cox for a specific age
#'
#' This plot can be used to compare, how well the regression data can be modeled via a
#' Box Cox power transformation.
#'
#' @param regressionModel The regression model from the 'bestModel' function
#' @param boxcoxParameters The parameters from the box cox power transformation
#' @param minRaw The lower bound of raw scores; must not fall below 0 due to restrictions of the
#' box cox function
#' @param maxRaw The upper bound of raw scores
#' @param type Type of plot; 0 = Show percentiles as function of raw scores, 1 = Show raw scores
#' as function of norm scores, 2 = Density plot
#' @return data frame with fitted box cox and regression scores
#' @export
#' @seealso boxcox
#' @examples
#'
#' # Calculate model based on PPVT4 data
#' data <- prepareData(ppvt)
#' model <- bestModel(data)
#'
#' # compute power function for a specific age, e. g. 9.2
#' bc <- boxcox(model, 9.2)
#'
#' # plot results as a function of norm scores
#' plotBoxCox(model, bc, minRaw=0, maxRaw=228, type=1)
#'
#' # plot density
#' plotBoxCox(model, bc, minRaw=0, maxRaw=228, type=2)
plotBoxCox <- function(regressionModel, boxcoxParameters, minRaw = NULL, maxRaw = NULL, type = 0) {
  if (is.null(minRaw)) {
    minRaw <- regressionModel$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- regressionModel$maxRaw
  }

  if (minRaw < 0) {
    stop("Negative values are not allowed in Box Cox transformations")
  }

  percentiles <- seq(from = 0.5 / boxcoxParameters$n, to = (boxcoxParameters$n - 0.5) / boxcoxParameters$n, length.out = boxcoxParameters$n)
  scale <- qnorm(percentiles, mean = boxcoxParameters$mean, sd = boxcoxParameters$sd)
  density <- dnorm(scale, mean = boxcoxParameters$mean, sd = boxcoxParameters$sd)

  rawBC <- rep(NA, length.out = length(percentiles))
  rawRegression <- rep(NA, length.out = length(percentiles))

  i <- 1
  while (i <= length(percentiles)) {
    rawRegression[[i]] <- predictRaw(scale[[i]], boxcoxParameters$age, regressionModel$coefficients, minRaw = minRaw, maxRaw = maxRaw)
    rawBC[[i]] <- predictRawBC(boxcoxParameters, percentiles[[i]])
    i <- i + 1
  }

  table <-
    do.call(rbind, Map(data.frame,
      percentiles = percentiles, scale = scale, density = density, rawRegression = rawRegression,
      rawBoxCox = rawBC
    ))

  COL <- rainbow(2)
  panelfun <- function(..., type, group.number) {
    lattice::panel.lines(...)
  }

  if (type == 0) {
    p <- lattice::xyplot(percentiles ~ rawRegression + rawBC, table,
      panel = function(...)
        lattice::panel.superpose(..., panel.groups = panelfun),
      main = "Regression Model vs. Box Cox Transformation",
      ylab = "Percentile", xlab = "Raw Score",
      col = COL, lwd = 2, grid = TRUE,
      key = list(
        corner = c(0.99, 0.1),
        lines = list(col = COL, lwd = 2),
        text = list(c("Regression", "Box Cox"))
      )
    )
  } else if (type == 1) {
    p <- lattice::xyplot(rawRegression + rawBC ~ scale, table,
      panel = function(...)
        lattice::panel.superpose(..., panel.groups = panelfun),
      main = "Regression Model vs. Box Cox Transformation",
      ylab = "Raw Score", xlab = "Standard Score",
      col = COL, lwd = 2, grid = TRUE,
      key = list(
        corner = c(0.99, 0.1),
        lines = list(col = COL, lwd = 2),
        text = list(c("Regression", "Box Cox"))
      )
    )
  } else {
    p <- lattice::xyplot(density ~ rawRegression + rawBC, table,
      panel = function(...)
        lattice::panel.superpose(..., panel.groups = panelfun),
      main = "Regression Model vs. Box Cox Transformation",
      ylab = "Density", xlab = "Raw value",
      col = COL, lwd = 2, grid = TRUE,
      key = list(
        corner = c(0.99, 0.1),
        lines = list(col = COL, lwd = 2),
        text = list(c("Regression", "Box Cox"))
      )
    )
  }

  print(p)
  return(table)
}
