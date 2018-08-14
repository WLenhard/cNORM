#' Generate box cox power function for regression model at specific age
#'
#' Applies a curve fitting for the regression model with the Box-Cox power transformation
#' at a specific age, simulates a data set and applies the transformation. It iteratively
#' determines the power transformation lambda parameter with a precision up to 10^E-5 with a lambda value
#' of 1 indicating normal distribution, values between 0 and 1 representing negative skew and
#' values above 1 positive skewness of the distribution. The function is an optional step
#' following the non-parametric modelling in order to conduct a parametric fitting of the percentiles.
#' @param model The regression model
#' @param age The specific age
#' @param n Number of simulated observations, used to span a percentile range from
#' .5/n to (n-.5)/n with equally distanced percentiles
#' @param m Scale mean of norm scale (default 50)
#' @param sd Scale sd of norm scale (default 10)
#'
#' @return data.frame with percentiles, normvalues, fitted raw values of the regression
#' model and the fitted values of the box cox curve fitting (indicated by variable names 'BC')
#' @references
#' Cole, T. J., & Green, P. J. (1992). Smoothing reference centile curves: the LMS method and penalized
#' likelihood. Statistics in medicine, 11(10), 1305-1319.
#' @references
#' Box, G. E., & Cox, D. R. (1964). An analysis of transformations. Journal of the Royal Statistical
#' Society. Series B (Methodological), 211-252.
#' @export
#'
#' @examples
#' # model sample data set
#' model <- bestModel(prepareData())
#'
#' # fitting values of regression model box cox power function at specific age
#' bcFitted <- boxcox(model, 3)
boxcox <- function(model, age, n = 250, m = 50, sd = 10) {

  # prepare simulated data and predicted values
  data <- seq(from = 0.5 / n, to = (n - 0.5) / n, length.out = n)
  perc <- data
  data <- stats::qnorm(data, mean = m, sd = sd)
  y <- rep(NA, n)
  i <- 1
  while (i <= n) {
    y[[i]] <- predictRaw(data[[i]], age, model$coefficients, min = 0)
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

  # print values
  message(paste0("BoxCox function parameters for age ", age, ":"))
  message(paste0("m: ", mlambda))
  message(paste0("sd: ", sdlambda))
  message(paste0("lambda: ", lambda))

  x <- ((x - mlambda) / sdlambda) * sd + m
  density <- stats::dnorm(x, mean = m, sd = sd)
  percentile <- stats::pnorm(x, mean = m, sd = sd)

  table <-
    do.call(rbind, Map(data.frame,
      percentile = perc, normvalue = data,
      fittedRaw = y,
      normValueBC = x,
      densityBC = density,
      percentileBC = percentile
    ))
  return(table)
}
