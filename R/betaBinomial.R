#' Calculate the negative log-likelihood for a heteroscedastic regression model
#'
#' This function computes the negative log-likelihood for a heteroscedastic regression model
#' where both the mean and standard deviation are modeled as functions of predictors.
#'
#' @param params A numeric vector containing all model parameters. The first n_beta elements
#'               are coefficients for the mean model, and the remaining elements are
#'               coefficients for the log-standard deviation model.
#' @param X A matrix of predictors for the mean model.
#' @param Z A matrix of predictors for the log-standard deviation model.
#' @param y A numeric vector of response values.
#'
#' @return The negative log-likelihood of the model.
#' @keywords internal
log_likelihood <- function(params, X, Z, y) {
  n_beta <- ncol(X)
  beta <- params[1:n_beta]
  gamma <- params[(n_beta+1):length(params)]

  mu <- X %*% beta
  log_sigma <- Z %*% gamma
  sigma <- exp(log_sigma)

  ll <- sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  return(-ll)  # Return negative log-likelihood for minimization
}


#' Fit a heteroscedastic regression model
#'
#' This function fits a heteroscedastic regression model where both the mean and
#' standard deviation of the response variable are modeled as polynomial functions
#' of the predictor variable.
#'
#' @param time A numeric vector of predictor values (e.g., age or time).
#' @param score A numeric vector of response values.
#' @param mu Integer specifying the degree of the polynomial for the mean model. Default is 2.
#' @param sigma Integer specifying the degree of the polynomial for the standard deviation model. Default is 1.
#' @param control A list of control parameters to be passed to the `optim` function.
#'   If NULL, default values are used.
#' @param n Number of items in the test, resp. maximum score to be achieved
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#'
#' @return A list of class "hetero_model" containing:
#'   \item{beta_est}{Estimated coefficients for the mean model}
#'   \item{gamma_est}{Estimated coefficients for the log-standard deviation model}
#'   \item{se}{Standard errors of the estimated coefficients}
#'   \item{mu}{Degree of the polynomial for the mean model}
#'   \item{sigma}{Degree of the polynomial for the standard deviation model}
#'   \item{result}{Full result from the optimization procedure}
#'
#' @details
#' The function standardizes the input variables, fits polynomial models for both
#' the mean and standard deviation, and uses maximum likelihood estimation to
#' find the optimal parameters. The optimization is performed using the BFGS method.
#'
#' @examples
#' # Fit a heteroscedastic regression model to the PPVT data
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#' summary(model)
#'
#'
#' @export
cnorm.betabinomial <- function(time, score, mu = 3, sigma = 3, n = NULL, control = NULL, scale = "T", descend = FALSE) {
  # Standardize inputs
  time_std <- standardize(time)
  score_std <- standardize(score)

  # Set up 'data' object containing both variables
  data <- data.frame(time = time_std, score = score_std)

  # Prepare the data matrices for mu and sigma, including intercept
  X <- cbind(1, poly(data$time, degree = mu, raw = TRUE))
  Z <- cbind(1, poly(data$time, degree = sigma, raw = TRUE))
  y <- data$score

  # Initial parameters: use some sensible starting values
  initial_params <- c(mean(y), rep(0, mu), log(sd(y)), rep(0, sigma))

  # Optimize to find parameter estimates. If control is NULL, set default
  if(is.null(control))
    control = list(reltol = 1e-8, maxit = 1000)

  result <- optim(initial_params, log_likelihood, X = X, Z = Z, y = y,
                  method = "BFGS", hessian = TRUE,
                  control = control)

  # Extract results and calculate standard errors
  beta_est <- result$par[1:(mu + 1)]
  gamma_est <- result$par[(mu + 2):length(result$par)]
  se <- sqrt(diag(solve(result$hessian)))

  # Store original mean and sd for unstandardizing later
  # add attributes for usage in other functions
  scaleM <- NA
  scaleSD <- NA

  # descriptives
  if ((typeof(scale) == "double" && length(scale) == 2)) {
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (scale == "IQ") {
    scaleM <- 100
    scaleSD <- 15
  } else if (scale == "z") {
    scaleM <- 0
    scaleSD <- 1
  } else if (scale == "T") {
    scaleM <- 50
    scaleSD <- 10
  }

  if(is.null(n))
    n <- max(score)

  attr(result, "time_mean") <- mean(time)
  attr(result, "time_sd") <- sd(time)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD
  attr(result, "descend") <- descend
  attr(result, "descend") <- descend

  model <- list(beta_est = beta_est, gamma_est = gamma_est, se = se,
                mu = mu, sigma = sigma,
                result = result)
  class(model) <- "cnormBetaBinomial"
  return(model)
}

#' Predict mean and standard deviation for a heteroscedastic regression model
#'
#' This function generates predictions from a fitted heteroscedastic regression model
#' for new time points.
#'
#' @param model An object of class "cnorm_betabinomial", typically the result of a call to \code{\link{cnorm.betabinomial}}.
#' @param times A numeric vector of time points at which to make predictions.
#'
#' @return A data frame with columns:
#'   \item{time}{The input time points}
#'   \item{mu}{Predicted mean values}
#'   \item{sigma}{Predicted standard deviation values}
#'
#' @details
#' This function takes a fitted heteroscedastic regression model and generates predictions
#' for new time points. It applies the same standardization used in model fitting,
#' generates predictions on the standardized scale, and then transforms these back
#' to the original scale.
#'
#' @keywords internal
predict.cnormBetaBinomial <- function(model, times, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  # Standardize new times
  times_std <- (times - attr(model$result, "time_mean")) / attr(model$result, "time_sd")

  # Create design matrices including intercept
  X_new <- cbind(1, poly(times_std, degree = model$mu, raw = TRUE))
  Z_new <- cbind(1, poly(times_std, degree = model$sigma, raw = TRUE))

  predicted_mu_std <- X_new %*% model$beta_est
  predicted_sigma_std <- exp(Z_new %*% model$gamma_est)

  # Unstandardize predictions
  predicted_mu <- predicted_mu_std * attr(model$result, "score_sd") + attr(model$result, "score_mean")
  predicted_sigma <- predicted_sigma_std * attr(model$result, "score_sd")

  if(is.null(n))
    n <- attr(model$result, "max")

  m <- predicted_mu
  var <- predicted_sigma^2

  m2 <- m*m
  m3 <- m2*m

  a <- (m2*n - m3 - m*var)/(n*var - n*m + m2)
  b <- a*((n - m)/m)

  predicted <- data.frame(time = times,
                          mu = as.vector(predicted_mu),
                          sigma = as.vector(predicted_sigma),
                          a = a,
                          b = b)
  return(predicted)
}

#' Compute Parameters of a Beta Binomial Distribution
#'
#' This function calculates the \eqn{\alpha} (a) and \eqn{\beta} (b) parameters of a beta binomial
#' distribution, along with the mean (m), variance (var) based on the input vector `x`
#' and the maximum number `n`.
#'
#' The beta-binomial distribution is a discrete probability distribution that models the
#' number of successes in a fixed number of trials, where the probability of success varies
#' from trial to trial. This variability in success probability is modeled by a beta
#' distribution. Such a calculation is particularly relevant in scenarios where there is
#' heterogeneity in success probabilities across trials, which is common in real-world
#' situations, as for example the number of correct solutions in a psychometric test, where
#' the test has a fixed number of items.
#'
#' @param x A numeric vector of non-negative integers representing observed counts.
#' @param n The maximum number or the maximum possible value of `x`. If not specified, uses max(x) instead.
#'
#' @return A numeric vector containing the calculated parameters in the following order:
#' alpha (a), beta (b), mean (m), standard deviation (sd), and the maximum number (n).
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' n <- 5
#'
#' betaCoefficients(x, n) # or, to set n to max(x)
#' betaCoefficients(x)
#'
#' @export
betaCoefficients <- function(x, n = NULL){
  if(is.null(n))
    n <- max(x)

  m <- mean(x)
  sd <- sd(x)
  var <- sd^2

  m2 <- m*m
  m3 <- m2*m

  a <- (m2*n - m3 - m*var)/(n*var - n*m + m2)
  b <- a*((n - m)/m)

  return(c(a, b, m, sd, n))
}

#' Calculate Cumulative Probabilities, Density, Percentiles, and Z-Scores for
#' Beta-Binomial Distribution
#'
#' @param model The model, which was fitted using the `optimized.model` function.
#' @param times A numeric vector of time points at which to make predictions.
#' @param n The number of items resp. the maximum score.
#' @param m An optional stop criterion in table generation. Positive integer lower than n.
#' @return A data frame with columns: x, Px, Pcum, Percentile, z, norm score
#' @export
normTable.betabinomial <- function(model, times, n = NULL, m = NULL){
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  if(is.null(n))
    n <- attr(model$result, "max")

  if(is.null(m))
    m <- n
  else if(m > n)
    m <- n

  predictions <- predict.cnormBetaBinomial(model, times, n)
  a <- predictions$a
  b <- predictions$b

  result <- list()

  for(k in 1:length(a)){
    x <- seq(from = 0, to = m)

    # Calculate probabilities using log-space to avoid overflow
    log_pmf <- lchoose(n, x) + lbeta(x + a[k], n - x + b[k]) - lbeta(a[k], b[k])
    Px <- exp(log_pmf)

    # Normalize to ensure sum to 1
    Px <- Px / sum(Px)

    # Calculate cumulative probabilities
    cum <- cumsum(Px)

    # Calculate percentiles
    perc <- cum - 0.5 * Px

    # Calculate z-scores
    z <- qnorm(perc)
    mScale <- attr(model$result, "scaleMean")
    sdScale <- attr(model$result, "scaleSD")
    norm <- rep(NA, length(z))

    if(!is.na(mScale) && !is.na(sdScale)){
      norm <- mScale + sdScale * z
    }

    df <- data.frame(x = x, Px = Px, Pcum = cum, Percentile = perc, z = z, norm = norm)
    result[[k]] <- df
  }

  return(result)
}

#' Predict Norm Scores from Raw Scores
#'
#' This function calculates norm scores based on raw scores, age, and a fitted cnormBetaBinomial model.
#'
#' @param raw A numeric vector of raw scores.
#' @param age A numeric vector of ages, same length as raw.
#' @param model A fitted model object of class "cnormBetaBinomial".
#' @param n The number of items resp. the maximum score.
#'
#' @return A numeric vector of norm scores.
#'
#' @details
#' The function first predicts the alpha and beta parameters of the beta-binomial distribution
#' for each age using the provided model. It then calculates the cumulative probability for
#' each raw score given these parameters. Finally, it converts these probabilities to the
#' norm scale specified in the model.
#'
#' @examples
#' # Assuming you have a fitted model named 'bb_model':
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#' raw <- c(100, 121, 97, 180)
#' ages <- c(7, 8, 9, 10)
#' norm_scores <- predictNorm.betabinomial(raw, ages, model)
#'
#' @export
predictNorm.betabinomial <- function(raw, age, model, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  if (length(age) != length(raw)) {
    stop("The lengths of 'ages' and 'raw' must be the same.")
  }

  if(is.null(n)) {
    n <- attr(model$result, "max")
  }

  predictions <- predict.cnormBetaBinomial(model, age, n)
  a <- predictions$a
  b <- predictions$b

  z_scores <- numeric(length(age))

  for (i in 1:length(age)) {
    x <- seq(from = 0, to = n)

    # Calculate probabilities using log-space to avoid overflow
    log_pmf <- lchoose(n, x) + lbeta(x + a[i], n - x + b[i]) - lbeta(a[i], b[i])
    Px <- exp(log_pmf)

    # Normalize to ensure sum to 1
    Px <- Px / sum(Px)

    # Calculate cumulative probabilities
    cum <- cumsum(Px)

    # Calculate percentiles
    perc <- cum - 0.5 * Px

    # Find the index of the raw score
    score_index <- which(x == raw[i])

    if (length(score_index) == 0) {
      warning(paste("Raw score", raw[i], "not found for age", age[i], ". Returning NA."))
      z_scores[i] <- NA
    } else {
      # Calculate z-score
      z_scores[i] <- qnorm(perc[score_index])
    }
  }

  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  if(!is.na(mScale) && !is.na(sdScale)){
    # scale z scores to scale
    z_scores <- mScale + sdScale * z_scores
    return(z_scores)
  }else{
    # percentile
    return(pnorm(z_scores)*100)
  }
}
