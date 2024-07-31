#' Calculate the negative log-likelihood for a beta-binomial regression model
#'
#' This function computes the negative log-likelihood for a beta-binomial regression model
#' where both the alpha and beta parameters are modeled as functions of predictors.
#'
#' @param params A numeric vector containing all model parameters. The first n_alpha elements
#'               are coefficients for the alpha model, and the remaining elements are
#'               coefficients for the beta model.
#' @param X A matrix of predictors for the alpha model.
#' @param Z A matrix of predictors for the beta model.
#' @param y A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial distribution).
#' @param weights A numeric vector of weights for each observation. If NULL, equal weights are used.
#'
#' @return The negative log-likelihood of the model.
#'
#' @details
#' This function uses a numerically stable implementation of the beta-binomial log-probability.
#' It allows for weighted observations, which can be useful for various modeling scenarios.
#'
#' @keywords internal
log_likelihood2 <- function(params, X, Z, y, n, weights = NULL) {
  n_alpha <- ncol(X)
  alpha_coef <- params[1:n_alpha]
  beta_coef <- params[(n_alpha+1):length(params)]

  log_alpha <- X %*% alpha_coef
  log_beta <- Z %*% beta_coef
  alpha <- exp(log_alpha)
  beta <- exp(log_beta)

  # More numerically stable implementation of beta-binomial log-probability
  dbetabinom <- function(x, size, alpha, beta, log = FALSE) {
    logp <- lchoose(size, x) +
      lgamma(size + 1) +
      lgamma(alpha + x) +
      lgamma(beta + size - x) +
      lgamma(alpha + beta) -
      lgamma(x + 1) -
      lgamma(size - x + 1) -
      lgamma(size + alpha + beta) -
      lgamma(alpha) -
      lgamma(beta)

    if (log) return(logp) else return(exp(logp))
  }

  # If weights are not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1, length(y))
  }

  ll <- sum(weights * dbetabinom(y, size = n, alpha = alpha, beta = beta, log = TRUE))
  return(-ll)  # Return negative log-likelihood for minimization
}


#' Fit a beta-binomial regression model for continuous norming
#'
#' This function fits a beta-binomial regression model where both the alpha and beta
#' parameters of the beta-binomial distribution are modeled as polynomial functions
#' of the predictor variable (typically age). While 'cnorm-betabinomial' fits a beta-binomial model
#' on the basis of /$mu$ and /$sigma$, this function fits a beta-binomial model directly on the basis
#' of /$alpha$ and /$beta$.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param n The maximum score (number of trials in the beta-binomial distribution). If NULL, max(score) is used.
#' @param weights A numeric vector of weights for each observation. Default is NULL (equal weights).
#' @param alpha_degree Integer specifying the degree of the polynomial for the alpha model. Default is 3.
#' @param beta_degree Integer specifying the degree of the polynomial for the beta model. Default is 3.
#' @param control A list of control parameters to be passed to the `optim` function.
#'   If NULL, default values are used.
#' @param scale Type of norm scale, either "T" (default), "IQ", "z" or a double vector with the mean and standard deviation.
#' @param plot Logical indicating whether to plot the model. Default is TRUE.
#'
#' @return A list of class "cnormBetaBinomial2" containing:
#'   \item{alpha_est}{Estimated coefficients for the alpha model}
#'   \item{beta_est}{Estimated coefficients for the beta model}
#'   \item{se}{Standard errors of the estimated coefficients}
#'   \item{alpha_degree}{Degree of the polynomial for the alpha model}
#'   \item{beta_degree}{Degree of the polynomial for the beta model}
#'   \item{result}{Full result from the optimization procedure}
#'
#' @details
#' The function standardizes the input variables, fits polynomial models for both
#' the alpha and beta parameters, and uses maximum likelihood estimation to
#' find the optimal parameters. The optimization is performed using the L-BFGS-B method.
#'
#' @examples
#' # Fit a beta-binomial regression model to the PPVT data
#' model <- cnorm.betabinomial2(ppvt$age, ppvt$raw)
#' summary(model)
#'
#' # Use weights for post-stratification
#' weights <- computeWeights(ppvt, margins)
#' model <- cnorm.betabinomial2(ppvt$age, ppvt$raw, weights = weights)
#'
#' @export
cnorm.betabinomial2 <- function(age, score, n = NULL, weights = NULL, alpha_degree = 3, beta_degree = 3, control = NULL, scale = "T", plot = T) {
  # Standardize inputs
  age_std <- standardize(age)

  # Set up 'data' object containing both variables
  data <- data.frame(age = age_std, score = score)
  if(is.null(n))
    n <- max(score)

  # Prepare the data matrices for alpha and beta, including intercept
  X <- cbind(1, poly(data$age, degree = alpha_degree, raw = TRUE))
  Z <- cbind(1, poly(data$age, degree = beta_degree, raw = TRUE))
  y <- data$score

  # Initial parameters: use some sensible starting values
  initial_alpha <- log(mean(y) / (n - mean(y)) + 1e-6)  # Add small constant
  initial_beta <- log(1 + 1e-6)  # Add small constant
  initial_params <- c(initial_alpha, rep(0, alpha_degree), initial_beta, rep(0, beta_degree))

  # Optimize to find parameter estimates. If control is NULL, set default
  if(is.null(control))
    control = list(factr = 1e-8, maxit = 1000)

  result <- optim(initial_params, log_likelihood2, X = X, Z = Z, y = y, n = n, weights = weights,
                  method = "L-BFGS-B", hessian = TRUE,
                  control = control)

  # Extract results and calculate standard errors
  alpha_est <- result$par[1:(alpha_degree + 1)]
  beta_est <- result$par[(alpha_degree + 2):length(result$par)]
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

  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD

  model <- list(alpha_est = alpha_est, beta_est = beta_est, se = se,
                alpha_degree = alpha_degree, beta_degree = beta_degree,
                result = result)

  class(model) <- "cnormBetaBinomial2"
  if(plot){
    p <- plot.betabinomial(model, age, score)
    print(p)
  }

  return(model)
}


#' Predict alpha and beta parameters for a beta-binomial regression model
#'
#' This function generates predictions from a fitted beta-binomial regression model
#' for new age points.
#'
#' @param model An object of class "cnormBetaBinomial2", typically the result of a call to cnorm.betabinomial2().
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The maximum score to be achieved.
#'
#' @return A data frame with columns:
#'   \item{age}{The input age points}
#'   \item{a}{Predicted alpha values}
#'   \item{b}{Predicted beta values}
#'   \item{mu}{Predicted mean values}
#'   \item{sigma}{Predicted standard deviation values}
#'
#' @details
#' This function takes a fitted beta-binomial regression model and generates predictions
#' for new age points. It applies the same standardization used in model fitting,
#' generates predictions on the standardized scale, and then transforms these back
#' to the original scale.
#'
#' @keywords export
predict.cnormBetaBinomial2 <- function(model, ages, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial2")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial2'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Create design matrices including intercept
  X_new <- cbind(1, poly(ages_std, degree = model$alpha_degree, raw = TRUE))
  Z_new <- cbind(1, poly(ages_std, degree = model$beta_degree, raw = TRUE))

  log_alpha <- X_new %*% model$alpha_est
  log_beta <- Z_new %*% model$beta_est

  alpha <- exp(log_alpha)
  beta <- exp(log_beta)

  if(is.null(n))
    n <- attr(model$result, "max")

  # Calculate mean and variance of beta-binomial distribution
  mu <- n * alpha / (alpha + beta)
  var <- (n * alpha * beta * (alpha + beta + n)) / ((alpha + beta)^2 * (alpha + beta + 1))
  sigma <- sqrt(var)

  predicted <- data.frame(age = ages,
                          a = as.vector(alpha),
                          b = as.vector(beta),
                          mu = as.vector(mu),
                          sigma = as.vector(sigma))
  return(predicted)
}
