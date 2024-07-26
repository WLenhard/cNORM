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
#' @param weights A numeric vector of weights for each observation.
#'
#' @return The negative log-likelihood of the model.
#' @keywords internal
log_likelihood <- function(params, X, Z, y, weights) {
  n_beta <- ncol(X)
  beta <- params[1:n_beta]
  gamma <- params[(n_beta+1):length(params)]

  mu <- X %*% beta
  log_sigma <- Z %*% gamma
  sigma <- exp(log_sigma)

  ll <- sum(weights * dnorm(y, mean = mu, sd = sigma, log = TRUE))
  return(-ll)  # Return negative log-likelihood for minimization
}


#' Fit a heteroscedastic regression model
#'
#' This function fits a heteroscedastic regression model where both the mean and
#' standard deviation of the response variable are modeled as polynomial functions
#' of the predictor variable.
#'
#' @param age A numeric vector of predictor values (e.g., age).
#' @param score A numeric vector of response values.
#' @param weights A numeric vector of weights for each observation. Default is NULL (equal weights).
#' @param mu Integer specifying the degree of the polynomial for the mean model. Default is 2.
#' @param sigma Integer specifying the degree of the polynomial for the standard deviation model. Default is 1.
#' @param control A list of control parameters to be passed to the `optim` function.
#'   If NULL, default values are used.
#' @param n Number of items in the test, resp. maximum score to be achieved
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param plot Logical indicating whether to plot the model. Default is TRUE.
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
#' # In modelling with the beta binomial function, just like in \code{\link{cnorm}}, weights
#' # can be applied as a means for post stratification. Please use the raking function
#' # \code{\link{computeWeightsl}}. Here an example for the ppvt dataset:
#' margins <- data.frame(variables = c("sex", "sex",
#'                                     "migration", "migration"),
#'                       levels = c(1, 2, 0, 1),
#'                       share = c(.52, .48, .7, .3))
#' weights <- computeWeights(ppvt, margins)
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw, weights = weights)
#'
#' @export
cnorm.betabinomial <- function(age, score, weights = NULL, mu = 3, sigma = 3, n = NULL, control = NULL, scale = "T", plot = T) {
  # If weights are not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1, length(age))
  } else if (length(weights) != length(age)) {
    stop("Length of weights must match length of age and score")
  }

  # Standardize inputs
  age_std <- standardize(age)
  score_std <- standardize(score)

  # Set up 'data' object containing both variables
  data <- data.frame(age = age_std, score = score_std)

  # Prepare the data matrices for mu and sigma, including intercept
  X <- cbind(1, poly(data$age, degree = mu, raw = TRUE))
  Z <- cbind(1, poly(data$age, degree = sigma, raw = TRUE))
  y <- data$score

  # Initial parameters: use some sensible starting values
  initial_params <- c(mean(y), rep(0, mu), log(sd(y)), rep(0, sigma))

  # Optimize to find parameter estimates. If control is NULL, set default
  if(is.null(control))
    control = list(reltol = 1e-8, maxit = 1000)

  result <- optim(initial_params, log_likelihood, X = X, Z = Z, y = y, weights = weights,
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

  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- n
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD

  model <- list(beta_est = beta_est, gamma_est = gamma_est, se = se,
                mu = mu, sigma = sigma,
                result = result)
  class(model) <- "cnormBetaBinomial"



  if(plot){
    p <- plot.betabinomial(model, age, score)
    print(p)
  }

  return(model)
}

#' Predict mean and standard deviation for a heteroscedastic regression model
#'
#' This function generates predictions from a fitted heteroscedastic regression model
#' for new age points.
#'
#' @param model An object of class "cnorm_betabinomial", typically the result of a call to \code{\link{cnorm.betabinomial}}.
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The maximum score to be achieved.
#'
#' @return A data frame with columns:
#'   \item{age}{The input age points}
#'   \item{mu}{Predicted mean values}
#'   \item{sigma}{Predicted standard deviation values}
#'
#' @details
#' This function takes a fitted heteroscedastic regression model and generates predictions
#' for new age points. It applies the same standardization used in model fitting,
#' generates predictions on the standardized scale, and then transforms these back
#' to the original scale.
#'
#' @keywords export
predict.cnormBetaBinomial <- function(model, ages, n = NULL) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Create design matrices including intercept
  X_new <- cbind(1, poly(ages_std, degree = model$mu, raw = TRUE))
  Z_new <- cbind(1, poly(ages_std, degree = model$sigma, raw = TRUE))

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

  predicted <- data.frame(age = ages,
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
#' This function generates a norm table for a specific ages based on the beta binomial
#' regression model. In case a confidence coefficient (CI, default .9) and the
#' reliability is specified, confidence intervals are computed for the true score
#' estimates, including a correction for regression to the mean (Eid & Schmidt, 2012, p. 272).
#' @param model The model, which was fitted using the `optimized.model` function.
#' @param ages A numeric vector of age points at which to make predictions.
#' @param n The number of items resp. the maximum score.
#' @param m An optional stop criterion in table generation. Positive integer lower than n.
#' @param range The range of the norm scores in standard deviations. Default is 3. Thus, scores in the
#' range of +/- 3 standard deviations are considered.
#' @param CI confidence coefficient, ranging from 0 to 1, default .9
#' @param reliability coefficient, ranging between  0 to 1

#' @return A list of data frames with columns: x, Px, Pcum, Percentile, z, norm score
#' and possibly confidence interval
#' @export
normTable.betabinomial <- function(model, ages, n = NULL, m = NULL, range = 3,
                                   CI = .9,
                                   reliability = NULL){
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
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

  if(is.null(n))
    n <- attr(model$result, "max")

  if(is.null(m))
    m <- n
  else if(m > n)
    m <- n

  predictions <- predict.cnormBetaBinomial(model, ages, n)
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
    perc <- (cum - 0.5 * Px)

    # Calculate z-scores
    z <- qnorm(perc)
    z[z < -range] <- -range
    z[z > range] <- range



    mScale <- attr(model$result, "scaleMean")
    sdScale <- attr(model$result, "scaleSD")
    norm <- rep(NA, length(z))

    if(!is.na(mScale) && !is.na(sdScale)){
      norm <- mScale + sdScale * z
    }

    df <- data.frame(x = x, Px = Px, Pcum = cum, Percentile = perc * 100, z = z, norm = norm)

    if(rel){
      zPredicted <- reliability * z
      df$lowerCI <- (zPredicted - se) * sdScale + mScale
      df$upperCI <- (zPredicted + se) * sdScale + mScale
      df$lowerCI_PR <- pnorm(zPredicted - se) * 100
      df$upperCI_PR <- pnorm(zPredicted + se) * 100

    }
    result[[k]] <- df
  }
  names(result) <- ages
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

#' Plot cnormBetaBinomial Model with Data and Percentile Lines
#'
#' This function creates a ggplot visualization of a fitted cnormBetaBinomial model,
#' including the original data points and specified percentile lines.
#'
#' @param model A fitted model object of class "cnormBetaBinomial".
#' @param age A vector the age data.
#' @param score A vector of the score data.
#' @param percentiles A numeric vector of percentiles to plot (between 0 and 1).
#'
#' @return A ggplot object.
#'
#' @export
plot.betabinomial <- function(model, age, score,
                              percentiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Model must be of class 'cnormBetaBinomial'")
  }

  if(length(age) != length(score)){
    stop("Length of 'age' and 'score' must be the same.")
  }

  # Generate prediction points
  n_points <- 100
  data <- data.frame(age = age, score = score)
  age_range <- range(age)
  pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)


  # Get predictions
  preds <- predict(model, pred_ages)

  # Calculate percentile lines
  percentile_lines <- lapply(percentiles, function(p) {
    qbeta(p, shape1 = preds$a, shape2 = preds$b) * attr(model$result, "max")
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(age = pred_ages,
                          mu = preds$mu,
                          sigma = preds$sigma,
                          percentile_data)

  # Create the plot
  p <- ggplot() +
    geom_point(data = data, aes_string(x = "age", y = "score"), alpha = 0.2, size = 0.6)

  # Add percentile lines
  colors <- rainbow(length(percentiles))
  for (i in seq_along(percentiles)) {
    p <- p + geom_line(data = plot_data,
                       aes_string(x = "age", y = paste0("P", percentiles[i] * 100)),
                       color = colors[i], size = 0.6)
  }

  # Customize the plot
  p <- p +
    theme_minimal() +
    labs(title = "Percentile Plot (Beta-Binomial Model)",
         x = "Age",
         y = "Score",
         color = "Percentile") +
    scale_y_continuous(limits = c(0, attr(model$result, "max"))) +
    scale_color_manual(values = colors,
                       breaks = paste0(percentiles * 100, "%"),
                       labels = paste0(percentiles * 100, "%"))

  return(p)
}

#' Diagnostic Information for Beta-Binomial Model
#'
#' This function provides diagnostic information for a fitted beta-binomial model
#' from the cnorm.betabinomial function. It returns various metrics related to
#' model convergence, fit, and complexity.
#'
#' @param model An object of class "cnormBetaBinomial", typically the result of a call to cnorm.betabinomial().
#'
#' @return A list containing the following diagnostic information:
#' \itemize{
#'   \item converged: Logical indicating whether the optimization algorithm converged.
#'   \item n_evaluations: Number of function evaluations performed during optimization.
#'   \item n_gradient: Number of gradient evaluations performed during optimization.
#'   \item final_value: Final value of the objective function (negative log-likelihood).
#'   \item message: Any message returned by the optimization algorithm.
#'   \item AIC: Akaike Information Criterion.
#'   \item BIC: Bayesian Information Criterion.
#'   \item max_gradient: Maximum absolute gradient at the solution (if available).
#' }
#'
#' @details
#' The AIC and BIC are calculated as:
#' AIC = 2k - 2ln(L)
#' BIC = ln(n)k - 2ln(L)
#' where k is the number of parameters, L is the maximum likelihood, and n is the number of observations.
#'
#' @examples
#' # Fit a beta-binomial model
#' model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
#'
#' # Get diagnostic information
#' diag_info <- diagnostics.betabinomial(model)
#'
#' # Print the diagnostic information
#' print(diag_info)
#'
#' # Check if the model converged
#' if(diag_info$converged) {
#'   cat("Model converged successfully.\n")
#' } else {
#'   cat("Warning: Model did not converge.\n")
#' }
#'
#' # Compare AIC and BIC
#' cat("AIC:", diag_info$AIC, "\n")
#' cat("BIC:", diag_info$BIC, "\n")
#'
#' @export
diagnostics.betabinomial <- function(model) {
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  opt_results <- model$result
  n_params <- length(model$beta_est) + length(model$gamma_est)
  n_obs <- attr(model$result, "N")
  convergence <- opt_results$convergence==0

  max_gradient <- NA

  if(is.numeric(opt_results$gradient) && length(opt_results$gradient) > 0)
    max(abs(opt_results$gradient))

  list(
    converged = convergence,
    n_evaluations = opt_results$counts["function"],
    n_gradient = opt_results$counts["gradient"],
    final_value = opt_results$value,
    message = opt_results$message,
    AIC = 2 * n_params + 2 * opt_results$value,
    BIC = log(n_obs) * n_params + 2 * opt_results$value,
    max_gradient = max_gradient
  )
}
