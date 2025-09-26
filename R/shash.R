#' Sinh-Arcsinh (ShaSh) distribution functions
#'
#' Density, distribution function, quantile function and random generation
#' for the Sinh-Arcsinh distribution
#'
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param mu location parameter
#' @param sigma scale parameter (> 0)
#' @param epsilon skewness parameter
#' @param delta tail weight parameter (> 0)
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p)
#' @param lower.tail logical; if TRUE, probabilities are P[X <= x]
#'
#' @keywords internal

# Density function
dshash <- function(x, mu = 0, sigma = 1, epsilon = 0, delta = 1, log = FALSE) {
  z <- (x - mu) / sigma
  sinh_z <- sinh(delta * asinh(z) + epsilon)
  cosh_z <- cosh(delta * asinh(z) + epsilon)

  logdens <- log(delta) - log(sigma) - 0.5 * log(2 * pi) -
    0.5 * log(1 + z^2) - 0.5 * sinh_z^2 + log(cosh_z)

  if (log) return(logdens) else return(exp(logdens))
}

# Distribution function
pshash <- function(q, mu = 0, sigma = 1, epsilon = 0, delta = 1,
                   lower.tail = TRUE, log.p = FALSE) {
  z <- (q - mu) / sigma
  u <- sinh(delta * asinh(z) + epsilon)

  p <- pnorm(u, lower.tail = lower.tail, log.p = log.p)
  return(p)
}

# Quantile function
qshash <- function(p, mu = 0, sigma = 1, epsilon = 0, delta = 1,
                   lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p

  u <- qnorm(p)
  z <- sinh((asinh(u) - epsilon) / delta)
  x <- mu + sigma * z

  return(x)
}

# Random generation
rshash <- function(n, mu = 0, sigma = 1, epsilon = 0, delta = 1) {
  u <- rnorm(n)
  z <- sinh((asinh(u) - epsilon) / delta)
  x <- mu + sigma * z

  return(x)
}

#' Calculate the negative log-likelihood for a ShaSh regression model
#'
#' This function computes the negative log-likelihood for a Sinh-Arcsinh regression model
#' where the location, scale, and skewness parameters are modeled as functions of predictors.
#'
#' @param params A numeric vector containing all model parameters
#' @param X_mu Design matrix for location parameter
#' @param X_sigma Design matrix for scale parameter
#' @param X_epsilon Design matrix for skewness parameter
#' @param y Response vector
#' @param weights Observation weights
#' @param delta Fixed tail weight parameter
#'
#' @return The negative log-likelihood of the model
#'
#' @keywords internal
log_likelihood_shash <- function(params, X_mu, X_sigma, X_epsilon, y, weights = NULL, delta = 1) {
  n_mu <- ncol(X_mu)
  n_sigma <- ncol(X_sigma)
  n_epsilon <- ncol(X_epsilon)

  # Extract parameter vectors
  mu_coef <- params[1:n_mu]
  sigma_coef <- params[(n_mu + 1):(n_mu + n_sigma)]
  epsilon_coef <- params[(n_mu + n_sigma + 1):(n_mu + n_sigma + n_epsilon)]

  # Compute parameters with constraints
  mu <- X_mu %*% mu_coef
  log_sigma <- pmax(pmin(X_sigma %*% sigma_coef, 10), -10)
  sigma <- exp(log_sigma)
  epsilon <- pmax(pmin(X_epsilon %*% epsilon_coef, 10), -10)

  # Use weights if provided
  if (is.null(weights)) {
    weights <- rep(1, length(y))
  }

  # Calculate log-likelihood using dshash
  loglik <- 0
  for (i in 1:length(y)) {
    logdens <- dshash(y[i], mu = mu[i], sigma = sigma[i],
                      epsilon = epsilon[i], delta = delta, log = TRUE)

    if (!is.finite(logdens)) {
      return(1e10)  # Penalty for invalid parameters
    }

    loglik <- loglik + weights[i] * logdens
  }

  if (!is.finite(loglik)) {
    return(1e10)
  }

  return(-loglik)  # Return negative log-likelihood for minimization
}

#' Fit a Sinh-Arcsinh (ShaSh) Regression Model for Continuous Norming
#'
#' This function fits a Sinh-Arcsinh (ShaSh; Jones & Pewsey, 2009) regression model for continuous norm
#' score modeling, where the distribution parameters vary smoothly as polynomial functions of age or other
#' predictors. The ShaSh distribution is well-suited for psychometric data as it can flexibly model
#' skewness and tail weight independently, making it ideal for handling floor effects, ceiling effects,
#' and varying degrees of individual differences across age groups. In a simulation study (Lenhard et
#' al, 2019), the ShaSh model demonstrated superior performance compared to other parametric approaches
#' from the Box Cox family of functions. In contrast to Box Cox, Sinh-Arcsinh can model distributions
#' including zero and negativ values.
#'
#' Caution! The fitting procedure is ressource intensive an can take some time.
#'
#' @param age A numeric vector of predictor values (typically age, but can be any continuous predictor).
#' @param score A numeric vector of response values (raw test scores). Must be the same length as age.
#'   The value range is unresticted and it can include zeros and negative values.
#' @param weights An optional numeric vector of weights for each observation.
#'   Useful for incorporating sampling weights. If NULL (default), all observations are weighted equally.
#' @param mu_degree Integer specifying the degree of the polynomial for modeling the location parameter μ(age).
#'   Default is 3. Higher degrees allow more flexible modeling of how the central tendency changes with age,
#'   but may lead to overfitting with small samples. Common choices:
#'   \itemize{
#'     \item 1: Linear change with age
#'     \item 2: Quadratic change (allows one inflection point)
#'     \item 3: Cubic change (allows two inflection points, suitable for most developmental curves)
#'     \item 4+: Higher-order changes (use cautiously, mainly for large samples)
#'   }
#' @param sigma_degree Integer specifying the degree of the polynomial for modeling the scale parameter σ(age).
#'   Default is 2. This controls how the variability (spread) of scores changes with age.
#'   Lower degrees are often sufficient as variability typically changes more smoothly than location.
#' @param epsilon_degree Integer specifying the degree of the polynomial for modeling the skewness parameter ε(age).
#'   Default is 1. This controls how the asymmetry of the distribution changes with age.
#'
#' @param delta Fixed tail weight parameter (must be > 0). Default is 1. This parameter controls the
#'   heaviness of the distribution tails and is kept constant across all ages in this implementation.
#'   \itemize{
#'     \item δ = 1: Normal-like tail behavior (baseline)
#'     \item δ > 1: Heavier tails, higher kurtosis (more extreme scores than normal distribution)
#'     \item δ < 1: Lighter tails, lower kurtosis (fewer extreme scores than normal distribution)
#'   }
#'
#' @param control An optional list of control parameters passed to the \code{optim} function for
#'   maximum likelihood estimation. If NULL, sensible defaults are chosen automatically based on
#'   the model complexity. Common parameters to adjust:
#'   \itemize{
#'     \item \code{factr}: Controls precision of optimization (default: 1e-8)
#'     \item \code{maxit}: Maximum number of iterations (default: n_parameters * 200)
#'     \item \code{lmm}: Memory limit for L-BFGS-B (default: min(n_parameters, 20))
#'   }
#'   Increase \code{maxit} or decrease \code{factr} if optimization fails to converge.
#'
#' @param scale Character string or numeric vector specifying the type of norm scale for output.
#'   This affects the scaling of derived norm scores but does not influence model fitting:
#'   \itemize{
#'     \item "T": T-scores (mean = 50, SD = 10) - default
#'     \item "IQ": IQ-like scores (mean = 100, SD = 15)
#'     \item "z": z-scores (mean = 0, SD = 1)
#'     \item c(M, SD): Custom scale with specified mean M and standard deviation SD
#'   }
#'
#' @param plot Logical indicating whether to automatically display a diagnostic plot of the fitted model.
#'   Default is TRUE.
#'
#' @return An object of class "cnormShaSh" containing the fitted model results. This is a list with components:
#'   \item{mu_est}{Numeric vector of estimated coefficients for the location parameter μ(age).
#'     The first coefficient is the intercept, subsequent coefficients correspond to polynomial terms.}
#'   \item{sigma_est}{Numeric vector of estimated coefficients for the scale parameter log(σ(age)).
#'     Note: These are coefficients for log(σ) to ensure σ > 0.}
#'   \item{epsilon_est}{Numeric vector of estimated coefficients for the skewness parameter ε(age).}
#'   \item{delta}{The fixed tail weight parameter value used in fitting.}
#'   \item{se}{Numeric vector of standard errors for all estimated coefficients (if Hessian computation succeeds).}
#'   \item{mu_degree, sigma_degree, epsilon_degree}{The polynomial degrees used for each parameter.}
#'   \item{result}{Complete output from the \code{optim} function, including convergence information,
#'     log-likelihood value, and other optimization details.}
#'
#' @details
#' \subsection{The Sinh-Arcsinh Distribution}{
#' The ShaSh distribution is defined by the transformation:
#' \deqn{Z = \sinh(\delta \cdot \text{arcsinh}(Y) + \epsilon)}
#' where Y ~ N(0,1), and the final variable is X = μ + σZ.
#'
#' This transformation provides:
#' \itemize{
#'   \item μ: Location parameter (similar to mean)
#'   \item σ: Scale parameter (similar to standard deviation)
#'   \item ε: Skewness parameter (ε = 0 for symmetry)
#'   \item δ: Tail weight parameter (δ = 1 for normal-like tails)
#' }
#' }
#'
#'
#' \subsection{Model Fitting}{
#' Parameters are estimated using maximum likelihood via the L-BFGS-B algorithm. The function
#' includes several robustness features:
#' \itemize{
#'   \item Automatic parameter bounds to prevent numerical overflow
#'   \item Multiple starting values if initial optimization fails
#'   \item Fallback optimization settings for difficult convergence cases
#'   \item Comprehensive convergence diagnostics
#' }
#' }
#'
#' \subsection{Model Selection}{
#' Choose polynomial degrees based on:
#' \itemize{
#'   \item Sample size (higher degrees need more data)
#'   \item Theoretical expectations about developmental trajectories
#'   \item Model comparison criteria (AIC, BIC)
#'   \item Visual inspection of fitted curves
#' }
#'
#' For most applications, mu_degree = 3, sigma_degree = 2, epsilon_degree = 2 provides
#' a good balance of flexibility and parsimony.
#' }
#'
#' @note
#' \itemize{
#'   \item The function requires the input data to have sufficient variability. Very small datasets
#'     or datasets with little age spread may cause convergence problems.
#'   \item Polynomial models can exhibit edge effects at the boundaries of the age range.
#'     Predictions outside the observed age range should be made cautiously.
#'   \item If convergence fails, try: (1) reducing polynomial degrees, (2) adjusting the delta parameter,
#'     (3) providing custom control parameters, or (4) checking for data quality issues.
#'   \item The tail weight parameter δ is fixed across ages in this implementation. For applications
#'     where tail behavior changes substantially with age, consider extending the model to include δ(age).
#' }
#'
#' @seealso
#' \code{\link{plot.cnormShaSh}} for plotting fitted models,
#' \code{\link{predict.cnormShaSh}} for generating predictions,
#' \code{\link{cnorm.betabinomial2}} for discrete beta-binomial alternative
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' model <- cnorm.shash(age = children$age, score = children$raw_score)
#'
#' # Custom polynomial degrees for complex developmental pattern
#' model_complex <- cnorm.shash(
#'   age = adolescents$age,
#'   score = adolescents$vocabulary_score,
#'   mu_degree = 4,      # Complex mean trajectory
#'   sigma_degree = 3,   # Changing variability pattern
#'   epsilon_degree = 2, # Skewness shifts
#'   delta = 1.3         # Slightly heavy tails
#' )
#'
#' # Homogeneous population with light tails
#' model_selective <- cnorm.shash(
#'   age = gifted$age,
#'   score = gifted$achievement,
#'   delta = 0.8,        # Lighter tails for selected population
#'   sigma_degree = 1    # Simple linear variance change
#' )
#'
#' # With sampling weights
#' model_weighted <- cnorm.shash(
#'   age = survey$age,
#'   score = survey$score,
#'   weights = survey$sample_weight
#' )
#'
#' # Custom optimization control for difficult convergence
#' model_robust <- cnorm.shash(
#'   age = mixed$age,
#'   score = mixed$score,
#'   control = list(factr = 1e-6, maxit = 2000),
#'   delta = 1.5
#' )
#'
#' # Model comparison for delta selection
#' models <- list(
#'   light = cnorm.shash(age, score, delta = 0.8, plot = FALSE),
#'   normal = cnorm.shash(age, score, delta = 1.0, plot = FALSE),
#'   heavy = cnorm.shash(age, score, delta = 1.5, plot = FALSE)
#' )
#'
#' # Compare AIC values
#' sapply(models, function(m) 2 * length(m$result$par) + 2 * m$result$value)
#' }
#'
#' @author Wolfgang Lenhard
#' @references
#' Jones, M. C., & Pewsey, A. (2009). Sinh-arcsinh distributions. *Biometrika*, 96(4), 761-780.
#'
#' Lenhard, A., Lenhard, W., Gary, S. (2019). Continuous norming of psychometric tests: A
#' simulation study of parametric and semi-parametric approaches. PLoS ONE, 14(9), e0222279.
#' https://doi.org/10.1371/journal.pone.0222279
#'
#' @export
cnorm.shash <- function(age,
                        score,
                        weights = NULL,
                        mu_degree = 3,
                        sigma_degree = 2,
                        epsilon_degree = 1,
                        delta = 1,
                        control = NULL,
                        scale = "T",
                        plot = TRUE) {

  # Input validation
  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (delta <= 0) {
    stop("Delta parameter must be positive.")
  }

  # Standardize age
  age_std <- standardize(age)

  # Create design matrices
  X_mu <- cbind(1, poly(age_std, degree = mu_degree, raw = TRUE))
  X_sigma <- cbind(1, poly(age_std, degree = sigma_degree, raw = TRUE))
  X_epsilon <- cbind(1, poly(age_std, degree = epsilon_degree, raw = TRUE))

  # Calculate initial parameter values
  initial_mu <- mean(score)
  initial_sigma <- sd(score)
  initial_epsilon <- 0  # Start with symmetric distribution

  # Initial parameter vectors
  initial_params <- c(
    c(initial_mu, rep(0, mu_degree)),           # mu parameters
    c(log(initial_sigma), rep(0, sigma_degree)), # sigma parameters
    c(initial_epsilon, rep(0, epsilon_degree))   # epsilon parameters
  )

  # Control parameters
  if (is.null(control)) {
    n_param <- length(initial_params)
    control <- list(
      factr = 1e-8,
      maxit = n_param * 200,
      lmm = min(n_param, 20)
    )
  }

  # Parameter bounds
  n_params <- length(initial_params)
  lower_bounds <- rep(-Inf, n_params)
  upper_bounds <- rep(Inf, n_params)

  # Bounds for sigma parameters (keep log_sigma reasonable)
  sigma_start <- ncol(X_mu) + 1
  sigma_end <- sigma_start + ncol(X_sigma) - 1
  lower_bounds[sigma_start:sigma_end] <- -10
  upper_bounds[sigma_start:sigma_end] <- 10

  # Bounds for epsilon parameters
  epsilon_start <- sigma_end + 1
  epsilon_end <- length(initial_params)
  lower_bounds[epsilon_start:epsilon_end] <- -10
  upper_bounds[epsilon_start:epsilon_end] <- 10

  # Optimization
  result <- tryCatch({
    optim(
      initial_params,
      log_likelihood_shash,
      X_mu = X_mu,
      X_sigma = X_sigma,
      X_epsilon = X_epsilon,
      y = score,
      weights = weights,
      delta = delta,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      hessian = TRUE,
      control = control
    )
  }, error = function(e) {
    message("First optimization attempt failed. Trying with different parameters...")

    # Try with different initial values
    initial_params[1] <- median(score)
    initial_params[ncol(X_mu) + 1] <- log(mad(score))

    # More relaxed control
    control$factr <- control$factr * 10
    control$maxit <- control$maxit * 2

    optim(
      initial_params,
      log_likelihood_shash,
      X_mu = X_mu,
      X_sigma = X_sigma,
      X_epsilon = X_epsilon,
      y = score,
      weights = weights,
      delta = delta,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      hessian = TRUE,
      control = control
    )
  })

  # Check convergence
  if (result$convergence != 0) {
    warning("Optimization did not converge (code: ", result$convergence,
            "). Consider adjusting control parameters or degrees of the polynomials. Check percentile curves for plausibility.")
  }

  # Extract parameter estimates
  n_mu <- ncol(X_mu)
  n_sigma <- ncol(X_sigma)
  n_epsilon <- ncol(X_epsilon)

  mu_est <- result$par[1:n_mu]
  sigma_est <- result$par[(n_mu + 1):(n_mu + n_sigma)]
  epsilon_est <- result$par[(n_mu + n_sigma + 1):(n_mu + n_sigma + n_epsilon)]

  # Calculate standard errors
  se <- tryCatch({
    sqrt(diag(solve(result$hessian)))
  }, error = function(e) {
    warning("Could not compute standard errors: Hessian matrix issue")
    rep(NA, length(result$par))
  })

  # Store scale information
  scaleM <- NA
  scaleSD <- NA

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

  # Store attributes
  attr(result, "age_mean") <- mean(age)
  attr(result, "age_sd") <- sd(age)
  attr(result, "ageMin") <- min(age)
  attr(result, "ageMax") <- max(age)
  attr(result, "score_mean") <- mean(score)
  attr(result, "score_sd") <- sd(score)
  attr(result, "max") <- max(score)
  attr(result, "min") <- min(score)
  attr(result, "N") <- length(score)
  attr(result, "scaleMean") <- scaleM
  attr(result, "scaleSD") <- scaleSD
  attr(result, "delta") <- delta

  # Create model object
  model <- list(
    mu_est = mu_est,
    sigma_est = sigma_est,
    epsilon_est = epsilon_est,
    delta = delta,
    se = se,
    mu_degree = mu_degree,
    sigma_degree = sigma_degree,
    epsilon_degree = epsilon_degree,
    result = result
  )

  class(model) <- "cnormShaSh"

  if (plot) {
    p <- plot.cnormShaSh(model, age, score, weights = weights)
    print(p)
  }

  return(model)
}

#' Predict parameters for a ShaSh regression model
#'
#' @param model An object of class "cnormShaSh"
#' @param ages A numeric vector of age points for prediction
#'
#' @return A data frame with predicted parameters and statistics
#'
#' @keywords internal
predictCoefficients_shash <- function(model, ages) {
  if (!inherits(model, "cnormShaSh")) {
    stop("Wrong object. Please provide object from class 'cnormShaSh'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Create design matrices
  X_mu_new <- cbind(1, poly(ages_std, degree = model$mu_degree, raw = TRUE))
  X_sigma_new <- cbind(1, poly(ages_std, degree = model$sigma_degree, raw = TRUE))
  X_epsilon_new <- cbind(1, poly(ages_std, degree = model$epsilon_degree, raw = TRUE))

  # Predict parameters
  mu <- X_mu_new %*% model$mu_est
  log_sigma <- X_sigma_new %*% model$sigma_est
  sigma <- exp(log_sigma)
  epsilon <- X_epsilon_new %*% model$epsilon_est

  predicted <- data.frame(
    age = ages,
    mu = as.vector(mu),
    sigma = as.vector(sigma),
    epsilon = as.vector(epsilon),
    delta = model$delta
  )

  return(predicted)
}

#' Plot cnormShaSh Model with Data and Percentile Lines
#'
#' @param x A fitted model object of class "cnormShaSh"
#' @param ... Additional arguments including age, score, weights, percentiles, points
#'
#' @return A ggplot object
#'
#' @export
plot.cnormShaSh <- function(x, ...) {
  model <- x
  args <- list(...)

  if ("age" %in% names(args)) { age <- args$age } else {if(length(args)>0) age <- args[[1]] else age <- NULL}
  if ("score" %in% names(args)) { score <- args$score } else {if(length(args)>1) score <- args[[2]] else score <- NULL}
  if ("weights" %in% names(args)) { weights <- args$weights } else { weights <- NULL }
  if ("percentiles" %in% names(args)) { percentiles <- args$percentiles } else { percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975) }
  if ("points" %in% names(args)) { points <- args$points } else { points <- TRUE }

  if(is.null(age) || is.null(score))
    stop("Please provide 'age' and 'score' vectors.")

  if (!inherits(model, "cnormShaSh")) {
    stop("Wrong object. Please provide object from class 'cnormShaSh'.")
  }

  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!is.null(weights) && length(weights) != length(age)) {
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  }

  # Generate prediction points
  n_points <- 100
  data <- data.frame(age = age, score = score)
  if (!is.null(weights)) {
    data$w <- weights
  } else {
    data$w <- rep(1, nrow(data))
  }

  age_range <- range(age)
  pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)

  # Get predictions
  preds <- predictCoefficients_shash(model, pred_ages)

  # Calculate percentile lines using ShaSh quantiles
  percentile_lines <- lapply(percentiles, function(p) {
    qshash(p, mu = preds$mu, sigma = preds$sigma,
           epsilon = preds$epsilon, delta = preds$delta)
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    mu = preds$mu,
    sigma = preds$sigma,
    epsilon = preds$epsilon,
    percentile_data
  )

  # Create the plot
  p <- ggplot()

  if (points)
    p <- p + geom_point(
      data = data,
      aes(x = age, y = score),
      alpha = 0.2,
      size = 0.6
    )

  # Calculate manifest percentiles
  if (length(age) / length(unique(age)) > 50 && min(table(data$age)) > 30) {
    data$group <- age
  } else {
    data$group <- getGroups(age)
  }

  # Get actual percentiles
  NAMES <- paste("PR", percentiles * 100, sep = "")
  percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data$group), function(df) {
    c(age = mean(df$age),
      weighted.quantile(df$score, probs = percentiles, weights = df$w))
  })))
  colnames(percentile.actual) <- c("age", NAMES)
  manifest_data <- percentile.actual

  # Add percentile lines and points
  for (i in seq_along(percentiles)) {
    p <- p +
      geom_line(
        data = plot_data,
        aes(
          x = .data$age,
          y = .data[[paste0("P", percentiles[i] * 100)]],
          color = !!NAMES[i]
        ),
        size = 0.6
      ) +
      geom_point(
        data = manifest_data,
        aes(
          x = .data$age,
          y = .data[[NAMES[i]]],
          color = !!NAMES[i]
        ),
        size = 2,
        shape = 18
      )
  }

  # Customize the plot
  p <- p +
    theme_minimal() +
    labs(
      title = "Percentile Plot (Sinh-Arcsinh Model)",
      x = "Age",
      y = "Score",
      color = "Percentile"
    ) +
    scale_color_manual(
      values = setNames(rainbow(length(percentiles)), NAMES),
      breaks = NAMES,
      labels = paste0(percentiles * 100, "%")
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = rep("solid", length(NAMES)),
      shape = rep(18, length(NAMES))
    )))

  p <- p +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}

#' Print method for cnormShaSh objects
#'
#' @param x A cnormShaSh object
#' @param ... Additional arguments
#' @export
print.cnormShaSh <- function(x, ...) {
  cat("Sinh-Arcsinh Regression Model for Continuous Norming\n")
  cat("====================================================\n\n")

  cat("Model Parameters:\n")
  cat("- Location (mu) polynomial degree:", x$mu_degree, "\n")
  cat("- Scale (sigma) polynomial degree:", x$sigma_degree, "\n")
  cat("- Skewness (epsilon) polynomial degree:", x$epsilon_degree, "\n")
  cat("- Tail weight (delta): Fixed at", x$delta, "\n\n")

  cat("Sample size:", attr(x$result, "N"), "\n")
  cat("Age range:", attr(x$result, "ageMin"), "to", attr(x$result, "ageMax"), "\n")
  cat("Score range:", attr(x$result, "min"), "to", attr(x$result, "max"), "\n\n")

  cat("Optimization:\n")
  cat("- Convergence:", ifelse(x$result$convergence == 0, "Successful", "Failed"), "\n")
  cat("- Log-likelihood:", -x$result$value, "\n")
  cat("- AIC:", 2 * length(x$result$par) + 2 * x$result$value, "\n")
}

#' Calculate Norm Tables for Sinh-Arcsinh Distribution
#'
#' Generates norm tables for specific ages based on a fitted Sinh-Arcsinh (ShaSh) regression model.
#' Computes probabilities, percentiles, z-scores, and norm scores for a specified range of raw scores.
#' Optionally includes confidence intervals when reliability is provided.
#'
#' @param model Fitted ShaSh model object of class "cnormShaSh"
#' @param ages Numeric vector of age points for norm table generation
#' @param start Minimum raw score value for the norm table
#' @param end Maximum raw score value for the norm table
#' @param step Step size between consecutive raw scores (default: 1)
#' @param CI Confidence coefficient (0-1, default: 0.9) for confidence intervals
#' @param reliability Reliability coefficient (0-1) for true score confidence intervals
#'
#' @return List of data frames (one per age) containing:
#'   \item{x}{Raw scores}
#'   \item{Px}{Probability density values}
#'   \item{Pcum}{Cumulative probabilities}
#'   \item{Percentile}{Percentile ranks (0-100)}
#'   \item{z}{Standardized z-scores}
#'   \item{norm}{Norm scores in specified scale}
#'   \item{lowerCI, upperCI}{Confidence intervals (if reliability provided)}
#'   \item{lowerCI_PR, upperCI_PR}{CI as percentile ranks (if reliability provided)}
#'
#' @details
#' For continuous ShaSh distributions, probability densities are computed and converted to
#' cumulative probabilities and percentiles. When reliability is specified, confidence
#' intervals include correction for regression to the mean.
#'
#' @examples
#' \dontrun{
#' # Basic norm table
#' model <- cnorm.shash(age, score)
#' tables <- normTable.shash(model, ages = c(7, 8, 9), start = 0, end = 50)
#'
#' # With confidence intervals and finer granularity
#' tables_ci <- normTable.shash(model, ages = c(8, 9), start = 10, end = 40,
#'                              step = 0.5, CI = 0.95, reliability = 0.85)
#' }
#'
#' @export
normTable.shash <- function(model,
                            ages,
                            start = NULL,
                            end = NULL,
                            step = 1,
                            CI = .9,
                            reliability = NULL) {

  # Input validation
  if (!inherits(model, "cnormShaSh")) {
    stop("Wrong object. Please provide object from class 'cnormShaSh'.")
  }

  if (is.null(start)) {
    start <- attr(model$result, "min")
  }

  if (is.null(end)) {
    end <- attr(model$result, "max")
  }

  if(is.null(step)){
    step <- 1
  }


  if (start >= end) {
    stop("Start value must be less than end value.")
  }

  if (step <= 0) {
    stop("Step size must be positive.")
  }

  if (is.null(CI) || is.na(CI)) {
    reliability <- NULL
  } else if (CI > .99999 || CI < .00001) {
    stop("Confidence coefficient (CI) out of range. Please specify value between 0 and 1.")
  }

  # Setup reliability and confidence intervals
  rel <- FALSE
  if (!is.null(reliability)) {
    if (reliability > .9999 || reliability < .0001) {
      stop("Reliability coefficient out of range. Please specify value between 0 and 1.")
    } else {
      se <- qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))
      rel <- TRUE
    }
  }

  # Get predictions for all ages
  predictions <- predictCoefficients_shash(model, ages)

  # Create score sequence
  x <- seq(from = start, to = end, by = step)

  # Get scale information
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  result <- list()

  # Generate norm table for each age
  for (k in 1:length(ages)) {
    mu_k <- predictions$mu[k]
    sigma_k <- predictions$sigma[k]
    epsilon_k <- predictions$epsilon[k]
    delta_k <- predictions$delta[k]

    # Calculate probability densities
    Px <- dshash(x, mu = mu_k, sigma = sigma_k, epsilon = epsilon_k, delta = delta_k) * step

    # Normalize to ensure sum to approximately 1
    Px <- Px / sum(Px)

    # Calculate cumulative probabilities using the continuous CDF
    cum <- pshash(x, mu = mu_k, sigma = sigma_k, epsilon = epsilon_k, delta = delta_k)

    # Calculate percentiles
    perc <- cum

    # Convert percentiles to z-scores
    z <- qnorm(perc)
    z[!is.finite(z)] <- 0  # Handle edge cases

    # Calculate norm scores
    norm <- rep(NA, length(z))
    if (!is.na(mScale) && !is.na(sdScale)) {
      norm <- mScale + sdScale * z
    }

    # Create data frame
    df <- data.frame(
      x = x,
      Px = Px,
      Pcum = cum,
      Percentile = perc * 100,
      z = z,
      norm = norm
    )

    # Add confidence intervals if reliability is specified
    if (rel) {
      # True score estimates (regression to the mean correction)
      zPredicted <- reliability * z

      # Confidence intervals
      df$lowerCI <- (zPredicted - se) * sdScale + mScale
      df$upperCI <- (zPredicted + se) * sdScale + mScale
      df$lowerCI_PR <- pnorm(zPredicted - se) * 100
      df$upperCI_PR <- pnorm(zPredicted + se) * 100

      # Ensure percentile ranks are within bounds
      df$lowerCI_PR <- pmax(0, pmin(100, df$lowerCI_PR))
      df$upperCI_PR <- pmax(0, pmin(100, df$upperCI_PR))
    }

    result[[k]] <- df
  }

  # Name the list elements with ages
  names(result) <- as.character(ages)

  return(result)
}

#' Summarize a Sinh-Arcsinh Continuous Norming Model
#'
#' This function provides a summary of a fitted Sinh-Arcsinh continuous norming model,
#' including model fit statistics, convergence information, and parameter estimates.
#'
#' @param object An object of class "cnormShaSh", typically the result of a call to
#'   \code{\link{cnorm.shash}}.
#' @param ... Additional arguments passed to the summary method:
#'   \itemize{
#'      \item age An optional numeric vector of age values corresponding to the raw scores.
#'        If provided along with \code{score}, additional fit statistics (R-squared, RMSE, bias)
#'        will be calculated.
#'      \item score An optional numeric vector of raw scores. Must be provided if \code{age} is given.
#'      \item weights An optional numeric vector of weights for each observation.
#'    }
#'
#' @return Invisibly returns a list containing detailed diagnostic information about the model.
#'   The function primarily produces printed output summarizing the model.
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item Basic model information (polynomial degrees, delta parameter, number of observations)
#'   \item Model fit statistics (log-likelihood, AIC, BIC)
#'   \item R-squared, RMSE, and bias (if age and raw scores are provided)
#'   \item Convergence information
#'   \item Parameter estimates with standard errors, z-values, and p-values
#' }
#'
#' @examples
#' \dontrun{
#' model <- cnorm.shash(children$age, children$score)
#' summary(model)
#'
#' # Including R-squared, RMSE, and bias in the summary:
#' summary(model, age = children$age, score = children$score)
#' }
#'
#' @seealso \code{\link{cnorm.shash}}, \code{\link{diagnostics.shash}}
#' @export
summary.cnormShaSh <- function(object, ...) {
  args <- list(...)

  if ("age" %in% names(args)) { age <- args$age } else {if(length(args)>0) age <- args[[1]] else age <- NULL}
  if ("score" %in% names(args)) { score <- args$score } else {if(length(args)>1) score <- args[[2]] else score <- NULL}
  if ("weights" %in% names(args)) { weights <- args$weights } else {if(length(args)>2) weights <- args[[3]] else weights <- NULL}

  diag <- diagnostics.shash(object, age, score, weights)

  cat("Sinh-Arcsinh Continuous Norming Model\n")
  cat("-------------------------------------\n")
  cat("Polynomial degrees:\n")
  cat("  Location (μ):", diag$mu_degree, "\n")
  cat("  Scale (σ):", diag$sigma_degree, "\n")
  cat("  Skewness (ε):", diag$epsilon_degree, "\n")
  cat("  Tail weight (δ): Fixed at", diag$delta, "\n")
  cat("Number of observations:", diag$n_obs, "\n")
  cat("Number of parameters:", diag$n_params, "\n")
  cat("\n")

  cat("Model Fit:\n")
  cat("  Log-likelihood:", round(diag$log_likelihood, 2), "\n")
  cat("  AIC:", round(diag$AIC, 2), "\n")
  cat("  BIC:", round(diag$BIC, 2), "\n")
  if (!is.na(diag$R2)) {
    cat("  R-squared:", round(diag$R2, 4), "\n")
    cat("  RMSE:", round(diag$rmse, 4), "\n")
    cat("  Bias:", round(diag$bias, 4), "\n")
  }
  cat("\n")

  cat("Convergence:\n")
  cat("  Converged:", diag$converged, "\n")
  cat("  Function evaluations:", diag$n_evaluations, "\n")
  cat("  Max gradient:", round(diag$max_gradient, 6), "\n")
  cat("  Message:", diag$message, "\n")
  cat("\n")

  cat("Parameter Estimates:\n")
  cat("Location (μ) parameters:\n")
  mu_table <- data.frame(
    Estimate = diag$mu_estimates,
    `Std. Error` = diag$mu_se,
    `z value` = diag$mu_z_values,
    `Pr(>|z|)` = diag$mu_p_values
  )
  rownames(mu_table) <- paste0("μ_", 0:(length(diag$mu_estimates)-1))
  print(mu_table, digits = 4)
  cat("\n")

  cat("Scale (σ) parameters:\n")
  sigma_table <- data.frame(
    Estimate = diag$sigma_estimates,
    `Std. Error` = diag$sigma_se,
    `z value` = diag$sigma_z_values,
    `Pr(>|z|)` = diag$sigma_p_values
  )
  rownames(sigma_table) <- paste0("log(σ)_", 0:(length(diag$sigma_estimates)-1))
  print(sigma_table, digits = 4)
  cat("\n")

  cat("Skewness (ε) parameters:\n")
  epsilon_table <- data.frame(
    Estimate = diag$epsilon_estimates,
    `Std. Error` = diag$epsilon_se,
    `z value` = diag$epsilon_z_values,
    `Pr(>|z|)` = diag$epsilon_p_values
  )
  rownames(epsilon_table) <- paste0("ε_", 0:(length(diag$epsilon_estimates)-1))
  print(epsilon_table, digits = 4)

  invisible(diag)
}

#' Diagnostic Statistics for Sinh-Arcsinh Continuous Norming Model
#'
#' This function computes detailed diagnostic statistics for a fitted ShaSh model,
#' including fit statistics, parameter estimates, and convergence information.
#'
#' @param object An object of class "cnormShaSh"
#' @param age An optional numeric vector of age values for computing fit statistics
#' @param score An optional numeric vector of raw scores for computing fit statistics
#' @param weights An optional numeric vector of observation weights
#'
#' @return A list containing comprehensive diagnostic information
#'
#' @keywords internal
diagnostics.shash <- function(object, age = NULL, score = NULL, weights = NULL) {

  if (!inherits(object, "cnormShaSh")) {
    stop("Wrong object. Please provide object from class 'cnormShaSh'.")
  }

  # Basic model information
  n_obs <- attr(object$result, "N")
  n_params <- length(object$result$par)
  log_likelihood <- -object$result$value
  AIC <- 2 * n_params - 2 * log_likelihood
  BIC <- n_params * log(n_obs) - 2 * log_likelihood

  # Parameter estimates and standard errors
  mu_estimates <- object$mu_est
  sigma_estimates <- object$sigma_est
  epsilon_estimates <- object$epsilon_est

  se <- object$se
  if (is.null(se) || any(is.na(se))) {
    mu_se <- rep(NA, length(mu_estimates))
    sigma_se <- rep(NA, length(sigma_estimates))
    epsilon_se <- rep(NA, length(epsilon_estimates))
  } else {
    n_mu <- length(mu_estimates)
    n_sigma <- length(sigma_estimates)

    mu_se <- se[1:n_mu]
    sigma_se <- se[(n_mu + 1):(n_mu + n_sigma)]
    epsilon_se <- se[(n_mu + n_sigma + 1):length(se)]
  }

  # Calculate z-values and p-values
  mu_z_values <- ifelse(is.na(mu_se) | mu_se == 0, NA, mu_estimates / mu_se)
  sigma_z_values <- ifelse(is.na(sigma_se) | sigma_se == 0, NA, sigma_estimates / sigma_se)
  epsilon_z_values <- ifelse(is.na(epsilon_se) | epsilon_se == 0, NA, epsilon_estimates / epsilon_se)

  mu_p_values <- ifelse(is.na(mu_z_values), NA, 2 * (1 - pnorm(abs(mu_z_values))))
  sigma_p_values <- ifelse(is.na(sigma_z_values), NA, 2 * (1 - pnorm(abs(sigma_z_values))))
  epsilon_p_values <- ifelse(is.na(epsilon_z_values), NA, 2 * (1 - pnorm(abs(epsilon_z_values))))

  # Convergence information
  converged <- object$result$convergence == 0
  n_evaluations <- object$result$counts["function"]
  message <- switch(as.character(object$result$convergence),
                    "0" = "Successful convergence",
                    "1" = "Maximum iterations reached",
                    "10" = "Degeneracy in Nelder-Mead simplex",
                    "51" = "Warning from L-BFGS-B",
                    "52" = "Error from L-BFGS-B",
                    paste("Convergence code:", object$result$convergence))

  # Calculate max gradient if available
  max_gradient <- NA
  if (!is.null(object$result$hessian)) {
    tryCatch({
      eigenvals <- eigen(object$result$hessian, only.values = TRUE)$values
      max_gradient <- max(abs(eigenvals), na.rm = TRUE)
    }, error = function(e) {
      max_gradient <- NA
    })
  }

  # Calculate R-squared if age and score data are provided
  R2 <- NA
  rmse <- NA
  bias <- NA
  if (!is.null(age) && !is.null(score)) {
    if (length(age) / length(unique(age)) > 50 && min(table(age)) > 30) {
      data <- data.frame(group = age, raw = score)
      data <- rankByGroup(
        data = data,
        raw = "raw",
        group = "group",
        weights = weights
      )
      norm_scores <- predict(object, data$group, data$raw)
    } else{
      data <- data.frame(age = age, raw = score)
      data$groups <- getGroups(age)
      width <- (max(age) - min(age)) / length(unique(data$groups))
      data <- rankBySlidingWindow(
        data,
        age = "age",
        raw = "raw",
        width = width,
        weights = weights
      )
      norm_scores <- predict(object, data$age, data$raw)
    }

    norm_manifest <- data$normValue
    R2 <- cor(norm_scores, norm_manifest, use="pairwise.complete.obs") ^ 2
    rmse <- sqrt(mean((norm_scores - norm_manifest) ^ 2))
    bias <- mean(norm_scores - norm_manifest)
  } else {
    message <- "No age and raw scores provided. Cannot calculate R2, RMSE, and bias."
  }

  # Return comprehensive diagnostic information
  list(
    # Basic model info
    mu_degree = object$mu_degree,
    sigma_degree = object$sigma_degree,
    epsilon_degree = object$epsilon_degree,
    delta = object$delta,
    n_obs = n_obs,
    n_params = n_params,

    # Fit statistics
    log_likelihood = log_likelihood,
    AIC = AIC,
    BIC = BIC,
    R2 = R2,
    rmse = rmse,
    bias = bias,

    # Parameter estimates
    mu_estimates = mu_estimates,
    sigma_estimates = sigma_estimates,
    epsilon_estimates = epsilon_estimates,

    # Standard errors
    mu_se = mu_se,
    sigma_se = sigma_se,
    epsilon_se = epsilon_se,

    # Test statistics
    mu_z_values = mu_z_values,
    sigma_z_values = sigma_z_values,
    epsilon_z_values = epsilon_z_values,

    mu_p_values = mu_p_values,
    sigma_p_values = sigma_p_values,
    epsilon_p_values = epsilon_p_values,

    # Convergence info
    converged = converged,
    n_evaluations = n_evaluations,
    max_gradient = max_gradient,
    message = message
  )
}

#' Predict Norm Scores from Raw Scores
#'
#' This function calculates norm scores based on raw scores, age, and a fitted cnormShaSh model.
#'
#' @param object A fitted model object of class 'cnormShaSh'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as score.
#'      \item score A numeric vector of raw scores.
#'      \item range The range of the norm scores in standard deviations. Default is 3.
#'        Thus, scores in the range of +/- 3 standard deviations are considered.
#'    }
#'
#' @return A numeric vector of norm scores.
#'
#' @details
#' The function predicts the ShaSh distribution parameters (μ, σ, ε, δ) for each age
#' using the provided model. It then calculates the cumulative probability for each
#' raw score given these parameters using the continuous ShaSh distribution. Finally,
#' it converts these probabilities to the norm scale specified in the model.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted model named 'shash_model':
#' model <- cnorm.shash(children$age, children$score)
#' raw_scores <- c(25.5, 30.2, 18.7, 45.3)
#' ages <- c(7, 8, 9, 10)
#' norm_scores <- predict(model, ages, raw_scores)
#' }
#'
#' @export
#' @family predict
predict.cnormShaSh <- function(object, ...) {

  model <- object
  args <- list(...)

  if ("age" %in% names(args)) { age <- args$age } else {if(length(args)>0) age <- args[[1]] else age <- NULL}
  if ("score" %in% names(args)) { score <- args$score } else {if(length(args)>1) score <- args[[2]] else score <- NULL}
  if ("range" %in% names(args)) { range <- args$range } else { if(length(args)>2) range <- args[[3]] else range <- 3 }

  if (!inherits(model, "cnormShaSh")) {
    stop("Wrong object. Please provide object from class 'cnormShaSh'.")
  }

  if (is.null(age) || is.null(score)) {
    stop("Both 'age' and 'score' must be provided.")
  }

  if (length(age) != length(score)) {
    stop("The lengths of 'age' and 'score' must be the same.")
  }

  # Get predicted distribution parameters for each age
  predictions <- predictCoefficients_shash(model, age)

  mu <- predictions$mu
  sigma <- predictions$sigma
  epsilon <- predictions$epsilon
  delta <- predictions$delta

  # Calculate cumulative probabilities (percentiles) for each score
  percentiles <- numeric(length(age))

  for (i in 1:length(age)) {
    # Calculate cumulative probability using ShaSh CDF
    percentiles[i] <- pshash(score[i], mu = mu[i], sigma = sigma[i],
                             epsilon = epsilon[i], delta = delta[i])
  }

  # Convert percentiles to z-scores
  z_scores <- qnorm(percentiles)

  # Handle edge cases and apply range constraints
  z_scores[!is.finite(z_scores)] <- 0
  z_scores[z_scores < -range] <- -range
  z_scores[z_scores > range] <- range

  # Get scale information
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  if (!is.na(mScale) && !is.na(sdScale)) {
    # Scale z-scores to specified norm scale
    norm_scores <- mScale + sdScale * z_scores
    return(norm_scores)
  } else {
    # Return percentile ranks if no scale specified
    return(percentiles * 100)
  }
}
