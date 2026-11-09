#' Fit a Normal (Gaussian) Regression Model for Continuous Norming
#'
#' This function fits a parametric two-parameter Normal (Gaussian) regression model for
#' continuous norm score modeling, where the location parameter \eqn{\mu} (mean) and scale
#' parameter \eqn{\sigma} (standard deviation) vary smoothly as polynomial functions of age
#' or other continuous predictors. It provides a parsimonious parametric baseline when raw
#' scores within age groups are approximately normally distributed (symmetric and mesokurtic)
#' and free from pronounced floor or ceiling effects.
#'
#' @param age A numeric vector of predictor values (typically age, but can be any continuous predictor).
#' @param score A numeric vector of response values (raw test scores). Must be the same length as age.
#'   The value range is unrestricted and can include zeros and negative values.
#' @param weights An optional numeric vector of weights for each observation.
#'   Useful for incorporating sampling or post-stratification weights. If NULL (default),
#'   all observations are weighted equally.
#' @param mu_degree Integer specifying the degree of the polynomial for modeling the location
#'   parameter \code{mu(age)}. Default is 3. Higher degrees allow more flexible modeling of how
#'   the mean changes with age, but may lead to overfitting with small samples. Common choices:
#'   \itemize{
#'     \item 0: Constant mean across age (intercept only)
#'     \item 1: Linear change with age
#'     \item 2: Quadratic change (allows one inflection point)
#'     \item 3: Cubic change (allows two inflection points, suitable for most developmental curves)
#'     \item 4+: Higher-order changes (use cautiously, mainly for large samples)
#'   }
#' @param sigma_degree Integer (or NULL) specifying the degree of the polynomial for modeling the
#'   scale parameter \code{log(sigma(age))}. Default is 2. This controls how the variability (spread)
#'   of scores changes with age. Set \code{sigma_degree = 0} (or \code{NULL}) to fit a homoscedastic
#'   model with a constant (but estimated) standard deviation across all ages.
#' @param control An optional list of control parameters passed to the \code{optim} function for
#'   maximum likelihood estimation. Any parameters not specified are filled with sensible defaults
#'   based on the model complexity. Common parameters to adjust:
#'   \itemize{
#'     \item \code{factr}: Controls precision of optimization (default: 1e-8)
#'     \item \code{maxit}: Maximum number of iterations (default: n_parameters * 200)
#'     \item \code{lmm}: Memory limit for L-BFGS-B (default: min(n_parameters, 20))
#'   }
#'   Increase \code{maxit} or decrease \code{factr} if optimization fails to converge.
#' @param scale Character string or numeric vector specifying the type of norm scale for output.
#'   This affects the scaling of derived norm scores but does not influence model fitting:
#'   \itemize{
#'     \item "T": T-scores (mean = 50, SD = 10) - default
#'     \item "IQ": IQ-like scores (mean = 100, SD = 15)
#'     \item "z": z-scores (mean = 0, SD = 1)
#'     \item c(M, SD): Custom scale with specified mean M and standard deviation SD
#'   }
#' @param plot Logical indicating whether to automatically display a diagnostic plot of the fitted model.
#'   Default is TRUE.
#'
#' @return An object of class "cnormNormal" containing the fitted model results. This is a list with components:
#'   \item{mu_est}{Numeric vector of estimated coefficients for the location parameter \code{mu(age)}.
#'     The first coefficient is the intercept, subsequent coefficients correspond to polynomial terms.}
#'   \item{sigma_est}{Numeric vector of estimated coefficients for the log-scale parameter \code{log(sigma(age))}.
#'     Note: These are coefficients for \code{log(sigma)} to ensure \code{sigma > 0}.}
#'   \item{se}{Numeric vector of standard errors for all estimated coefficients (if Hessian computation succeeds).}
#'   \item{mu_degree, sigma_degree}{The polynomial degrees used for each parameter.}
#'   \item{result}{Complete output from the \code{optim} function, including convergence information,
#'     log-likelihood value, and other optimization details.}
#'
#' @details
#' Parameters are estimated simultaneously using maximum likelihood via the L-BFGS-B algorithm
#' with exact analytic gradients.
#'
#' \subsection{The Normal Regression Model}{
#' Conditional on age \eqn{a}, raw scores are assumed to follow a normal distribution:
#' \deqn{X \mid a \sim \mathcal{N}(\mu(a), \sigma^2(a))}
#' where \eqn{\mu(a) = \sum_{k=0}^{d_\mu} \beta_{\mu, k} a_{\text{std}}^k} and
#' \eqn{\log(\sigma(a)) = \sum_{k=0}^{d_\sigma} \beta_{\sigma, k} a_{\text{std}}^k}.
#' Note that this model corresponds to the special case of the Sinh-Arcsinh (\code{\link{cnorm.shash}})
#' model with skewness fixed at \eqn{\epsilon = 0} and tail weight fixed at \eqn{\delta = 1}.
#' Because both models use continuous densities on the same support, their log-likelihood,
#' AIC, and BIC values are directly comparable.
#' }
#'
#' \subsection{Model Selection}{
#' Choose polynomial degrees based on:
#' \itemize{
#'   \item Sample size (higher degrees need more data)
#'   \item Theoretical expectations about developmental trajectories
#'   \item Model comparison criteria (AIC, BIC) via \code{\link{autoselect.normal}}
#'   \item Visual inspection of fitted percentile curves
#' }
#' If the data exhibit skewness, floor/ceiling effects, or heavy/light tails, consider using
#' \code{\link{cnorm.shash}}, \code{\link{cnorm.betabinomial}}, or distribution-free Taylor
#' polynomials (\code{\link{cnorm}}) instead.
#' }
#'
#' @note
#' \itemize{
#'   \item The function requires the input data to have sufficient variability. Very small datasets
#'     or datasets with little age spread may cause convergence problems.
#'   \item Polynomial models can exhibit edge effects at the boundaries of the age range.
#'     Predictions outside the observed age range should be made cautiously.
#'   \item If parameters end up at their box constraints, the reported standard errors and
#'     Wald tests for these parameters are not valid; a warning is issued in this case.
#' }
#'
#' @seealso
#' \code{\link{autoselect.normal}} for automatic degree selection via BIC,
#' \code{\link{plot.cnormNormal}} for plotting fitted models,
#' \code{\link{predict.cnormNormal}} for generating predictions,
#' \code{\link{normTable.normal}} for generating norm tables,
#' \code{\link{cnorm.shash}} for skewed or non-mesokurtic continuous distributions
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings (cubic mu, quadratic log-sigma)
#' model <- cnorm.normal(age = children$age, score = children$raw_score)
#'
#' # Homoscedastic model (constant standard deviation across age)
#' model_homo <- cnorm.normal(
#'   age = children$age,
#'   score = children$raw_score,
#'   mu_degree = 3,
#'   sigma_degree = 0
#' )
#'
#' # With sampling weights
#' model_weighted <- cnorm.normal(
#'   age = survey$age,
#'   score = survey$score,
#'   weights = survey$sample_weight
#' )
#'
#' # Compare with a 4-parameter shash model
#' model_shash <- cnorm.shash(age = children$age, score = children$raw_score)
#' compare(model, model_shash, age = children$age, score = children$raw_score)
#' }
#'
#' @author Wolfgang Lenhard
#' @export
cnorm.normal <- function(age,
                         score,
                         weights = NULL,
                         mu_degree = 3,
                         sigma_degree = 2,
                         control = NULL,
                         scale = "T",
                         plot = TRUE) {
  # Input validation
  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!is.null(weights) && length(age) != length(weights)) {
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  }

  if (is.null(mu_degree) || !is.numeric(mu_degree) || length(mu_degree) != 1 || mu_degree < 0) {
    stop("'mu_degree' must be a non-negative integer.")
  }
  mu_degree <- as.integer(mu_degree)

  if (is.null(sigma_degree)) {
    sigma_degree <- 0L
  } else if (!is.numeric(sigma_degree) || length(sigma_degree) != 1 || sigma_degree < 0) {
    stop("'sigma_degree' must be a non-negative integer or NULL.")
  } else {
    sigma_degree <- as.integer(sigma_degree)
  }

  # Validate 'scale' early with explicit errors. Accepts integer vectors as well.
  if (is.numeric(scale) && length(scale) == 2) {
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (is.character(scale) && length(scale) == 1) {
    scaleM <- switch(scale,
                     "T" = 50,
                     "IQ" = 100,
                     "z" = 0,
                     stop("Unknown scale '", scale,
                          "'. Use 'T', 'IQ', 'z', or a numeric vector c(M, SD)."))
    scaleSD <- switch(scale, "T" = 10, "IQ" = 15, "z" = 1)
  } else {
    stop("'scale' must be 'T', 'IQ', 'z', or a numeric vector c(M, SD).")
  }

  # Prepare vectors
  vectors_to_check <- list(age = age, score = score)
  if (!is.null(weights)) {
    vectors_to_check$weights <- weights
  }

  # Check if filtering needed
  needs_filtering <- any(sapply(vectors_to_check, function(x)
    any(!is.finite(x))))

  if (needs_filtering) {
    message("Vector(s) contained non-finite values (NA, NaN, Inf). These cases will be removed.")
    tmp <- do.call(filter_complete, c(vectors_to_check, verbose = FALSE))
    age <- tmp[[1]]
    score <- tmp[[2]]
    if (!is.null(weights))
      weights <- tmp[[3]]
  }

  # Standardize age
  age_std <- standardize(age)

  # Create design matrices (supporting degree 0 for intercept-only terms)
  X_mu <- if (mu_degree == 0L) {
    matrix(1, nrow = length(age_std), ncol = 1)
  } else {
    cbind(1, poly(age_std, degree = mu_degree, raw = TRUE))
  }

  X_sigma <- if (sigma_degree == 0L) {
    matrix(1, nrow = length(age_std), ncol = 1)
  } else {
    cbind(1, poly(age_std, degree = sigma_degree, raw = TRUE))
  }

  # Calculate initial parameter values
  initial_mu <- mean(score)
  initial_sigma <- sd(score)

  # Guard against degenerate input (zero/undefined variance)
  if (!is.finite(initial_sigma) || initial_sigma <= 0) {
    stop("'score' has zero or undefined variance. The model cannot be fitted.")
  }

  # Initial parameter vectors
  initial_params <- c(
    c(initial_mu, rep(0, mu_degree)),            # mu parameters
    c(log(initial_sigma), rep(0, sigma_degree))  # log(sigma) parameters
  )

  n_param <- length(initial_params)
  control_defaults <- list(factr = 1e-8,
                           maxit = n_param * 200,
                           lmm = min(n_param, 20))
  control <- if (is.null(control))
    control_defaults
  else
    utils::modifyList(control_defaults, control)

  # Parameter bounds
  n_params <- length(initial_params)
  lower_bounds <- rep(-Inf, n_params)
  upper_bounds <- rep(Inf, n_params)

  # Bounds for sigma parameters (keep log_sigma reasonable)
  sigma_start <- ncol(X_mu) + 1
  sigma_end <- sigma_start + ncol(X_sigma) - 1
  lower_bounds[sigma_start:sigma_end] <- -10
  upper_bounds[sigma_start:sigma_end] <- 10

  # Optimization (with analytic gradient)
  result <- tryCatch({
    optim(
      initial_params,
      log_likelihood_normal,
      gr = gradient_normal,
      X_mu = X_mu,
      X_sigma = X_sigma,
      y = score,
      weights = weights,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      hessian = TRUE,
      control = control
    )
  }, error = function(e) {
    message("First optimization attempt failed. Trying with different parameters...")

    # Try with different, more robust initial values
    initial_params[1] <- median(score)

    # mad(score) can be 0 with strong floor/ceiling effects
    # (> 50% identical scores); fall back to sd in that case.
    s_robust <- mad(score)
    if (!is.finite(s_robust) || s_robust <= 0)
      s_robust <- sd(score)
    initial_params[ncol(X_mu) + 1] <- log(s_robust)

    # Guard against missing list entries when mutating a
    # user-supplied control list (NULL * 10 == numeric(0)).
    control$factr <- if (is.null(control$factr)) 1e-7 else control$factr * 10
    control$maxit <- if (is.null(control$maxit)) 1000 else control$maxit * 2

    optim(
      initial_params,
      log_likelihood_normal,
      gr = gradient_normal,
      X_mu = X_mu,
      X_sigma = X_sigma,
      y = score,
      weights = weights,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      hessian = TRUE,
      control = control
    )
  })

  # Check convergence
  if (result$convergence != 0) {
    warning(
      "Optimization did not converge (code: ",
      result$convergence,
      "). Consider adjusting control parameters or degrees of the polynomials. Check percentile curves for plausibility."
    )
  }

  # Warn when parameters sit at their box constraints; SEs and Wald
  # tests are invalid there.
  at_bound <- is.finite(lower_bounds) &
    (result$par <= lower_bounds + 1e-6 | result$par >= upper_bounds - 1e-6)
  if (any(at_bound)) {
    warning(
      "Parameter(s) at index ",
      paste(which(at_bound), collapse = ", "),
      " reached their bounds. Standard errors and Wald tests for these ",
      "parameters are not valid. Consider reducing model complexity."
    )
  }

  # Extract parameter estimates
  n_mu <- ncol(X_mu)
  n_sigma <- ncol(X_sigma)

  mu_est <- result$par[1:n_mu]
  sigma_est <- result$par[(n_mu + 1):(n_mu + n_sigma)]

  # Guarded SE computation. solve() on an indefinite Hessian could otherwise
  # either error or silently produce NaN via sqrt of negative diagonals.
  se <- tryCatch({
    h_inv <- solve(result$hessian)
    d <- diag(h_inv)
    if (any(d < 0, na.rm = TRUE)) {
      warning("Hessian is not positive definite; standard errors are set to NA where invalid.")
      d[d < 0] <- NA
    }
    sqrt(d)
  }, error = function(e) {
    warning("Could not compute standard errors: Hessian matrix issue")
    rep(NA_real_, length(result$par))
  })

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

  # Create model object
  model <- list(
    mu_est = mu_est,
    sigma_est = sigma_est,
    se = se,
    mu_degree = mu_degree,
    sigma_degree = sigma_degree,
    result = result
  )

  class(model) <- "cnormNormal"

  if (plot) {
    p <- plot.cnormNormal(model, age, score, weights = weights)
    print(p)
  }

  return(model)
}



#' Calculate the negative log-likelihood for a normal regression model
#'
#' This function computes the negative log-likelihood for a two-parameter Normal
#' regression model where the location (\code{mu}) and log-scale (\code{log(sigma)})
#' parameters are modeled as polynomial functions of predictors.
#'
#' @param params A numeric vector containing all model parameters
#' @param X_mu Design matrix for location parameter
#' @param X_sigma Design matrix for scale parameter
#' @param y Response vector
#' @param weights Observation weights
#'
#' @return The negative log-likelihood of the model
#'
#' @keywords internal
log_likelihood_normal <- function(params,
                                  X_mu,
                                  X_sigma,
                                  y,
                                  weights = NULL) {
  n_mu <- ncol(X_mu)
  n_sigma <- ncol(X_sigma)

  # Extract coefficients
  mu_coef <- params[seq_len(n_mu)]
  sigma_coef <- params[n_mu + seq_len(n_sigma)]

  # Compute parameters with efficient matrix operations
  mu <- drop(X_mu %*% mu_coef)
  log_sigma <- pmin.int(pmax.int(drop(X_sigma %*% sigma_coef), -10), 10)
  sigma <- exp(log_sigma)

  if (is.null(weights)) {
    weights <- 1  # Will broadcast in multiplication
  }

  z <- (y - mu) / sigma
  log_densities <- -log_sigma - 0.918938533204673 - 0.5 * z * z  # 0.918938533204673 = 0.5 * log(2 * pi)

  bad <- !is.finite(log_densities)
  if (any(bad)) {
    if (anyNA(log_densities))
      return(1e10)
    log_densities[bad] <- -1e6
  }

  loglik <- sum(weights * log_densities)

  if (!is.finite(loglik))
    return(1e10)

  return(-loglik)
}


#' Analytic gradient of the negative log-likelihood for a normal regression model
#'
#' Computes the exact gradient of \code{\link{log_likelihood_normal}} with
#' respect to all regression coefficients. Supplying this gradient to
#' \code{\link[stats]{optim}} (method \code{"L-BFGS-B"}) avoids finite-difference
#' approximations, yielding faster and more reliable convergence.
#'
#' @details
#' With \eqn{z = (y - \mu)/\sigma}, the per-observation log-density is
#' \deqn{\ell = -\log\sigma - \tfrac{1}{2}\log(2\pi) - \tfrac{1}{2}z^2.}
#' The partial derivatives with respect to the linear predictors are:
#' \deqn{\partial\ell/\partial\mu = z / \sigma}
#' \deqn{\partial\ell/\partial\log\sigma = z^2 - 1}
#' Coefficient gradients follow as \eqn{-X^\top (w \cdot \partial\ell/\partial\eta)}
#' for the negative log-likelihood. Contributions of observations whose linear
#' predictor \eqn{\log\sigma} is clamped to \eqn{[-10, 10]} are zeroed, which is
#' the exact subgradient of the clamped objective.
#'
#' @inheritParams log_likelihood_normal
#'
#' @return Numeric vector: gradient of the negative log-likelihood with
#'   respect to \code{params}.
#'
#' @keywords internal
gradient_normal <- function(params,
                            X_mu,
                            X_sigma,
                            y,
                            weights = NULL) {
  n_mu <- ncol(X_mu)
  n_sigma <- ncol(X_sigma)

  # Extract coefficients
  mu_coef <- params[seq_len(n_mu)]
  sigma_coef <- params[n_mu + seq_len(n_sigma)]

  # Linear predictors with clamping identical to log_likelihood_normal;
  # indicator vector records where the clamp is inactive (gradient nonzero)
  mu <- drop(X_mu %*% mu_coef)

  eta_sigma <- drop(X_sigma %*% sigma_coef)
  sigma_free <- (eta_sigma > -10) & (eta_sigma < 10)
  sigma <- exp(pmin.int(pmax.int(eta_sigma, -10), 10))

  if (is.null(weights)) {
    weights <- 1
  }

  z <- (y - mu) / sigma

  d_mu   <- z / sigma      # dl/dmu
  d_lsig <- z * z - 1      # dl/d(log sigma)

  # Zero out contributions from numerically failed observations
  ok <- is.finite(d_mu) & is.finite(d_lsig)
  d_mu[!ok] <- 0
  d_lsig[!ok] <- 0

  # Apply clamp subgradient
  d_lsig <- d_lsig * sigma_free

  # Gradient of the NEGATIVE weighted log-likelihood w.r.t. coefficients
  w <- weights
  grad <- c(
    -drop(crossprod(X_mu, w * d_mu)),
    -drop(crossprod(X_sigma, w * d_lsig))
  )

  # Final safety: L-BFGS-B cannot handle non-finite gradients
  grad[!is.finite(grad)] <- 0

  return(grad)
}


#' Predict parameters for a normal regression model
#'
#' @param model An object of class "cnormNormal"
#' @param ages A numeric vector of age points for prediction
#'
#' @return A data frame with predicted parameters (\code{age}, \code{mu}, \code{sigma})
#'
#' @keywords internal
predictCoefficients_normal <- function(model, ages) {
  if (!isNormal(model)) {
    stop("Wrong object. Please provide object from class 'cnormNormal'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  # Create design matrices
  X_mu_new <- if (model$mu_degree == 0L) {
    matrix(1, nrow = length(ages_std), ncol = 1)
  } else {
    cbind(1, poly(ages_std, degree = model$mu_degree, raw = TRUE))
  }

  X_sigma_new <- if (is.null(model$sigma_degree) || model$sigma_degree == 0L) {
    matrix(1, nrow = length(ages_std), ncol = 1)
  } else {
    cbind(1, poly(ages_std, degree = model$sigma_degree, raw = TRUE))
  }

  # Predict parameters
  mu <- X_mu_new %*% model$mu_est
  log_sigma <- X_sigma_new %*% model$sigma_est
  sigma <- exp(log_sigma)

  predicted <- data.frame(
    age = ages,
    mu = as.vector(mu),
    sigma = as.vector(sigma)
  )

  return(predicted)
}


#' Plot Normal Regression Model with Data and Percentile Lines
#'
#' @param x A fitted model object of class "cnormNormal"
#' @param ... Additional arguments including age, score, weights, percentiles, points
#'
#' @return A ggplot object
#'
#' @export
plot.cnormNormal <- function(x, ...) {
  model <- x
  args <- list(...)

  if ("age" %in% names(args)) {
    age <- args$age
  } else {
    if (length(args) > 0)
      age <- args[[1]]
    else
      age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else {
    if (length(args) > 1)
      score <- args[[2]]
    else
      score <- NULL
  }
  if ("weights" %in% names(args)) {
    weights <- args$weights
  } else {
    weights <- NULL
  }
  if ("percentiles" %in% names(args)) {
    percentiles <- args$percentiles
  } else {
    percentiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  }
  if ("points" %in% names(args)) {
    points <- args$points
  } else {
    points <- TRUE
  }

  if (is.null(age) || is.null(score))
    stop("Please provide 'age' and 'score' vectors.")

  if (!isNormal(model)) {
    stop("Wrong object. Please provide object from class 'cnormNormal'.")
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
  preds <- predictCoefficients_normal(model, pred_ages)

  # Calculate percentile lines using normal quantiles
  percentile_lines <- lapply(percentiles, function(p) {
    qnorm(
      p,
      mean = preds$mu,
      sd = preds$sigma
    )
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    mu = preds$mu,
    sigma = preds$sigma,
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
  if (length(age) / length(unique(age)) > 50 &&
      min(table(data$age)) > 30) {
    data$group <- age
  } else {
    data$group <- getGroups(age)
  }

  # Limit to max 30 groups for better visibility
  if (length(unique(data$group)) > 30) {
    data$group <- getGroups(age, n = 30)
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
        linewidth = 0.6
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
      title = "Percentile Plot (Normal Model)",
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
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
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


#' Print method for Normal model objects
#'
#' @param x A cnormNormal object
#' @param ... Additional arguments
#' @export
print.cnormNormal <- function(x, ...) {
  cat("Normal Regression Model for Continuous Norming\n")
  cat("==============================================\n\n")

  cat("Model Parameters:\n")
  cat("- Location (mu) polynomial degree:", x$mu_degree, "\n")
  if (is.null(x$sigma_degree) || x$sigma_degree == 0L) {
    cat("- Scale (sigma): Constant across age (degree 0)\n\n")
  } else {
    cat("- Scale (sigma) polynomial degree:", x$sigma_degree, "\n\n")
  }

  cat("Sample size:", attr(x$result, "N"), "\n")
  cat("Age range:",
      attr(x$result, "ageMin"),
      "to",
      attr(x$result, "ageMax"),
      "\n")
  cat("Score range:",
      attr(x$result, "min"),
      "to",
      attr(x$result, "max"),
      "\n\n")

  cat("Optimization:\n")
  cat("- Convergence:",
      ifelse(x$result$convergence == 0, "Successful", "Failed"),
      "\n")
  cat("- Log-likelihood:", -x$result$value, "\n")
  cat("- AIC:", 2 * length(x$result$par) + 2 * x$result$value, "\n")
}


#' Automatic model selection for Normal continuous norming via BIC
#'
#' Selects the polynomial degrees for the two components of a
#' \code{\link{cnorm.normal}} model (\eqn{\mu} and \eqn{\sigma}) by minimizing
#' BIC over a full grid.
#'
#' A degree of \code{0} for \eqn{\sigma} represents a homoscedastic model with
#' a constant (age-invariant) standard deviation estimated from the data.
#'
#' Parallel execution is attempted by default. If the workers cannot access
#' the \pkg{cNORM} namespace, the function transparently falls back to
#' sequential execution.
#'
#' @param age,score Numeric vectors of predictor and response values.
#' @param weights Optional numeric vector of observation weights.
#' @param max_mu,max_sigma Maximum polynomial degrees for the location (\eqn{\mu})
#'   and scale (\eqn{\sigma}) parameters. Defaults: \code{5, 3}.
#' @param min_mu Minimum polynomial degree for \eqn{\mu} (default 1).
#' @param min_sigma Minimum polynomial degree for \eqn{\sigma}. \code{0} (default)
#'   includes the homoscedastic (constant \eqn{\sigma}) model in the search.
#' @param control Optional control list passed to \code{\link[stats]{optim}}.
#' @param scale Norm scale (default \code{"T"}).
#' @param parallel Logical; attempt parallel execution. Default \code{TRUE}.
#' @param n_cores Number of cores. Defaults to all logical cores.
#' @param plot Logical; plot the selected model. Default \code{TRUE}.
#' @param verbose Logical; print progress. Default \code{TRUE}.
#'
#' @return The selected fitted \code{cnormNormal} model with an additional
#'   element \code{$selection} containing:
#'   \itemize{
#'     \item \code{evaluated}: data frame of every combination tried, sorted by BIC.
#'     \item \code{selected}: list with the chosen degrees and BIC.
#'   }
#'
#' @examples
#' \dontrun{
#' m <- autoselect.normal(elfe$group, elfe$raw)
#' m$selection$evaluated
#' summary(m, age = elfe$group, score = elfe$raw)
#' }
#'
#' @seealso \code{\link{cnorm.normal}}, \code{\link{autoselect.shash}}, \code{\link{autoselect.betabinomial}}
#' @export
autoselect.normal <- function(age,
                              score,
                              weights   = NULL,
                              max_mu    = 5,
                              max_sigma = 3,
                              min_mu    = 1,
                              min_sigma = 0,
                              control   = NULL,
                              scale     = "T",
                              parallel  = TRUE,
                              n_cores   = NULL,
                              plot      = TRUE,
                              verbose   = TRUE) {

  # ---- Input validation -------------------------------------------------
  if (length(age) != length(score))
    stop("Length of 'age' and 'score' must be the same.")
  if (!is.null(weights) && length(weights) != length(age))
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  if (max_mu    < min_mu)    stop("'max_mu' must be >= 'min_mu'.")
  if (max_sigma < min_sigma) stop("'max_sigma' must be >= 'min_sigma'.")
  if (min_mu < 0)
    stop("'min_mu' must be >= 0.")
  if (min_sigma < 0)
    stop("'min_sigma' must be >= 0 (0 means constant sigma).")

  say <- function(...) {
    if (verbose) { cat(..., sep = ""); utils::flush.console() }
  }

  # ---- Build the 2-D grid ----------------------------------------------
  grid <- expand.grid(mu    = min_mu:max_mu,
                      sigma = min_sigma:max_sigma,
                      KEEP.OUT.ATTRS = FALSE)
  pairs <- lapply(seq_len(nrow(grid)),
                  function(i) c(grid$mu[i], grid$sigma[i]))
  say(sprintf(
    "Search grid: %d candidate models (mu %d:%d, sigma %d:%d).\n",
    length(pairs),
    min_mu, max_mu, min_sigma, max_sigma
  ))

  # ---- Parallel setup with dev-mode fallback ---------------------------
  use_parallel <- FALSE
  cl <- NULL
  # If cNORM is currently shimmed by pkgload/devtools::load_all(), skip PSOCK
  # workers so we always test the live in-memory code rather than a stale disk install.
  is_dev_loaded <- "devtools_shims" %in% search() ||
    exists(".__DEVTOOLS__", envir = asNamespace("cNORM"))

  if (isTRUE(parallel) && !is_dev_loaded) {
    avail <- tryCatch(parallel::detectCores(logical = TRUE),
                      error = function(e) 1L)
    if (is.null(n_cores)) n_cores <- avail
    n_cores <- max(1L, min(n_cores, length(pairs), avail))

    if (n_cores > 1L && length(pairs) > 1L) {
      cl <- tryCatch(parallel::makeCluster(n_cores),
                     error = function(e) NULL)
      if (!is.null(cl)) {
        worker_ok <- tryCatch({
          res <- parallel::clusterCall(cl, function() {
            requireNamespace("cNORM", quietly = TRUE) &&
              exists("cnorm.normal",       envir = asNamespace("cNORM")) &&
              exists("diagnostics.normal", envir = asNamespace("cNORM"))
          })
          all(vapply(res, isTRUE, logical(1)))
        }, error = function(e) FALSE)

        if (worker_ok) {
          use_parallel <- TRUE
          on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
          parallel::clusterExport(
            cl,
            varlist = c("age", "score", "weights",
                        "control", "scale"),
            envir = environment()
          )
          say(sprintf("Parallel mode: using %d cores.\n", n_cores))
        } else {
          try(parallel::stopCluster(cl), silent = TRUE); cl <- NULL
          say("Note: cNORM is not installed in the worker library path. ",
              "Falling back to sequential execution.\n")
        }
      }
    }
  } else if (isTRUE(parallel) && is_dev_loaded) {
    say("Note: cNORM is loaded via devtools::load_all(); ",
        "running sequentially to use in-memory code.\n")
  }

  # ---- Worker ----------------------------------------------------------
  fit_worker <- function(p) {
    # Resolve via asNamespace("cNORM") or enclosing environment so this
    # works even before NAMESPACE exports are generated by devtools::document()
    cnorm_fn <- if (exists("cnorm.normal", mode = "function")) {
      get("cnorm.normal", mode = "function")
    } else {
      get("cnorm.normal", envir = asNamespace("cNORM"))
    }
    diag_fn <- if (exists("diagnostics.normal", mode = "function")) {
      get("diagnostics.normal", mode = "function")
    } else {
      get("diagnostics.normal", envir = asNamespace("cNORM"))
    }

    tryCatch({
      m <- suppressMessages(suppressWarnings(
        cnorm_fn(
          age          = age,
          score        = score,
          weights      = weights,
          mu_degree    = p[1],
          sigma_degree = p[2],
          control      = control,
          scale        = scale,
          plot         = FALSE
        )
      ))
      d <- diag_fn(m)
      list(mu = p[1], sigma = p[2],
           model = m,
           BIC = if (is.finite(d$BIC)) d$BIC else Inf,
           AIC = d$AIC, logLik = d$log_likelihood,
           converged = isTRUE(d$converged),
           status  = if (!is.finite(d$BIC)) "error"
           else if (!isTRUE(d$converged)) "not_converged"
           else "ok",
           message = NA_character_)
    }, error = function(e) {
      list(mu = p[1], sigma = p[2],
           model = NULL, BIC = Inf, AIC = NA_real_, logLik = NA_real_,
           converged = FALSE, status = "error",
           message = conditionMessage(e))
    })
  }

  # ---- Cache + reporting -----------------------------------------------
  cache <- new.env(hash = TRUE, parent = emptyenv())
  ck <- function(p) sprintf("%d_%d", p[1], p[2])

  fmt_sigma <- function(s) if (s == 0L) "0 (const)" else as.character(s)

  report <- function(r) {
    tag <- switch(r$status,
                  ok            = "",
                  not_converged = "  (not strictly converged)",
                  error         = paste0("  [error: ", r$message, "]"))
    say(sprintf("  mu=%d, sigma=%s : BIC = %s%s\n",
                r$mu, fmt_sigma(r$sigma),
                formatC(r$BIC, digits = 3, format = "f"), tag))
  }

  evaluate_pairs <- function(pairs) {
    todo <- pairs[!vapply(pairs,
                          function(p) exists(ck(p), envir = cache),
                          logical(1))]
    if (length(todo) == 0L) return(invisible(NULL))

    say(sprintf("Evaluating %d model%s ...\n",
                length(todo), if (length(todo) == 1) "" else "s"))

    if (use_parallel && length(todo) > 1L) {
      chunks <- split(todo, ceiling(seq_along(todo) / n_cores))
      for (chunk in chunks) {
        results <- parallel::parLapply(cl, chunk, fit_worker)
        for (r in results) {
          assign(ck(c(r$mu, r$sigma)), r, envir = cache)
          report(r)
        }
      }
    } else {
      for (p in todo) {
        r <- fit_worker(p)
        assign(ck(c(r$mu, r$sigma)), r, envir = cache)
        report(r)
      }
    }
    invisible(NULL)
  }

  # ---- Run the search --------------------------------------------------
  evaluate_pairs(pairs)
  all_res <- mget(ls(envir = cache), envir = cache)
  bics    <- vapply(all_res, `[[`, numeric(1), "BIC")
  if (all(!is.finite(bics)))
    stop("Selection failed: no model produced a finite BIC. ",
         "Inspect $selection$evaluated for per-fit messages.")
  current <- all_res[[which.min(bics)]]

  # ---- Compile results -------------------------------------------------
  evaluated <- do.call(rbind, lapply(
    all_res,
    function(r) data.frame(mu = r$mu, sigma = r$sigma,
                           BIC = r$BIC, AIC = r$AIC,
                           logLik = r$logLik,
                           converged = r$converged,
                           status = r$status,
                           message = r$message,
                           stringsAsFactors = FALSE)))
  evaluated <- evaluated[order(evaluated$BIC), , drop = FALSE]
  rownames(evaluated) <- NULL

  say(sprintf("\nSelected model: mu=%d, sigma=%s (BIC = %.3f)\n",
              current$mu, fmt_sigma(current$sigma), current$BIC))

  final_model <- current$model
  if (is.null(final_model))
    stop("Selection failed: the best candidate did not produce a usable model.")

  final_model$selection <- list(
    evaluated = evaluated,
    selected  = list(mu_degree    = current$mu,
                     sigma_degree = current$sigma,
                     BIC          = current$BIC)
  )

  if (plot) print(plot(final_model, age = age, score = score, weights = weights))
  return(final_model)
}


#' Calculate Norm Tables for Normal Regression Model
#'
#' Generates norm tables for specific ages based on a fitted Normal regression model.
#' Computes interval probabilities, percentiles, z-scores, and norm scores for a specified
#' range of raw scores. Optionally includes confidence intervals when reliability is provided.
#'
#' @param model Fitted normal model object of class "cnormNormal"
#' @param ages Numeric vector of age points for norm table generation
#' @param start Minimum raw score value for the norm table
#' @param end Maximum raw score value for the norm table
#' @param step Step size between consecutive raw scores (default: 1)
#' @param CI Confidence coefficient (0-1, default: 0.9) for confidence intervals
#' @param reliability Reliability coefficient (0-1) for true score confidence intervals
#'
#' @return List of data frames (one per age) containing:
#'   \item{x}{Raw scores}
#'   \item{Px}{Approximate interval probabilities (density times step width).
#'     These are consistent with \code{Pcum} but do not necessarily sum to 1
#'     over the tabulated score range.}
#'   \item{Pcum}{Cumulative probabilities}
#'   \item{Percentile}{Percentile ranks (0-100)}
#'   \item{z}{Standardized z-scores}
#'   \item{norm}{Norm scores in specified scale}
#'   \item{lowerCI, upperCI}{Confidence intervals (if reliability provided)}
#'   \item{lowerCI_PR, upperCI_PR}{CI as percentile ranks (if reliability provided)}
#'
#' @details
#' For continuous normal distributions, densities are converted to interval probabilities
#' (density times step width) and cumulative probabilities. Standardized z-scores are
#' computed directly via \eqn{z = (x - \mu(\text{age})) / \sigma(\text{age})}. When reliability
#' is specified, confidence intervals include correction for regression to the mean.
#'
#' @examples
#' \dontrun{
#' # Basic norm table
#' model <- cnorm.normal(age, score)
#' tables <- normTable.normal(model, ages = c(7, 8, 9), start = 0, end = 50)
#'
#' # With confidence intervals and finer granularity
#' tables_ci <- normTable.normal(model, ages = c(8, 9), start = 10, end = 40,
#'                               step = 0.5, CI = 0.95, reliability = 0.85)
#' }
#'
#' @export
normTable.normal <- function(model,
                             ages,
                             start = NULL,
                             end = NULL,
                             step = 1,
                             CI = .9,
                             reliability = NULL) {
  # Input validation
  if (!isNormal(model)) {
    stop("Wrong object. Please provide object from class 'cnormNormal'.")
  }

  if (is.null(start)) {
    start <- attr(model$result, "min")
  }

  if (is.null(end)) {
    end <- attr(model$result, "max")
  }

  if (is.null(step)) {
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
  predictions <- predictCoefficients_normal(model, ages)

  # Create score sequence
  x <- seq(from = start, to = end, by = step)

  # Get scale information
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  result <- list()

  # Generate norm table for each age
  for (k in seq_along(ages)) {
    mu_k <- predictions$mu[k]
    sigma_k <- predictions$sigma[k]

    Px <- dnorm(x, mean = mu_k, sd = sigma_k) * step

    # Exact z-scores (bounded at +/- qnorm(1 - 1e-12) for consistency with other families)
    z_max <- qnorm(1 - 1e-12)
    z <- pmin(pmax((x - mu_k) / sigma_k, -z_max), z_max)

    # Calculate cumulative probabilities and percentiles
    cum <- pnorm(x, mean = mu_k, sd = sigma_k)
    perc <- cum

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


#' Summarize a Normal Continuous Norming Model
#'
#' This function provides a summary of a fitted Normal continuous norming model,
#' including model fit statistics, convergence information, and parameter estimates.
#'
#' @param object An object of class "cnormNormal", typically the result of a call to
#'   \code{\link{cnorm.normal}}.
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
#'   \item Basic model information (polynomial degrees, number of observations, number of parameters)
#'   \item Model fit statistics (log-likelihood, AIC, BIC)
#'   \item R-squared, RMSE, and bias (if age and raw scores are provided)
#'   \item Convergence information
#'   \item Parameter estimates with standard errors, z-values, and p-values
#' }
#'
#' @examples
#' \dontrun{
#' model <- cnorm.normal(children$age, children$score)
#' summary(model)
#'
#' # Including R-squared, RMSE, and bias in the summary:
#' summary(model, age = children$age, score = children$score)
#' }
#'
#' @seealso \code{\link{cnorm.normal}}, \code{\link{diagnostics.normal}}
#' @export
summary.cnormNormal <- function(object, ...) {
  args <- list(...)

  if ("age" %in% names(args)) {
    age <- args$age
  } else {
    if (length(args) > 0)
      age <- args[[1]]
    else
      age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else {
    if (length(args) > 1)
      score <- args[[2]]
    else
      score <- NULL
  }
  if ("weights" %in% names(args)) {
    weights <- args$weights
  } else {
    if (length(args) > 2)
      weights <- args[[3]]
    else
      weights <- NULL
  }

  diag <- diagnostics.normal(object, age, score, weights)

  cat("Normal Continuous Norming Model\n")
  cat("-------------------------------\n")
  cat("Polynomial degrees:\n")
  cat("  Location (mu):", diag$mu_degree, "\n")
  if (is.null(diag$sigma_degree) || diag$sigma_degree == 0L) {
    cat("  Scale (sigma): 0 (constant across age)\n")
  } else {
    cat("  Scale (sigma):", diag$sigma_degree, "\n")
  }
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
  cat("  Max Hessian eigenvalue:", round(diag$max_gradient, 6), "\n")
  cat("  Message:", diag$message, "\n")
  cat("\n")

  cat("Parameter Estimates:\n")
  cat("Location (mu) parameters:\n")
  mu_table <- data.frame(
    Estimate = diag$mu_estimates,
    `Std. Error` = diag$mu_se,
    `z value` = diag$mu_z_values,
    `Pr(>|z|)` = diag$mu_p_values
  )
  rownames(mu_table) <- paste0("mu_", seq_along(diag$mu_estimates) - 1L)
  print(mu_table, digits = 4)
  cat("\n")

  cat("Scale (sigma) parameters:\n")
  sigma_table <- data.frame(
    Estimate = diag$sigma_estimates,
    `Std. Error` = diag$sigma_se,
    `z value` = diag$sigma_z_values,
    `Pr(>|z|)` = diag$sigma_p_values
  )
  rownames(sigma_table) <- paste0("log(sigma)_", seq_along(diag$sigma_estimates) - 1L)
  print(sigma_table, digits = 4)

  invisible(diag)
}


#' Diagnostic Statistics for Normal Continuous Norming Model
#'
#' This function computes detailed diagnostic statistics for a fitted normal regression model,
#' including fit statistics, parameter estimates, and convergence information.
#'
#' @param object An object of class "cnormNormal"
#' @param age An optional numeric vector of age values for computing fit statistics
#' @param score An optional numeric vector of raw scores for computing fit statistics
#' @param weights An optional numeric vector of observation weights
#'
#' @return A list containing comprehensive diagnostic information
#'
#' @export
diagnostics.normal <- function(object,
                               age = NULL,
                               score = NULL,
                               weights = NULL) {
  if (!isNormal(object)) {
    stop("Wrong object. Please provide object from class 'cnormNormal'.")
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

  se <- object$se
  if (is.null(se) || all(is.na(se))) {
    mu_se <- rep(NA_real_, length(mu_estimates))
    sigma_se <- rep(NA_real_, length(sigma_estimates))
  } else {
    n_mu <- length(mu_estimates)
    n_sigma <- length(sigma_estimates)

    mu_se <- se[1:n_mu]
    sigma_se <- se[(n_mu + 1):(n_mu + n_sigma)]
  }

  # Calculate z-values and p-values
  mu_z_values <- ifelse(is.na(mu_se) | mu_se == 0,
                        NA,
                        mu_estimates / mu_se)
  sigma_z_values <- ifelse(is.na(sigma_se) | sigma_se == 0,
                           NA,
                           sigma_estimates / sigma_se)

  mu_p_values <- ifelse(is.na(mu_z_values), NA, 2 * (1 - pnorm(abs(mu_z_values))))
  sigma_p_values <- ifelse(is.na(sigma_z_values), NA, 2 * (1 - pnorm(abs(sigma_z_values))))

  # Convergence information
  converged <- object$result$convergence == 0
  n_evaluations <- object$result$counts["function"]
  message <- switch(
    as.character(object$result$convergence),
    "0" = "Successful convergence",
    "1" = "Maximum iterations reached",
    "10" = "Degeneracy in Nelder-Mead simplex",
    "51" = "Warning from L-BFGS-B",
    "52" = "Error from L-BFGS-B",
    paste("Convergence code:", object$result$convergence)
  )

  # Largest Hessian eigenvalue (curvature diagnostic), if available
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
    if (length(age) / length(unique(age)) > 50 &&
        min(table(age)) > 30) {
      data <- data.frame(group = age, raw = score)
      data <- rankByGroup(
        data = data,
        raw = "raw",
        group = "group",
        weights = weights,
        scale = c(
          attr(object$result, "scaleMean"),
          attr(object$result, "scaleSD")
        )
      )
      norm_scores <- predict(object, data$group, data$raw)
    } else {
      data <- data.frame(age = age, raw = score)
      data$groups <- getGroups(age)
      width <- (max(age) - min(age)) / length(unique(data$groups))
      data <- rankBySlidingWindow(
        data,
        age = "age",
        raw = "raw",
        width = width,
        weights = weights,
        scale = c(
          attr(object$result, "scaleMean"),
          attr(object$result, "scaleSD")
        )
      )
      norm_scores <- predict(object, data$age, data$raw)
    }

    norm_manifest <- data$normValue
    R2 <- cor(norm_scores, norm_manifest, use = "pairwise.complete.obs")^2
    rmse <- sqrt(mean((norm_scores - norm_manifest)^2))
    bias <- mean(norm_scores - norm_manifest)
  } else {
    message <- "No age and raw scores provided. Cannot calculate R2, RMSE, and bias."
  }

  # Return comprehensive diagnostic information
  list(
    # Basic model info
    mu_degree = object$mu_degree,
    sigma_degree = object$sigma_degree,
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

    # Standard errors
    mu_se = mu_se,
    sigma_se = sigma_se,

    # Test statistics
    mu_z_values = mu_z_values,
    sigma_z_values = sigma_z_values,

    mu_p_values = mu_p_values,
    sigma_p_values = sigma_p_values,

    # Convergence info
    converged = converged,
    n_evaluations = n_evaluations,
    max_gradient = max_gradient,
    message = message
  )
}


#' Predict Norm Scores from Raw Scores for a Normal Model
#'
#' This function calculates norm scores based on raw scores, age, and a fitted
#' \code{cnormNormal} model.
#'
#' @param object A fitted model object of class 'cnormNormal'.
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
#' The function predicts the Normal distribution parameters (\code{mu}, \code{sigma})
#' for each age using the provided model. Standardized z-scores are calculated directly
#' as \eqn{z = (\text{score} - \mu) / \sigma}, constrained to \code{[-range, range]},
#' and transformed to the norm scale specified in the model.
#'
#' @examples
#' \dontrun{
#' model <- cnorm.normal(children$age, children$score)
#' raw_scores <- c(25.5, 30.2, 18.7, 45.3)
#' ages <- c(7, 8, 9, 10)
#' norm_scores <- predict(model, ages, raw_scores)
#' }
#'
#' @export
#' @family predict
predict.cnormNormal <- function(object, ...) {
  model <- object
  args <- list(...)

  if ("age" %in% names(args)) {
    age <- args$age
  } else {
    if (length(args) > 0)
      age <- args[[1]]
    else
      age <- NULL
  }
  if ("score" %in% names(args)) {
    score <- args$score
  } else {
    if (length(args) > 1)
      score <- args[[2]]
    else
      score <- NULL
  }
  if ("range" %in% names(args)) {
    range <- args$range
  } else {
    if (length(args) > 2)
      range <- args[[3]]
    else
      range <- 3
  }

  if (!isNormal(model)) {
    stop("Wrong object. Please provide object from class 'cnormNormal'.")
  }

  if (is.null(age) || is.null(score)) {
    stop("Both 'age' and 'score' must be provided.")
  }

  if (length(age) != length(score)) {
    stop("The lengths of 'age' and 'score' must be the same.")
  }

  # Get predicted distribution parameters for each age
  predictions <- predictCoefficients_normal(model, age)

  mu <- predictions$mu
  sigma <- predictions$sigma

  # Exact standardized z-scores
  z_scores <- (score - mu) / sigma

  # Apply range constraints
  z_scores <- pmin(pmax(z_scores, -range), range)

  # Get scale information
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  if (!is.na(mScale) && !is.na(sdScale)) {
    # Scale z-scores to specified norm scale
    norm_scores <- mScale + sdScale * z_scores
    return(norm_scores)
  } else {
    # Return percentile ranks if no scale specified
    return(pnorm(z_scores) * 100)
  }
}
