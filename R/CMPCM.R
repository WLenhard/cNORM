# =============================================================================
# Conway-Maxwell-Poisson (CMP) regression for continuous norming of count data
# (e.g., speeded tests). Analogue to the shash and beta-binomial modules.
#
# Helpers assumed from the cNORM namespace (as used by the shash module):
#   standardize(), filter_complete(), getGroups(), weighted.quantile(),
#   rankByGroup(), rankBySlidingWindow()
# =============================================================================

#' Admissible range of log(nu): nu in [exp(-3), exp(3)] = [0.05, 20.1].
#' Linear predictors of log(nu) are clamped to this range (cf. delta in shash).
#' @noRd
CMP_LOG_NU_RANGE <- c(-3, 3)

#' Fit a Conway-Maxwell-Poisson (CMP) Regression Model for Continuous Norming
#'
#' This function fits a Conway-Maxwell-Poisson regression model (Conway & Maxwell, 1962;
#' Shmueli et al., 2005) for continuous norming of count data, e.g., speeded tests where
#' the raw score is the number of correctly processed items within a time limit. Both
#' the location parameter mu(age) and the dispersion parameter nu(age) vary smoothly as
#' polynomial functions of age (or any other continuous predictor). In contrast to the
#' Poisson model, the CMP distribution does not force variance = mean: it can model
#' over-dispersion (nu < 1), equi-dispersion (nu = 1, Poisson) and under-dispersion
#' (nu > 1), the latter being typical for speeded tests with homogeneous performance.
#'
#' @section Parameterization:
#' The probability mass function is
#' \deqn{P(Y = y) = \frac{1}{Z(\mu, \nu)} \left(\frac{\mu^y}{y!}\right)^\nu, \quad
#'   Z(\mu, \nu) = \sum_{j=0}^{\infty} \left(\frac{\mu^j}{j!}\right)^\nu}
#' i.e., the Poisson kernel raised to the power nu. This is the classical CMP
#' distribution with rate \eqn{\lambda = \mu^\nu}. Modelling \eqn{\mu} instead of
#' \eqn{\lambda} (i) makes mu interpretable as an approximate mean,
#' \eqn{E(Y) \approx \mu - (\nu - 1)/(2\nu)}, with \eqn{Var(Y) \approx \mu/\nu}
#' (the exact moments are available via \code{predictCoefficients_cmp(..., moments = TRUE)}),
#' (ii) decouples the location and dispersion parameters, which stabilizes the optimization
#' and the standard errors, and (iii) yields trivial starting values (a Poisson regression).
#' The model is
#' \deqn{\log \mu(a) = \sum_{k=0}^{K_\mu} \beta_k a^k, \qquad
#'       \log \nu(a) = \sum_{k=0}^{K_\nu} \gamma_k a^k}
#' with standardized age \eqn{a}.
#'
#' @param age A numeric vector of predictor values (typically age, but can be any continuous predictor).
#' @param score A numeric vector of raw scores. Must be the same length as age and must
#'   consist of non-negative integers (counts).
#' @param weights An optional numeric vector of weights for each observation.
#'   If NULL (default), all observations are weighted equally.
#' @param mu_degree Integer (>= 1) specifying the degree of the polynomial for log(mu(age)).
#'   Default is 3. Note that the polynomial acts on the log scale, so lower degrees
#'   than in the Taylor or shash approaches are often sufficient.
#' @param nu_degree Integer specifying the degree of the polynomial for log(nu(age)).
#'   Default is 2. Use \code{0} for a constant but estimated dispersion, and \code{NULL}
#'   to fix the dispersion at the value given in \code{nu} (\code{nu = 1} yields a
#'   Poisson regression). Recommendation: keep \code{nu_degree} low to avoid overfitting.
#' @param nu Fixed dispersion parameter (must be > 0), used only if \code{nu_degree} is NULL.
#'   Default is 1. Values < 1: over-dispersion; > 1: under-dispersion.
#' @param control An optional list of control parameters passed to \code{optim}. Any
#'   parameters not specified are filled with defaults:
#'   \itemize{
#'     \item \code{factr}: Controls precision of optimization (default: 1e3). A less extreme
#'       value than in the shash module is used because the normalizing constant is evaluated
#'       with a finite truncation tolerance, which limits attainable function precision.
#'     \item \code{maxit}: Maximum number of iterations (default: n_parameters * 200)
#'     \item \code{lmm}: Memory limit for L-BFGS-B (default: min(n_parameters, 20))
#'   }
#' @param scale Character string or numeric vector specifying the type of norm scale for output.
#'   This affects the scaling of derived norm scores but does not influence model fitting:
#'   \itemize{
#'     \item "T": T-scores (mean = 50, SD = 10) - default
#'     \item "IQ": IQ-like scores (mean = 100, SD = 15)
#'     \item "z": z-scores (mean = 0, SD = 1)
#'     \item c(M, SD): Custom scale with specified mean M and standard deviation SD
#'   }
#' @param max_terms Maximum number of terms used to evaluate the normalizing constant
#'   Z(mu, nu). Default (NULL) is \code{max(2000, 20 * max(score))}. Increase only if
#'   a warning indicates that the series did not converge.
#' @param tol Relative truncation tolerance for the normalizing constant (default 1e-12).
#' @param plot Logical indicating whether to automatically display a diagnostic plot of the
#'   fitted model. Default is TRUE.
#'
#' @return An object of class "cnormCMP" containing the fitted model results. This is a list with:
#'   \item{mu_est}{Coefficients of the polynomial for log(mu(age)). The first coefficient is the intercept.}
#'   \item{nu_est}{Coefficients of the polynomial for log(nu(age)); NULL if nu is fixed.}
#'   \item{nu}{The fixed dispersion value (relevant if \code{nu_degree} is NULL).}
#'   \item{se}{Standard errors of all estimated coefficients (if the Hessian could be inverted).}
#'   \item{mu_degree, nu_degree}{The polynomial degrees used.}
#'   \item{result}{Complete output from \code{optim}, including convergence information,
#'     and the data attributes needed for prediction.}
#'
#' @details
#' Parameters are estimated by maximum likelihood with L-BFGS-B and analytic gradients.
#' The CMP distribution belongs to the exponential family in (log lambda, -nu), so the
#' score equations are simple moment conditions: the gradient with respect to the linear
#' predictors only requires E(Y) and E(log Y!) under the current CMP distribution, which
#' are obtained from the same truncated series as the normalizing constant. Series
#' evaluation is vectorized, truncation is adaptive with a rigorous geometric tail bound,
#' and observations sharing the same (mu, nu) (e.g., age groups) are evaluated only once.
#'
#' \subsection{Differences to the beta-binomial and shash models}{
#' The CMP distribution has an unbounded support. It is appropriate for speeded tests and
#' other open-ended counts. For tests with a fixed number of items and a hard ceiling
#' (accuracy tests), the beta-binomial model respects this bound, whereas the CMP model
#' assigns (small) probability mass to scores above the ceiling. Restrict the norm table
#' to the possible score range in this case.
#' }
#'
#' @note
#' \itemize{
#'   \item Raw scores must be non-negative integers.
#'   \item The dispersion is restricted to nu in [0.05, 20]. If coefficients end up at their
#'     bounds, a warning is issued and the Wald tests are not valid.
#'   \item Polynomial models can exhibit edge effects; predict outside the observed age range
#'     with caution.
#'   \item Because the distribution is discrete, percentile curves in the plot are step-like.
#' }
#'
#' @seealso \code{\link{cnorm.shash}}, \code{\link{autoselect.cmp}},
#'   \code{\link{normTable.cmp}}, \code{\link{dcmp}}
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- cnorm.cmp(age = speeded$age, score = speeded$raw)
#'
#' # Constant but estimated dispersion
#' model0 <- cnorm.cmp(speeded$age, speeded$raw, mu_degree = 3, nu_degree = 0)
#'
#' # Poisson regression (dispersion fixed to 1)
#' model_pois <- cnorm.cmp(speeded$age, speeded$raw, nu_degree = NULL, nu = 1)
#'
#' summary(model, age = speeded$age, score = speeded$raw)
#' normTable.cmp(model, ages = c(8, 9), start = 0, end = 80)
#' }
#'
#' @author Wolfgang Lenhard
#' @references
#' Conway, R. W., & Maxwell, W. L. (1962). A queuing model with state dependent service rates.
#' *Journal of Industrial Engineering*, 12, 132-136.
#'
#' Shmueli, G., Minka, T. P., Kadane, J. B., Borle, S., & Boatwright, P. (2005). A useful
#' distribution for fitting discrete data: revival of the Conway-Maxwell-Poisson distribution.
#' *Journal of the Royal Statistical Society C*, 54(1), 127-142.
#'
#' Sellers, K. F., & Shmueli, G. (2010). A flexible regression model for count data.
#' *The Annals of Applied Statistics*, 4(2), 943-961.
#'
#' Huang, A. (2017). Mean-parametrized Conway-Maxwell-Poisson regression models for dispersed
#' counts. *Statistical Modelling*, 17(6), 359-380.
#'
#' @export
cnorm.cmp <- function(age,
                      score,
                      weights = NULL,
                      mu_degree = 3,
                      nu_degree = 2,
                      nu = 1,
                      control = NULL,
                      scale = "T",
                      max_terms = NULL,
                      tol = 1e-12,
                      plot = TRUE) {
  # Input validation
  if (length(age) != length(score)) {
    stop("Length of 'age' and 'score' must be the same.")
  }

  if (!is.null(weights) && length(age) != length(weights)) {
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  }

  if (!is.numeric(mu_degree) || length(mu_degree) != 1L || mu_degree < 1 ||
      mu_degree != round(mu_degree)) {
    stop("'mu_degree' must be a positive integer.")
  }

  if (!is.null(nu_degree) &&
      (!is.numeric(nu_degree) || length(nu_degree) != 1L || nu_degree < 0 ||
       nu_degree != round(nu_degree))) {
    stop("'nu_degree' must be NULL (fixed nu) or a non-negative integer (0 = constant nu).")
  }

  if (!is.numeric(nu) || length(nu) != 1L || !is.finite(nu) || nu <= 0) {
    stop("'nu' must be a single positive number.")
  }

  # validate 'scale' early with explicit errors. Accepts integer vectors as well.
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

  # The CMP model is a model for counts
  if (any(score < 0)) {
    stop("'score' contains negative values. The CMP model requires non-negative integer counts.")
  }
  if (any(abs(score - round(score)) > 1e-8)) {
    stop("'score' contains non-integer values. The CMP model requires integer counts. ",
         "For continuous raw scores, consider cnorm.shash().")
  }
  score <- round(score)

  # Guard against degenerate input (zero/undefined variance)
  if (length(score) < 2L || !is.finite(sd(score)) || sd(score) <= 0) {
    stop("'score' has zero or undefined variance. The model cannot be fitted.")
  }

  if (is.null(max_terms)) {
    max_terms <- max(2000L, as.integer(ceiling(20 * max(score))))
  }

  # Standardize age
  age_std <- standardize(age)

  # Create design matrices
  X_mu <- cmp_design(age_std, mu_degree)
  if (!is.null(nu_degree)) {
    X_nu <- cmp_design(age_std, nu_degree)
    use_varying_nu <- TRUE
  } else {
    X_nu <- NULL
    use_varying_nu <- FALSE
  }
  fixed_nu <- if (use_varying_nu) NULL else nu

  # Initial values (Poisson regression for mu, moment-based start for nu)
  initial_params <- cmp_start_values(X_mu, X_nu, score, weights)

  n_param <- length(initial_params)
  control_defaults <- list(factr = 1e3,
                           maxit = n_param * 200,
                           lmm = min(n_param, 20))
  control <- if (is.null(control))
    control_defaults
  else
    utils::modifyList(control_defaults, control)

  # Parameter bounds (only the dispersion coefficients are box-constrained)
  lower_bounds <- rep(-Inf, n_param)
  upper_bounds <- rep(Inf, n_param)
  if (use_varying_nu) {
    nu_idx <- ncol(X_mu) + seq_len(ncol(X_nu))
    lower_bounds[nu_idx] <- CMP_LOG_NU_RANGE[1]
    upper_bounds[nu_idx] <- CMP_LOG_NU_RANGE[2]
  }

  # Objective with analytic gradient; function value and gradient share one
  # evaluation of the series (cached for identical parameter vectors)
  obj <- cmp_make_objective(X_mu, X_nu, score, weights, fixed_nu, tol, max_terms)

  run_optim <- function(start, ctrl) {
    optim(
      start,
      obj$fn,
      gr = obj$gr,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      hessian = TRUE,
      control = ctrl
    )
  }

  result <- tryCatch(run_optim(initial_params, control), error = function(e) NULL)

  if (is.null(result) || !is.finite(result$value) || result$value >= 1e9) {
    message("First optimization attempt failed. Trying with different parameters...")

    # Try with more conservative initial values
    initial_params2 <- initial_params
    initial_params2[seq_len(ncol(X_mu))] <- c(log(max(stats::median(score), 0.5)),
                                              rep(0, ncol(X_mu) - 1L))
    if (use_varying_nu)
      initial_params2[nu_idx] <- 0

    control2 <- control
    control2$factr <- if (is.null(control2$factr)) 1e4 else control2$factr * 10
    control2$maxit <- if (is.null(control2$maxit)) 1000 else control2$maxit * 2

    result <- run_optim(initial_params2, control2)
  }

  if (!is.finite(result$value) || result$value >= 1e9) {
    stop("Optimization failed: the CMP likelihood could not be evaluated. ",
         "Try reducing the polynomial degrees, fixing nu (nu_degree = NULL) or increasing 'max_terms'.")
  }

  # Check convergence
  if (result$convergence != 0) {
    warning(
      "Optimization did not converge (code: ",
      result$convergence,
      "). Consider adjusting control parameters or degrees of the polynomials. Check percentile curves for plausibility."
    )
  }

  # Warn when parameters sit at their box constraints; SEs and Wald tests are invalid there.
  at_bound <- is.finite(lower_bounds) &
    (result$par <= lower_bounds + 1e-6 | result$par >= upper_bounds - 1e-6)
  if (any(at_bound)) {
    warning(
      "Parameter(s) at index ",
      paste(which(at_bound), collapse = ", "),
      " reached their bounds (nu is restricted to [",
      round(exp(CMP_LOG_NU_RANGE[1]), 2), ", ", round(exp(CMP_LOG_NU_RANGE[2]), 1),
      "]). Standard errors and Wald tests for these ",
      "parameters are not valid. Consider reducing model complexity."
    )
  }

  # Extract parameter estimates
  n_mu <- ncol(X_mu)
  mu_est <- result$par[seq_len(n_mu)]
  nu_est <- if (use_varying_nu) result$par[n_mu + seq_len(ncol(X_nu))] else NULL

  # Guarded SE computation
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
  attr(result, "nu") <- nu
  attr(result, "max_terms") <- max_terms
  attr(result, "tol") <- tol

  # Create model object
  model <- list(
    mu_est = mu_est,
    nu_est = nu_est,
    nu = nu,
    se = se,
    mu_degree = mu_degree,
    nu_degree = nu_degree,
    result = result
  )

  class(model) <- "cnormCMP"

  if (plot) {
    p <- plot.cnormCMP(model, age, score, weights = weights)
    print(p)
  }

  return(model)
}


#' Conway-Maxwell-Poisson (CMP) Distribution
#'
#' Probability mass function, distribution function, quantile function and random
#' generation for the Conway-Maxwell-Poisson distribution in the (mu, nu)
#' parameterization used by \code{\link{cnorm.cmp}}:
#' \deqn{P(Y = y) \propto (\mu^y / y!)^\nu .}
#' The classical rate parameter is \eqn{\lambda = \mu^\nu}. For \code{nu = 1}, the
#' distribution is the Poisson distribution with mean \code{mu}.
#'
#' @name cmp
#' @aliases dcmp pcmp qcmp rcmp
#'
#' @param x,q vector of quantiles (non-negative integers for \code{dcmp}).
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param mu location parameter (> 0, default 1); approximately the mean,
#'   \eqn{E(Y) \approx \mu - (\nu - 1)/(2\nu)}.
#' @param nu dispersion parameter (> 0, default 1). \code{nu < 1}: over-dispersion
#'   (variance > mean); \code{nu > 1}: under-dispersion (variance < mean).
#' @param log,log.p logical; if TRUE, probabilities are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are P[X <= x], otherwise P[X > x].
#'
#' @details
#' The normalizing constant is evaluated by a vectorized, adaptively truncated series
#' with a rigorous geometric tail bound (relative error < 1e-12). The quantile function
#' is computed by inversion of the cumulative sums, and random numbers are generated by
#' inversion.
#'
#' @return \code{dcmp} gives the mass, \code{pcmp} the distribution function,
#' \code{qcmp} the quantile function, and \code{rcmp} generates random deviates.
#'
#' @references
#' Shmueli, G., Minka, T. P., Kadane, J. B., Borle, S., & Boatwright, P. (2005). A useful
#' distribution for fitting discrete data: revival of the Conway-Maxwell-Poisson distribution.
#' \emph{Journal of the Royal Statistical Society C}, 54(1), 127-142.
#'
#' @examples
#' # Equals the Poisson distribution for nu = 1
#' all.equal(dcmp(0:20, mu = 8, nu = 1), dpois(0:20, 8))
#'
#' # Under-dispersed (nu = 3) and over-dispersed (nu = 0.5) counts with similar mean
#' x <- rcmp(1000, mu = 20, nu = 3)
#' c(mean(x), var(x))
#' y <- rcmp(1000, mu = 20, nu = 0.5)
#' c(mean(y), var(y))
#'
#' qcmp(c(0.025, 0.5, 0.975), mu = 20, nu = 3)
#' pcmp(20, mu = 20, nu = 3)
#'
#' @export
#' @rdname cmp
dcmp <- function(x, mu = 1, nu = 1, log = FALSE) {
  if (length(x) == 0L || length(mu) == 0L || length(nu) == 0L)
    return(numeric(0))
  n <- max(length(x), length(mu), length(nu))
  x <- rep_len(x, n)
  mu <- rep_len(mu, n)
  nu <- rep_len(nu, n)

  out <- rep(NA_real_, n)
  bad <- is.na(x) | is.na(mu) | is.na(nu)
  nan <- !bad & (mu < 0 | nu <= 0)
  zero <- !bad & !nan & mu == 0
  calc <- !bad & !nan & mu > 0
  out[nan] <- NaN

  if (any(zero)) {
    out[zero] <- as.numeric(x[zero] == 0)
    if (log) out[zero] <- ifelse(x[zero] == 0, 0, -Inf)
  }

  if (any(calc)) {
    xc <- x[calc]
    eta <- base::log(mu[calc])
    nuc <- nu[calc]
    mom <- cmp_engine(eta, nuc, max_terms = cmp_max_terms(mu[calc]))
    ld <- nuc * (xc * eta - lgamma(xc + 1)) - mom$logZ
    nonint <- abs(xc - round(xc)) > 1e-7 * pmax(1, abs(xc))
    if (any(nonint)) warning("non-integer x in dcmp")
    ld[nonint | xc < 0] <- -Inf
    if (any(!mom$ok)) {
      warning("CMP series did not converge for some parameter combinations; NaN produced.")
      ld[!mom$ok] <- NaN
    }
    out[calc] <- if (log) ld else exp(ld)
  }
  out
}

#' @export
#' @rdname cmp
pcmp <- function(q, mu = 1, nu = 1, lower.tail = TRUE, log.p = FALSE) {
  if (length(q) == 0L || length(mu) == 0L || length(nu) == 0L)
    return(numeric(0))
  n <- max(length(q), length(mu), length(nu))
  q <- rep_len(q, n)
  mu <- rep_len(mu, n)
  nu <- rep_len(nu, n)

  out <- rep(NA_real_, n)
  bad <- is.na(q) | is.na(mu) | is.na(nu)
  nan <- !bad & (mu < 0 | nu <= 0)
  zero <- !bad & !nan & mu == 0
  calc <- !bad & !nan & mu > 0
  out[nan] <- NaN

  if (any(zero)) {
    p0 <- as.numeric(q[zero] >= 0)
    out[zero] <- if (lower.tail) p0 else 1 - p0
  }

  if (any(calc)) {
    mom <- cmp_engine(base::log(mu[calc]), nu[calc], q = floor(q[calc]),
                      max_terms = cmp_max_terms(mu[calc]))
    pr <- if (lower.tail) mom$lower else mom$upper
    if (any(!mom$ok)) {
      warning("CMP series did not converge for some parameter combinations; NaN produced.")
      pr[!mom$ok] <- NaN
    }
    out[calc] <- pr
  }

  if (log.p) base::log(out) else out
}

#' @export
#' @rdname cmp
qcmp <- function(p, mu = 1, nu = 1, lower.tail = TRUE, log.p = FALSE) {
  if (length(p) == 0L || length(mu) == 0L || length(nu) == 0L)
    return(numeric(0))
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  n <- max(length(p), length(mu), length(nu))
  p <- rep_len(p, n)
  mu <- rep_len(mu, n)
  nu <- rep_len(nu, n)

  out <- rep(NA_real_, n)
  bad <- is.na(p) | is.na(mu) | is.na(nu)
  nan <- !bad & (mu < 0 | nu <= 0 | p < 0 | p > 1)
  out[nan] <- NaN

  ok_in <- !bad & !nan
  out[ok_in & (mu == 0 | p == 0)] <- 0
  out[ok_in & mu > 0 & p == 1] <- Inf

  calc <- ok_in & mu > 0 & p > 0 & p < 1
  if (any(calc)) {
    mom <- cmp_engine(base::log(mu[calc]), nu[calc], p = p[calc],
                      max_terms = cmp_max_terms(mu[calc]))
    qq <- mom$quant
    if (any(!mom$ok)) {
      warning("CMP series did not converge for some parameter combinations; NaN produced.")
      qq[!mom$ok] <- NaN
    }
    out[calc] <- qq
  }
  out
}

#' @export
#' @rdname cmp
rcmp <- function(n, mu = 1, nu = 1) {
  if (length(n) > 1L) n <- length(n)
  if (n == 0L) return(numeric(0))
  u <- stats::runif(n)
  qcmp(u, mu = rep_len(mu, n), nu = rep_len(nu, n))
}


# -----------------------------------------------------------------------------
# Internal computing engine
# -----------------------------------------------------------------------------

#' Polynomial design matrix with intercept (degree 0 = intercept only)
#' @keywords internal
#' @noRd
cmp_design <- function(x, degree) {
  if (degree < 1L) {
    matrix(1, nrow = length(x), ncol = 1L)
  } else {
    cbind(1, poly(x, degree = degree, raw = TRUE))
  }
}

#' Default truncation limit for the series, depending on the largest mu
#' @keywords internal
#' @noRd
cmp_max_terms <- function(mu) {
  mu <- mu[is.finite(mu)]
  top <- if (length(mu)) max(mu) else 0
  as.integer(max(2000, min(1e5, ceiling(20 * top))))
}

#' Group identical rows of one or more numeric key vectors
#'
#' @param keys List of numeric vectors of equal length.
#' @return List with \code{rep} (index of one representative per group) and
#'   \code{inv} (group index of every element).
#' @keywords internal
#' @noRd
cmp_group <- function(keys) {
  n <- length(keys[[1L]])
  if (n < 2L) {
    return(list(rep = seq_len(n), inv = seq_len(n)))
  }
  ord <- do.call(order, unname(keys))
  newgrp <- c(TRUE, rep(FALSE, n - 1L))
  for (k in keys) {
    ks <- k[ord]
    d <- ks[-1L] != ks[-n]
    d[is.na(d)] <- TRUE
    newgrp[-1L] <- newgrp[-1L] | d
  }
  inv <- integer(n)
  inv[ord] <- cumsum(newgrp)
  list(rep = ord[newgrp], inv = inv)
}

#' Evaluate the CMP series for a block of parameter pairs with a common truncation J
#'
#' With eta = log(mu), the log-terms are log t_j = nu * (j * eta - log(j!)), j = 0..J.
#' The terms are log-concave in j with maximum at floor(mu); they are therefore
#' normalized by the exact maximum (stable log-sum-exp). Since
#' t_{j+1} / t_j = (mu / (j + 1))^nu is decreasing in j, the neglected tail after J is
#' bounded by t_J * r / (1 - r) with r = (mu / (J + 1))^nu (valid for r < 1).
#'
#' @param eta,nu Vectors (length m) of log(mu) and nu.
#' @param J Highest summation index.
#' @param q Optional vector (length m): returns P(Y <= q) and P(Y > q).
#' @param p Optional vector (length m): returns the smallest x with P(Y <= x) >= p.
#' @param second Logical; also return E(Y^2).
#' @param tol Relative tolerance of the tail bound.
#'
#' @return A list with logZ, EY, ElgF (= E[log Y!]), ok and optionally EY2, lower, upper, quant.
#' @keywords internal
#' @noRd
cmp_block <- function(eta, nu, J, q = NULL, p = NULL, second = FALSE, tol = 1e-12) {
  m <- length(eta)
  js <- 0:J
  lg <- lgamma(js + 1)

  A <- outer(eta, js)                     # m x (J + 1): j * eta
  A <- (A - rep(lg, each = m)) * nu       # nu * (j * eta - log j!)

  jstar <- pmin(floor(exp(pmin(eta, 700))), J)
  mx <- nu * (jstar * eta - lgamma(jstar + 1))
  E <- exp(A - mx)                        # relative terms, maximum equals 1
  S <- rowSums(E)

  out <- list(
    logZ = mx + log(S),
    EY   = drop(E %*% js) / S,
    ElgF = drop(E %*% lg) / S
  )
  if (second) out$EY2 <- drop(E %*% (js^2)) / S

  # Rigorous bound of the truncated tail (relative to the sum)
  r <- exp(nu * (eta - log(J + 1)))
  tail_rel <- E[, J + 1L] * r / pmax(1 - r, 1e-300) / S
  tail_rel[!(r < 1)] <- Inf
  out$ok <- is.finite(out$logZ) & (tail_rel < tol)

  if (!is.null(q)) {
    mask <- matrix(js, nrow = m, ncol = J + 1L, byrow = TRUE) <= q
    out$lower <- rowSums(E * mask) / S
    out$upper <- rowSums(E * (!mask)) / S
  }
  if (!is.null(p)) {
    Cm <- t(apply(E, 1L, cumsum)) / S
    out$quant <- rowSums(Cm < p * (1 - 64 * .Machine$double.eps))
  }
  out
}

#' Vectorized CMP series engine with adaptive truncation
#'
#' Rows are de-duplicated (identical mu, nu[, q or p] are evaluated once), binned by their
#' estimated required truncation (powers of 2), and processed in memory-bounded blocks.
#' Rows whose tail bound is not met are re-evaluated with doubled truncation up to
#' \code{max_terms}; rows failing there are flagged via \code{ok = FALSE}.
#'
#' @param eta Numeric vector, log(mu).
#' @param nu Numeric vector (or scalar), dispersion.
#' @param q,p,second See \code{cmp_block}.
#' @param tol Relative tolerance of the tail bound.
#' @param max_terms Maximum number of series terms.
#' @param block_size Maximum number of matrix cells per block.
#'
#' @return A list of vectors (length of \code{eta}): logZ, EY, ElgF, ok and optionally
#'   EY2, lower, upper, quant.
#' @keywords internal
#' @noRd
cmp_engine <- function(eta, nu, q = NULL, p = NULL, second = FALSE,
                       tol = 1e-12, max_terms = 2000L, block_size = 1e6) {
  n <- length(eta)
  if (length(nu) == 1L) nu <- rep(nu, n)

  keys <- list(eta, nu)
  if (!is.null(q)) keys <- c(keys, list(q))
  if (!is.null(p)) keys <- c(keys, list(p))
  g <- cmp_group(keys)

  e <- eta[g$rep]
  v <- nu[g$rep]
  qq <- if (!is.null(q)) q[g$rep] else NULL
  pp <- if (!is.null(p)) p[g$rep] else NULL
  m <- length(e)

  fields <- c("logZ", "EY", "ElgF")
  if (second) fields <- c(fields, "EY2")
  if (!is.null(q)) fields <- c(fields, "lower", "upper")
  if (!is.null(p)) fields <- c(fields, "quant")
  res <- lapply(stats::setNames(fields, fields), function(f) rep(NA_real_, m))
  res$ok <- rep(FALSE, m)

  valid <- is.finite(e) & is.finite(v) & v > 0
  if (!is.null(qq)) valid <- valid & !is.na(qq)
  if (!is.null(pp)) valid <- valid & !is.na(pp)
  pending <- valid

  # Initial truncation: the terms peak at about mu with spread about sqrt(mu / nu)
  mu_a <- exp(pmin(e, 700))
  J_est <- ceiling(mu_a + 10 * sqrt(mu_a / v) + 30)
  J_bin <- pmin(2^ceiling(log2(pmax(J_est, 64))), max_terms)

  while (any(pending)) {
    for (b in sort(unique(J_bin[pending]))) {
      rows <- which(pending & J_bin == b)
      if (length(rows) == 0L) next
      step <- max(1L, floor(block_size / (b + 1)))
      for (s in seq(1L, length(rows), by = step)) {
        idx <- rows[s:min(s + step - 1L, length(rows))]
        blk <- cmp_block(e[idx], v[idx], b, q = qq[idx], p = pp[idx],
                         second = second, tol = tol)
        for (f in fields) res[[f]][idx] <- blk[[f]]
        good <- blk$ok
        res$ok[idx[good]] <- TRUE
        finished <- good | (b >= max_terms)
        pending[idx[finished]] <- FALSE
        if (any(!finished)) J_bin[idx[!finished]] <- min(2 * b, max_terms)
      }
    }
  }

  lapply(res, function(x) x[g$inv])
}

#' Initial values for the CMP regression
#'
#' mu: Poisson regression (weighted); nu: from the Pearson dispersion of that fit,
#' using Var(Y) approx mu / nu.
#' @keywords internal
#' @noRd
cmp_start_values <- function(X_mu, X_nu, y, weights) {
  n_mu <- ncol(X_mu)
  w <- if (is.null(weights)) rep(1, length(y)) else weights

  beta <- NULL
  phi <- NA_real_
  fit <- tryCatch(
    suppressWarnings(stats::glm.fit(x = X_mu, y = y, weights = w,
                                    family = stats::poisson(),
                                    control = list(maxit = 50))),
    error = function(e) NULL
  )
  if (!is.null(fit) && isTRUE(fit$converged) && all(is.finite(fit$coefficients))) {
    beta <- unname(fit$coefficients)
    fv <- fit$fitted.values
    pos <- is.finite(fv) & fv > 0
    phi <- sum(w[pos] * (y[pos] - fv[pos])^2 / fv[pos]) / max(sum(w[pos]) - n_mu, 1)
  }
  if (is.null(beta)) {
    beta <- c(log(max(stats::weighted.mean(y, w), 0.1)), rep(0, n_mu - 1L))
  }

  start <- beta
  if (!is.null(X_nu)) {
    nu0 <- if (is.finite(phi) && phi > 0) -log(phi) else 0
    nu0 <- min(max(nu0, -2), 2)
    start <- c(start, nu0, rep(0, ncol(X_nu) - 1L))
  }
  start
}

#' Negative log-likelihood and gradient of the CMP regression model (joint evaluation)
#'
#' Per observation, with eta = log(mu), nu = exp(eta_nu) and
#' \eqn{\ell = \nu (y \eta - \log y!) - \log Z(\mu, \nu)}:
#' \deqn{\partial \ell / \partial \eta = \nu (y - E Y)}
#' \deqn{\partial \ell / \partial \log\nu = \nu [\eta (y - E Y) - (\log y! - E \log Y!)]}
#' where expectations are taken under the current CMP distribution. Contributions of
#' observations whose log(nu) predictor is clamped are zeroed (exact subgradient of the
#' clamped objective).
#'
#' @return A list with \code{nll} and \code{grad}. If the series cannot be evaluated
#'   (non-finite parameters or truncation limit reached), a large penalty (1e10) and a
#'   zero gradient are returned, as in the shash module.
#' @keywords internal
#' @noRd
cmp_evaluate <- function(params,
                         X_mu,
                         X_nu = NULL,
                         y,
                         weights = NULL,
                         fixed_nu = NULL,
                         tol = 1e-12,
                         max_terms = 2000L) {
  n_mu <- ncol(X_mu)
  penalty <- list(nll = 1e10, grad = rep(0, length(params)))

  eta <- drop(X_mu %*% params[seq_len(n_mu)])
  if (!is.null(X_nu)) {
    eta_nu <- drop(X_nu %*% params[n_mu + seq_len(ncol(X_nu))])
    nu_free <- (eta_nu > CMP_LOG_NU_RANGE[1]) & (eta_nu < CMP_LOG_NU_RANGE[2])
    nu <- exp(pmin.int(pmax.int(eta_nu, CMP_LOG_NU_RANGE[1]), CMP_LOG_NU_RANGE[2]))
  } else {
    nu <- rep(fixed_nu, length(y))
  }

  if (is.null(weights)) {
    weights <- 1  # Will broadcast in multiplication
  }

  if (any(!is.finite(eta))) return(penalty)

  mom <- cmp_engine(eta, nu, tol = tol, max_terms = max_terms)
  if (!all(mom$ok)) return(penalty)

  lgy <- lgamma(y + 1)
  logdens <- nu * (y * eta - lgy) - mom$logZ
  nll <- -sum(weights * logdens)
  if (!is.finite(nll)) return(penalty)

  resid <- y - mom$EY
  grad <- -drop(crossprod(X_mu, weights * (nu * resid)))
  if (!is.null(X_nu)) {
    d_lognu <- nu * (eta * resid - (lgy - mom$ElgF)) * nu_free
    grad <- c(grad, -drop(crossprod(X_nu, weights * d_lognu)))
  }
  grad[!is.finite(grad)] <- 0

  list(nll = nll, grad = grad)
}

#' Calculate the negative log-likelihood for a CMP regression model
#'
#' @param params A numeric vector containing all model parameters (mu coefficients, then nu coefficients)
#' @param X_mu Design matrix for log(mu)
#' @param X_nu Design matrix for log(nu) (or NULL for fixed nu)
#' @param y Response vector (counts)
#' @param weights Observation weights
#' @param fixed_nu If X_nu is NULL, the dispersion is fixed to this value
#' @param tol,max_terms Tolerance and truncation limit of the series
#'
#' @return The negative log-likelihood of the model
#' @keywords internal
log_likelihood_cmp <- function(params, X_mu, X_nu = NULL, y, weights = NULL,
                               fixed_nu = NULL, tol = 1e-12, max_terms = 2000L) {
  cmp_evaluate(params, X_mu, X_nu, y, weights, fixed_nu, tol, max_terms)$nll
}

#' Analytic gradient of the negative log-likelihood for a CMP regression model
#'
#' @inheritParams log_likelihood_cmp
#' @return Numeric vector: gradient of the negative log-likelihood with respect to \code{params}.
#' @keywords internal
gradient_cmp <- function(params, X_mu, X_nu = NULL, y, weights = NULL,
                         fixed_nu = NULL, tol = 1e-12, max_terms = 2000L) {
  cmp_evaluate(params, X_mu, X_nu, y, weights, fixed_nu, tol, max_terms)$grad
}

#' Build objective and gradient closures that share one series evaluation
#' @keywords internal
#' @noRd
cmp_make_objective <- function(X_mu, X_nu, y, weights, fixed_nu, tol, max_terms) {
  last_par <- NULL
  last_val <- NULL
  evaluate <- function(par) {
    if (!is.null(last_par) && identical(par, last_par)) return(last_val)
    val <- cmp_evaluate(par, X_mu, X_nu, y, weights, fixed_nu, tol, max_terms)
    last_par <<- par
    last_val <<- val
    val
  }
  list(fn = function(par) evaluate(par)$nll,
       gr = function(par) evaluate(par)$grad)
}

#' Is the object a fitted CMP model?
#' @keywords internal
#' @noRd
isCMP <- function(model) {
  inherits(model, "cnormCMP")
}

#' Cumulative and point probabilities in one pass (for norm tables and prediction)
#'
#' @return List with pmf = P(Y = x), cdf_prev = P(Y <= x - 1), cdf = P(Y <= x), ok.
#' @keywords internal
#' @noRd
cmp_cum_pmf <- function(x, mu, nu) {
  eta <- base::log(mu)
  mom <- cmp_engine(eta, nu, q = x - 1, max_terms = cmp_max_terms(mu))
  pmf <- exp(nu * (x * eta - lgamma(x + 1)) - mom$logZ)
  cdf_prev <- mom$lower
  list(pmf = pmf, cdf_prev = cdf_prev, cdf = pmin(cdf_prev + pmf, 1), ok = mom$ok)
}


#' Predict parameters for a CMP regression model
#'
#' @param model An object of class "cnormCMP"
#' @param ages A numeric vector of age points for prediction
#' @param moments Logical; if TRUE, the exact mean, variance and standard deviation of the
#'   CMP distribution are added (requires one additional series evaluation per age).
#'
#' @return A data frame with predicted mu, nu and lambda = mu^nu (and optionally mean, variance, sd)
#'
#' @keywords internal
predictCoefficients_cmp <- function(model, ages, moments = FALSE) {
  if (!isCMP(model)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }

  # Standardize new ages
  ages_std <- (ages - attr(model$result, "age_mean")) / attr(model$result, "age_sd")

  X_mu_new <- cmp_design(ages_std, model$mu_degree)
  log_mu <- drop(X_mu_new %*% model$mu_est)
  mu <- exp(log_mu)

  if (!is.null(model$nu_degree)) {
    X_nu_new <- cmp_design(ages_std, model$nu_degree)
    log_nu <- pmin(pmax(drop(X_nu_new %*% model$nu_est), CMP_LOG_NU_RANGE[1]),
                   CMP_LOG_NU_RANGE[2])
    nu <- exp(log_nu)
  } else {
    nu <- rep(model$nu, length(ages))
  }

  predicted <- data.frame(
    age = ages,
    mu = mu,
    nu = nu,
    lambda = mu^nu
  )

  if (moments) {
    mom <- cmp_engine(log_mu, nu, second = TRUE, max_terms = cmp_max_terms(mu))
    predicted$mean <- mom$EY
    predicted$variance <- pmax(mom$EY2 - mom$EY^2, 0)
    predicted$sd <- sqrt(predicted$variance)
  }

  return(predicted)
}

#' Plot CMP Model with Data and Percentile Lines
#'
#' @param x A fitted model object of class "cnormCMP"
#' @param ... Additional arguments including age, score, weights, percentiles, points
#'
#' @return A ggplot object
#'
#' @export
plot.cnormCMP <- function(x, ...) {
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

  if (!isCMP(model)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
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
  preds <- predictCoefficients_cmp(model, pred_ages)

  # Calculate percentile lines using CMP quantiles (discrete: step-like curves)
  percentile_lines <- lapply(percentiles, function(p) {
    qcmp(p, mu = preds$mu, nu = preds$nu)
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    mu = preds$mu,
    nu = preds$nu,
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
      title = "Percentile Plot (Conway-Maxwell-Poisson Model)",
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

#' Print method for CMP objects
#'
#' @param x A cnormCMP object
#' @param ... Additional arguments
#' @export
print.cnormCMP <- function(x, ...) {
  cat("Conway-Maxwell-Poisson Regression Model for Continuous Norming\n")
  cat("==============================================================\n\n")

  cat("Model Parameters:\n")
  cat("- Location (mu, log link) polynomial degree:", x$mu_degree, "\n")
  if (!is.null(x$nu_degree)) {
    cat("- Dispersion (nu, log link): Polynomial degree",
        x$nu_degree,
        ifelse(x$nu_degree == 0, "(constant)", ""),
        "\n\n")
  } else {
    cat("- Dispersion (nu): Fixed at", x$nu, "\n\n")
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
  invisible(x)
}

#' Automatic model selection for CMP continuous norming via BIC
#'
#' Selects the polynomial degrees for the two components of a
#' \code{\link{cnorm.cmp}} model (\eqn{\mu}, \eqn{\nu}) by minimizing BIC over a full grid.
#'
#' The degree for \eqn{\nu} has three kinds of values: \code{-1} fixes the dispersion at the
#' value given in \code{nu} (\code{nu_degree = NULL} in \code{cnorm.cmp}; \code{nu = 1} is the
#' Poisson model), \code{0} estimates a constant dispersion, and values >= 1 let the
#' dispersion vary with age.
#'
#' Parallel execution is attempted by default. If the workers cannot access
#' the \pkg{cNORM} namespace, the function transparently falls back to sequential execution.
#'
#' @param age,score Numeric vectors of predictor and response values (counts).
#' @param weights Optional numeric vector of observation weights.
#' @param max_mu,max_nu Maximum polynomial degrees for log(mu) and log(nu). Defaults: \code{4, 2}.
#' @param min_mu Minimum polynomial degree for log(mu) (default 1).
#' @param min_nu Minimum degree for log(nu). \code{-1}: fixed nu is included in the search;
#'   \code{0} (default): constant estimated nu is the simplest candidate.
#' @param nu Value of the dispersion used whenever a candidate has a fixed nu (degree -1). Default 1.
#' @param control Optional control list passed to \code{\link[stats]{optim}}.
#' @param scale Norm scale (default \code{"T"}).
#' @param max_terms,tol Passed to \code{\link{cnorm.cmp}}.
#' @param parallel Logical; attempt parallel execution. Default \code{TRUE}.
#' @param n_cores Number of cores. Defaults to all logical cores.
#' @param plot Logical; plot the selected model. Default \code{TRUE}.
#' @param verbose Logical; print progress. Default \code{TRUE}.
#'
#' @return The selected fitted \code{cnormCMP} model with an additional
#'   element \code{$selection} containing:
#'   \itemize{
#'     \item \code{evaluated}: data frame of every combination tried, sorted by BIC.
#'     \item \code{selected}: list with the chosen degrees and BIC.
#'   }
#'
#' @examples
#' \dontrun{
#' m <- autoselect.cmp(speeded$age, speeded$raw)
#' m$selection$evaluated
#' summary(m, age = speeded$age, score = speeded$raw)
#'
#' # Include the Poisson model (nu fixed at 1) as a candidate
#' m <- autoselect.cmp(speeded$age, speeded$raw, min_nu = -1)
#' }
#'
#' @seealso \code{\link{cnorm.cmp}}, \code{\link{autoselect.shash}}
#' @export
autoselect.cmp <- function(age,
                           score,
                           weights   = NULL,
                           max_mu    = 4,
                           max_nu    = 2,
                           min_mu    = 1,
                           min_nu    = 0,
                           nu        = 1,
                           control   = NULL,
                           scale     = "T",
                           max_terms = NULL,
                           tol       = 1e-12,
                           parallel  = TRUE,
                           n_cores   = NULL,
                           plot      = TRUE,
                           verbose   = TRUE) {

  # ---- Input validation -------------------------------------------------
  if (length(age) != length(score))
    stop("Length of 'age' and 'score' must be the same.")
  if (!is.null(weights) && length(weights) != length(age))
    stop("Length of 'weights' must match length of 'age' and 'score'.")
  if (max_mu < min_mu) stop("'max_mu' must be >= 'min_mu'.")
  if (max_nu < min_nu) stop("'max_nu' must be >= 'min_nu'.")
  if (min_mu < 1)
    stop("Minimum degree for mu must be >= 1.")
  if (min_nu < -1)
    stop("'min_nu' must be >= -1 (-1 means fixed nu, 0 means constant estimated nu).")
  if (nu <= 0) stop("'nu' must be > 0.")

  say <- function(...) {
    if (verbose) { cat(..., sep = ""); utils::flush.console() }
  }

  # ---- Build the grid ---------------------------------------------------
  grid <- expand.grid(mu = min_mu:max_mu,
                      nu = min_nu:max_nu,
                      KEEP.OUT.ATTRS = FALSE)
  pairs <- lapply(seq_len(nrow(grid)),
                  function(i) c(grid$mu[i], grid$nu[i]))
  say(sprintf(
    "Search grid: %d candidate models (mu %d:%d, nu %d:%d).\n",
    length(pairs), min_mu, max_mu, min_nu, max_nu
  ))

  # ---- Parallel setup with dev-mode fallback ---------------------------
  use_parallel <- FALSE
  cl <- NULL
  if (isTRUE(parallel)) {
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
              exists("cnorm.cmp",       envir = asNamespace("cNORM")) &&
              exists("diagnostics.cmp", envir = asNamespace("cNORM"))
          })
          all(vapply(res, isTRUE, logical(1)))
        }, error = function(e) FALSE)

        if (worker_ok) {
          use_parallel <- TRUE
          on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
          parallel::clusterExport(
            cl,
            varlist = c("age", "score", "weights", "nu", "control",
                        "scale", "max_terms", "tol"),
            envir = environment()
          )
          say(sprintf("Parallel mode: using %d cores.\n", n_cores))
        } else {
          try(parallel::stopCluster(cl), silent = TRUE); cl <- NULL
          say("Note: cNORM is not installed in the worker library path ",
              "(typical during devtools::load_all). ",
              "Falling back to sequential execution.\n")
        }
      }
    }
  }

  # ---- Worker --------------------------------------------------------
  fit_worker <- function(p) {
    # p = c(mu_deg, nu_deg); nu_deg == -1 -> fixed nu (nu_degree = NULL)
    dd <- if (p[2] < 0) NULL else p[2]
    tryCatch({
      m <- suppressMessages(suppressWarnings(
        cNORM::cnorm.cmp(
          age = age, score = score, weights = weights,
          mu_degree = p[1],
          nu_degree = dd,
          nu        = nu,
          control   = control,
          scale     = scale,
          max_terms = max_terms,
          tol       = tol,
          plot      = FALSE
        )
      ))
      d <- cNORM::diagnostics.cmp(m)
      list(mu = p[1], nu = p[2],
           model = m,
           BIC = if (is.finite(d$BIC)) d$BIC else Inf,
           AIC = d$AIC, logLik = d$log_likelihood,
           converged = isTRUE(d$converged),
           status  = if (!is.finite(d$BIC)) "error"
           else if (!isTRUE(d$converged)) "not_converged"
           else "ok",
           message = NA_character_)
    }, error = function(e) {
      list(mu = p[1], nu = p[2],
           model = NULL, BIC = Inf, AIC = NA_real_, logLik = NA_real_,
           converged = FALSE, status = "error",
           message = conditionMessage(e))
    })
  }

  fmt_nu <- function(d) if (d < 0) "fixed" else as.character(d)

  report <- function(r) {
    tag <- switch(r$status,
                  ok            = "",
                  not_converged = "  (not strictly converged)",
                  error         = paste0("  [error: ", r$message, "]"))
    say(sprintf("  mu=%d, nu=%s : BIC = %s%s\n",
                r$mu, fmt_nu(r$nu),
                formatC(r$BIC, digits = 3, format = "f"), tag))
  }

  # ---- Run the search --------------------------------------------------
  say(sprintf("Evaluating %d model%s ...\n",
              length(pairs), if (length(pairs) == 1) "" else "s"))

  all_res <- list()
  chunk_size <- if (use_parallel) n_cores else 1L
  chunks <- split(pairs, ceiling(seq_along(pairs) / chunk_size))
  for (chunk in chunks) {
    results <- if (use_parallel && length(chunk) > 1L)
      parallel::parLapply(cl, chunk, fit_worker)
    else
      lapply(chunk, fit_worker)
    for (r in results) {
      all_res[[length(all_res) + 1L]] <- r
      report(r)
    }
  }

  bics <- vapply(all_res, `[[`, numeric(1), "BIC")
  if (all(!is.finite(bics)))
    stop("Selection failed: no model produced a finite BIC. ",
         "Inspect the error messages above for per-fit information.")
  current <- all_res[[which.min(bics)]]

  # ---- Compile results --------------------------------------------------
  evaluated <- do.call(rbind, lapply(
    all_res,
    function(r) data.frame(mu = r$mu, nu = r$nu,
                           BIC = r$BIC, AIC = r$AIC,
                           logLik = r$logLik,
                           converged = r$converged,
                           status = r$status,
                           message = r$message,
                           stringsAsFactors = FALSE)))
  evaluated <- evaluated[order(evaluated$BIC), , drop = FALSE]
  rownames(evaluated) <- NULL

  say(sprintf("\nSelected model: mu=%d, nu=%s (BIC = %.3f)\n",
              current$mu, fmt_nu(current$nu), current$BIC))

  final_model <- current$model
  if (is.null(final_model))
    stop("Selection failed: the best candidate did not produce a usable model.")

  final_model$selection <- list(
    evaluated = evaluated,
    selected  = list(mu_degree = current$mu,
                     nu_degree = if (current$nu < 0) NULL else current$nu,
                     nu        = nu,
                     BIC       = current$BIC)
  )

  if (plot) print(plot(final_model, age = age, score = score, weights = weights))
  return(final_model)
}

#' Calculate Norm Tables for the Conway-Maxwell-Poisson Distribution
#'
#' Generates norm tables for specific ages based on a fitted CMP regression model.
#' Computes point probabilities, cumulative probabilities, percentile ranks, z-scores,
#' and norm scores for integer raw scores.
#'
#' @param model Fitted CMP model object of class "cnormCMP"
#' @param ages Numeric vector of age points for norm table generation
#' @param start Minimum raw score value for the norm table (default: observed minimum)
#' @param end Maximum raw score value for the norm table (default: observed maximum)
#' @param step Step size between consecutive raw scores (integer, default: 1)
#' @param CI Confidence coefficient (0-1, default: 0.9) for confidence intervals
#' @param reliability Reliability coefficient (0-1) for true score confidence intervals
#' @param mid_p Logical; if TRUE (default), uses mid-p adjusted percentiles for discrete scores
#' @param minRaw,maxRaw Optional aliases for \code{start} and \code{end}
#' @param ... Additional arguments
#'
#' @export
normTable.cmp <- function(model,
                          ages,
                          start = NULL,
                          end = NULL,
                          step = 1,
                          CI = .9,
                          reliability = NULL,
                          mid_p = TRUE,
                          minRaw = NULL,
                          maxRaw = NULL,
                          ...) {
  # Input validation
  if (!isCMP(model)) {
    stop("Wrong object. Please provide an object of class 'cnormCMP'.")
  }

  # Support minRaw and maxRaw as aliases
  if (is.null(start) && !is.null(minRaw)) start <- minRaw
  if (is.null(end) && !is.null(maxRaw))   end <- maxRaw

  if (is.null(start)) {
    start <- attr(model$result, "min")
  }
  if (is.null(end)) {
    end <- attr(model$result, "max")
  }

  # Ensure integer bounds for count data
  start <- max(0L, as.integer(ceiling(start)))
  end   <- as.integer(floor(end))

  if (start >= end) {
    stop("Start value must be less than end value.")
  }

  # For discrete counts, step must be an integer >= 1
  if (is.null(step) || step < 1) {
    step <- 1L
  } else {
    step <- as.integer(round(step))
  }

  if (is.null(CI) || is.na(CI)) {
    reliability <- NULL
  } else if (CI > .99999 || CI < .00001) {
    stop("Confidence coefficient (CI) out of range. Please specify a value between 0 and 1.")
  }

  # Setup reliability and confidence intervals
  rel <- FALSE
  if (!is.null(reliability)) {
    if (reliability > .9999 || reliability < .0001) {
      stop("Reliability coefficient out of range. Please specify a value between 0 and 1.")
    } else {
      se <- qnorm(1 - ((1 - CI) / 2)) * sqrt(reliability * (1 - reliability))
      rel <- TRUE
    }
  }

  # Get predicted CMP parameters for all requested ages
  predictions <- predictCoefficients_cmp(model, ages)

  # Discrete raw count sequence
  x <- seq(from = start, to = end, by = step)

  # Scale metrics
  mScale <- attr(model$result, "scaleMean")
  sdScale <- attr(model$result, "scaleSD")

  result <- vector("list", length(ages))

  # Generate norm table for each age
  for (k in seq_along(ages)) {
    cp <- cmp_cum_pmf(
      x,
      mu = rep(predictions$mu[k], length(x)),
      nu = rep(predictions$nu[k], length(x))
    )

    if (any(!cp$ok)) {
      warning("CMP series did not converge for age ", ages[k],
              "; results may be unreliable. Consider increasing 'max_terms'.")
    }

    Px <- cp$pmf
    cum <- cp$cdf

    # Mid-p percentile ranks: P(Y < x) + 0.5 * P(Y = x)
    perc <- if (mid_p) cp$cdf_prev + 0.5 * cp$pmf else cum

    # Clamp extreme probabilities to avoid non-finite z-scores
    z <- qnorm(pmin(pmax(perc, 1e-12), 1 - 1e-12))

    # Calculate norm scores
    norm <- rep(NA_real_, length(z))
    if (!is.na(mScale) && !is.na(sdScale)) {
      norm <- mScale + sdScale * z
    }

    df <- data.frame(
      x = x,
      Px = Px,
      Pcum = cum,
      Percentile = perc * 100,
      z = z,
      norm = norm
    )

    # Add Kelley's true score confidence intervals if reliability is provided
    if (rel) {
      zPredicted <- reliability * z
      df$lowerCI <- (zPredicted - se) * sdScale + mScale
      df$upperCI <- (zPredicted + se) * sdScale + mScale
      df$lowerCI_PR <- pmax(0, pmin(100, pnorm(zPredicted - se) * 100))
      df$upperCI_PR <- pmax(0, pmin(100, pnorm(zPredicted + se) * 100))
    }

    result[[k]] <- df
  }

  names(result) <- as.character(ages)
  return(result)
}

#' Summarize a CMP Continuous Norming Model
#'
#' This function provides a summary of a fitted Conway-Maxwell-Poisson continuous norming
#' model, including model fit statistics, convergence information, and parameter estimates.
#'
#' @param object An object of class "cnormCMP", typically the result of a call to
#'   \code{\link{cnorm.cmp}}.
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
#' The summary includes basic model information, model fit statistics (log-likelihood, AIC, BIC),
#' R-squared, RMSE and bias (if age and raw scores are provided), convergence information,
#' and parameter estimates with standard errors, z-values and p-values. Coefficients refer to
#' the log scale: log(mu) and log(nu).
#'
#' @examples
#' \dontrun{
#' model <- cnorm.cmp(speeded$age, speeded$raw)
#' summary(model)
#' summary(model, age = speeded$age, score = speeded$raw)
#' }
#'
#' @seealso \code{\link{cnorm.cmp}}, \code{\link{diagnostics.cmp}}
#' @export
summary.cnormCMP <- function(object, ...) {
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

  diag <- diagnostics.cmp(object, age, score, weights)

  cat("Conway-Maxwell-Poisson Continuous Norming Model\n")
  cat("-----------------------------------------------\n")
  cat("Polynomial degrees:\n")
  cat("  Location (log mu):", diag$mu_degree, "\n")
  if (!is.null(diag$nu_degree)) {
    cat("  Dispersion (log nu):", diag$nu_degree,
        ifelse(diag$nu_degree == 0, "(constant)", ""), "\n")
  } else {
    cat("  Dispersion (nu): Fixed at", diag$nu, "\n")
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
  cat("Location (mu) parameters (log scale):\n")
  mu_table <- data.frame(
    Estimate = diag$mu_estimates,
    `Std. Error` = diag$mu_se,
    `z value` = diag$mu_z_values,
    `Pr(>|z|)` = diag$mu_p_values,
    check.names = FALSE
  )
  rownames(mu_table) <- paste0("log(mu)_", 0:(length(diag$mu_estimates) - 1))
  print(mu_table, digits = 4)

  if (!is.null(diag$nu_estimates)) {
    cat("\n")
    cat("Dispersion (nu) parameters (log scale):\n")
    nu_table <- data.frame(
      Estimate = diag$nu_estimates,
      `Std. Error` = diag$nu_se,
      `z value` = diag$nu_z_values,
      `Pr(>|z|)` = diag$nu_p_values,
      check.names = FALSE
    )
    rownames(nu_table) <- paste0("log(nu)_", 0:(length(diag$nu_estimates) - 1))
    print(nu_table, digits = 4)
  }

  invisible(diag)
}

#' Diagnostic Statistics for CMP Continuous Norming Model
#'
#' This function computes detailed diagnostic statistics for a fitted CMP model,
#' including fit statistics, parameter estimates, and convergence information.
#'
#' @param object An object of class "cnormCMP"
#' @param age An optional numeric vector of age values for computing fit statistics
#' @param score An optional numeric vector of raw scores for computing fit statistics
#' @param weights An optional numeric vector of observation weights
#'
#' @return A list containing comprehensive diagnostic information
#'
#' @keywords internal
#' @export
diagnostics.cmp <- function(object,
                            age = NULL,
                            score = NULL,
                            weights = NULL) {
  if (!isCMP(object)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }

  # Basic model information
  n_obs <- attr(object$result, "N")
  n_params <- length(object$result$par)
  log_likelihood <- -object$result$value
  AIC <- 2 * n_params - 2 * log_likelihood
  BIC <- n_params * log(n_obs) - 2 * log_likelihood

  # Parameter estimates and standard errors
  mu_estimates <- object$mu_est
  nu_estimates <- object$nu_est

  se <- object$se
  if (is.null(se) || any(is.na(se))) {
    mu_se <- rep(NA, length(mu_estimates))
    nu_se <- if (!is.null(nu_estimates))
      rep(NA, length(nu_estimates))
    else
      NULL
  } else {
    n_mu <- length(mu_estimates)
    mu_se <- se[1:n_mu]
    if (!is.null(nu_estimates)) {
      n_nu <- length(nu_estimates)
      nu_se <- se[(n_mu + 1):(n_mu + n_nu)]
    } else {
      nu_se <- NULL
    }
  }

  # Calculate z-values and p-values
  mu_z_values <- ifelse(is.na(mu_se) |
                          mu_se == 0, NA, mu_estimates / mu_se)
  mu_p_values <- ifelse(is.na(mu_z_values), NA, 2 * (1 - pnorm(abs(mu_z_values))))

  if (!is.null(nu_estimates)) {
    nu_z_values <- ifelse(is.na(nu_se) |
                            nu_se == 0, NA, nu_estimates / nu_se)
    nu_p_values <- ifelse(is.na(nu_z_values), NA, 2 * (1 - pnorm(abs(nu_z_values))))
  } else {
    nu_z_values <- NULL
    nu_p_values <- NULL
  }

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
    max_gradient <- tryCatch({
      eigenvals <- eigen(object$result$hessian, only.values = TRUE)$values
      max(abs(eigenvals), na.rm = TRUE)
    }, error = function(e) {
      NA
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
    } else{
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
    nu_degree = object$nu_degree,
    nu = object$nu,
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
    nu_estimates = nu_estimates,

    # Standard errors
    mu_se = mu_se,
    nu_se = nu_se,

    # Test statistics
    mu_z_values = mu_z_values,
    nu_z_values = nu_z_values,
    mu_p_values = mu_p_values,
    nu_p_values = nu_p_values,

    # Convergence info
    converged = converged,
    n_evaluations = n_evaluations,
    max_gradient = max_gradient,
    message = message
  )
}

#' Predict Norm Scores from Raw Scores (CMP model)
#'
#' This function calculates norm scores based on raw scores, age, and a fitted cnormCMP model.
#'
#' @param object A fitted model object of class 'cnormCMP'.
#' @param ... Additional arguments passed to the prediction method:
#'   \itemize{
#'      \item age A numeric vector of ages, same length as score.
#'      \item score A numeric vector of raw scores (integers).
#'      \item range The range of the norm scores in standard deviations. Default is 3.
#'        Thus, scores in the range of +/- 3 standard deviations are considered.
#'      \item mid_p Logical; use mid-p percentile ranks, P(Y < x) + 0.5 * P(Y = x)
#'        (default TRUE), instead of P(Y <= x).
#'    }
#'
#' @return A numeric vector of norm scores.
#'
#' @details
#' The function predicts the CMP parameters (mu, nu) for each age using the provided
#' model, calculates the (mid-)percentile rank of each raw score under the corresponding
#' discrete distribution, and converts it to the norm scale specified in the model.
#' Extreme percentiles are mapped to the boundary of the norm score range
#' (mean +/- \code{range} standard deviations).
#'
#' @examples
#' \dontrun{
#' model <- cnorm.cmp(children$age, children$score)
#' norm_scores <- predict(model, c(7, 8, 9, 10), c(25, 30, 18, 45))
#' }
#'
#' @export
#' @family predict
predict.cnormCMP <- function(object, ...) {
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

  if (any(score < 0, na.rm = TRUE)) {
    stop("'score' contains negative values. Raw scores must be non-negative counts.")
  }

  if ("range" %in% names(args)) {
    range <- args$range
  } else {
    if (length(args) > 2)
      range <- args[[3]]
    else
      range <- 3
  }
  mid_p <- if ("mid_p" %in% names(args)) isTRUE(args$mid_p) else TRUE

  if (!isCMP(model)) {
    stop("Wrong object. Please provide object from class 'cnormCMP'.")
  }

  if (is.null(age) || is.null(score)) {
    stop("Both 'age' and 'score' must be provided.")
  }

  if (length(age) != length(score)) {
    stop("The lengths of 'age' and 'score' must be the same.")
  }

  if (any(abs(score - round(score)) > 1e-8, na.rm = TRUE)) {
    warning("Non-integer raw scores were rounded; the CMP model is defined for counts.")
  }
  score <- round(score)

  # Get predicted distribution parameters for each age
  predictions <- predictCoefficients_cmp(model, age)

  # (Mid-)percentile ranks under the discrete distribution
  cp <- cmp_cum_pmf(score, mu = predictions$mu, nu = predictions$nu)
  percentiles <- if (mid_p) cp$cdf_prev + 0.5 * cp$pmf else cp$cdf
  percentiles[!cp$ok] <- NA

  percentiles_c <- pmin(pmax(percentiles, 1e-12), 1 - 1e-12)
  z_scores <- qnorm(percentiles_c)

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
    return(percentiles * 100)
  }
}
