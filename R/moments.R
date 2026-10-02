# ===========================================================================
# moments.R - Model-Implied Distributional Moments for cNORM Models
# ===========================================================================

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Gauss-Hermite nodes and weights (probabilist form)
#'
#' Computes nodes and weights via the Golub-Welsch algorithm, rescaled so
#' that E[g(Z)] with Z ~ N(0,1) is approximated by sum(w * g(z)).
#' The weights sum to 1; no external dependencies.
#'
#' @param n Number of quadrature nodes.
#' @return A list with elements \code{z} (nodes on the standard normal scale)
#'   and \code{w} (probability weights summing to 1).
#' @keywords internal
#' @noRd
.gaussHermite <- function(n) {
  i <- seq_len(n - 1)
  J <- matrix(0, n, n)
  J[cbind(i, i + 1)] <- J[cbind(i + 1, i)] <- sqrt(i / 2)
  e <- eigen(J, symmetric = TRUE)
  list(z = sqrt(2) * e$values,     # nodes on N(0,1) scale
       w = e$vectors[1, ]^2)       # weights, sum to 1
}

#' Evaluate polynomial via Horner's method
#'
#' @param x Numeric vector of evaluation points.
#' @param coeff Coefficients in ascending order (c0, c1, ..., ck).
#' @keywords internal
#' @noRd
.evalPoly <- function(x, coeff) {
  y <- rep(coeff[length(coeff)], length(x))
  if (length(coeff) > 1) {
    for (j in (length(coeff) - 1):1) y <- y * x + coeff[j]
  }
  y
}

#' Central moments from a discrete/weighted representation
#'
#' @param x Values (quadrature evaluations or discrete support points).
#' @param w Probability weights (must sum to 1).
#' @return Named list: mean, variance, sd, skewness, kurtosis (excess).
#' @keywords internal
#' @noRd
.weightedMoments <- function(x, w) {
  m  <- sum(w * x)
  d  <- x - m
  v  <- sum(w * d^2)
  if (v <= .Machine$double.eps^0.5 * max(1, m^2)) {
    return(list(mean = m, variance = max(v, 0), sd = sqrt(max(v, 0)),
                skewness = NA_real_, kurtosis = NA_real_))
  }
  m3 <- sum(w * d^3)
  m4 <- sum(w * d^4)
  list(mean     = m,
       variance = v,
       sd       = sqrt(v),
       skewness = m3 / v^1.5,
       kurtosis = m4 / v^2 - 3)     # excess kurtosis; 0 for the normal
}

#' Assemble result data.frame
#' @keywords internal
#' @noRd
.momentsDataFrame <- function(age, momentsList, method) {
  df <- data.frame(
    age      = age,
    mean     = vapply(momentsList, `[[`, numeric(1), "mean"),
    sd       = vapply(momentsList, `[[`, numeric(1), "sd"),
    variance = vapply(momentsList, `[[`, numeric(1), "variance"),
    skewness = vapply(momentsList, `[[`, numeric(1), "skewness"),
    kurtosis = vapply(momentsList, `[[`, numeric(1), "kurtosis"),
    row.names = NULL
  )
  attr(df, "method") <- method
  df
}

#' Input check for the age vector
#' @keywords internal
#' @noRd
.checkAge <- function(age) {
  if (!is.numeric(age) || length(age) == 0 || any(!is.finite(age)))
    stop("'age' must be a numeric vector with finite values.", call. = FALSE)
  invisible(age)
}


# ---------------------------------------------------------------------------
# Generic
# ---------------------------------------------------------------------------

#' Model-Implied Distributional Moments at Specific Ages
#'
#' Computes the mean, standard deviation, variance, skewness and (excess)
#' kurtosis of the raw score distribution implied by a fitted cNORM model
#' at one or more ages (or, more generally, values of the explanatory
#' variable). The moments are model-implied population moments of the
#' conditional raw score distribution, censored at the bounds of the raw
#' score range \code{[minRaw, maxRaw]} for consistency across model families.
#'
#' @details
#' The computation strategy depends on the model family:
#' \describe{
#'   \item{Taylor polynomial (\code{cnorm})}{The bivariate regression function
#'     is collapsed at the specified age into a univariate polynomial in the
#'     norm score (location) variable. Moments are then obtained by
#'     Gauss-Hermite quadrature, which is mathematically exact for
#'     polynomial quantile functions (up to the censoring at
#'     \code{minRaw}/\code{maxRaw}).}
#'   \item{Beta-binomial (\code{cnormBetaBinomial},
#'     \code{cnormBetaBinomial2})}{Moments are computed exactly by summation
#'     over the discrete probability mass function on the support
#'     \code{0:n}, using the age-specific predicted \eqn{\alpha} and
#'     \eqn{\beta} parameters. This respects the discreteness of the
#'     distribution; no continuity approximation is involved.}
#'   \item{Conway-Maxwell-Poisson (\code{cnormCMP})}{Moments are computed
#'     exactly by discrete summation over the probability mass function using
#'     the age-specific predicted \eqn{\mu} and \eqn{\nu} parameters. By default,
#'     the distribution is censored at \code{[minRaw, maxRaw]} to match the
#'     empirical test score range, consistent with the other methods.
#'     Uncensored theoretical moments can be computed with \code{censor = FALSE}.}
#'   \item{SHASH (\code{cnormShash})}{Moments are obtained by Gauss-Hermite
#'     quadrature of the quantile function \code{qshash} evaluated at the
#'     age-specific distribution parameters, censored at
#'     \code{minRaw}/\code{maxRaw}.}
#' }
#'
#' Kurtosis is reported as \emph{excess} kurtosis (0 for the normal
#' distribution).
#'
#' Note that the skewness and kurtosis of the \emph{censored} distribution
#' are reported by default. For well-fitting models whose raw score range covers the
#' probability mass of the conditional distribution, censoring effects are
#' negligible; for distributions with substantial floor or ceiling effects,
#' the censored moments are the substantively meaningful ones.
#'
#' @param model A model object of class \code{cnorm},
#'   \code{cnormBetaBinomial}, \code{cnormBetaBinomial2}, \code{cnormCMP},
#'   or \code{cnormShash}.
#' @param age A numeric vector of ages (values of the explanatory variable)
#'   at which to compute the moments.
#' @param ... Additional parameters passed to the methods, e.g.
#'   \code{nNodes} or \code{censor}.
#'
#' @return A \code{data.frame} with one row per age and the columns
#'   \code{age}, \code{mean}, \code{sd}, \code{variance}, \code{skewness}
#'   and \code{kurtosis} (excess). The computation method is stored in the
#'   attribute \code{"method"}.
#'
#' @examples
#' \dontrun{
#' # Taylor polynomial model
#' model <- cnorm(raw = elfe$raw, group = elfe$group)
#' predictMoments(model, age = c(2.25, 2.75, 3.25, 3.75, 4.25))
#'
#' # Beta-binomial model
#' bb <- cnorm.betabinomial(age = ppvt$age, score = ppvt$raw, n = 228)
#' predictMoments(bb, age = seq(4, 16, by = 2))
#'
#' # Conway-Maxwell-Poisson model (speeded tests)
#' cmp <- cnorm.cmp(age = speeded$age, score = speeded$raw)
#' predictMoments(cmp, age = c(7, 8, 9, 10))
#' }
#'
#' @references
#' Conway, R. W., & Maxwell, W. L. (1962). A queuing model with state dependent
#' service rates. Journal of Industrial Engineering, 12, 132-136.
#'
#' Isserlis, L. (1918). On a formula for the product-moment coefficient of
#' any order of a normal frequency distribution in any number of variables.
#' Biometrika, 12(1/2), 134-139.
#'
#' Jones, M. C. & Pewsey, A. (2009). Sinh-arcsinh distributions.
#' Biometrika, 96(4), 761-780.
#'
#' @seealso \code{\link{normTable}}, \code{\link{predictNorm}},
#'   \code{\link{predictRaw}}
#' @export
predictMoments <- function(model, age, ...) {
  UseMethod("predictMoments")
}


# ---------------------------------------------------------------------------
# Default method
# ---------------------------------------------------------------------------

#' @rdname predictMoments
#' @export
predictMoments.default <- function(model, age, ...) {
  stop("predictMoments is not defined for objects of class '",
       paste(class(model), collapse = "', '"),
       "'. Supported classes: cnorm, cnormBetaBinomial, cnormBetaBinomial2, ",
       "cnormCMP, cnormShash.", call. = FALSE)
}


# ---------------------------------------------------------------------------
# Taylor polynomial models
# ---------------------------------------------------------------------------

#' @rdname predictMoments
#' @param nNodes Number of Gauss-Hermite quadrature nodes (default 100).
#'   Only relevant for the Taylor and SHASH methods; ignored for
#'   discrete models (Beta-Binomial, CMP), which are computed exactly by summation.
#' @export
predictMoments.cnorm <- function(model, age, nNodes = 100, ...) {
  .checkAge(age)
  m <- model$model                        # inner regression model

  gh <- .gaussHermite(nNodes)
  L  <- m$scaleM + m$scaleSD * gh$z       # nodes on the norm score scale

  momentsList <- lapply(age, function(a) {
    cf <- calcPolyInLBase2(raw = 0, age = a,
                           coeff = m$coefficients, k = m$k)
    x  <- .evalPoly(L, cf)
    x  <- pmin(pmax(x, m$minRaw), m$maxRaw)   # censoring
    .weightedMoments(x, gh$w)
  })

  .momentsDataFrame(age, momentsList,
                    method = "Taylor polynomial / Gauss-Hermite quadrature")
}


# ---------------------------------------------------------------------------
# Beta-binomial models (exact, via pmf summation)
# ---------------------------------------------------------------------------

#' Retrieve age-specific alpha/beta parameters of a beta-binomial model
#'
#' @keywords internal
#' @noRd
.betaBinomialParams <- function(model, age) {
  if (inherits(model, "cnormBetaBinomial2")) {
    pars <- predictCoefficients2(model, age)   # mu/sigma parameterization
  } else {
    pars <- predictCoefficients(model, age)    # direct alpha/beta
  }
  if (!all(c("a", "b") %in% names(pars)))
    stop("Internal parameter prediction did not return columns 'a' and 'b'.",
         call. = FALSE)
  pars
}

#' Exact moments of the beta-binomial distribution via pmf summation
#'
#' Numerically stable through log-space computation of the pmf. Exact on
#' the discrete support 0:n; censoring is implicit since the support
#' coincides with the raw score range.
#'
#' @keywords internal
#' @noRd
.betaBinomialMoments <- function(a, b, n) {
  x    <- 0:n
  logp <- lchoose(n, x) + lbeta(x + a, n - x + b) - lbeta(a, b)
  p    <- exp(logp)
  p    <- p / sum(p)                      # guard against numeric drift
  .weightedMoments(x, p)
}

#' @rdname predictMoments
#' @export
predictMoments.cnormBetaBinomial <- function(model, age, ...) {
  .checkAge(age)

  n <- attr(model$result, "max")
  if (is.null(n))
    stop("Could not retrieve the number of items 'n' from the model object.",
         call. = FALSE)

  pars <- .betaBinomialParams(model, age)

  momentsList <- lapply(seq_along(age), function(i) {
    .betaBinomialMoments(pars$a[i], pars$b[i], n)
  })

  .momentsDataFrame(age, momentsList,
                    method = "Beta-binomial / exact pmf summation")
}

#' @rdname predictMoments
#' @export
predictMoments.cnormBetaBinomial2 <- predictMoments.cnormBetaBinomial


# ---------------------------------------------------------------------------
# Conway-Maxwell-Poisson models (exact, via pmf summation)
# ---------------------------------------------------------------------------

#' Exact moments of the Conway-Maxwell-Poisson distribution via pmf summation
#'
#' Computes moments by discrete summation over the support.
#' If censor = TRUE, mass below minRaw and above maxRaw is collapsed onto
#' the respective boundary point.
#'
#' @keywords internal
#' @noRd
.cmpMoments <- function(mu, nu, censor = TRUE, minRaw = 0, maxRaw = NULL) {
  if (!is.finite(mu) || !is.finite(nu) || mu <= 0 || nu <= 0) {
    return(list(mean = NA_real_, variance = NA_real_, sd = NA_real_,
                skewness = NA_real_, kurtosis = NA_real_))
  }

  if (isTRUE(censor)) {
    minRaw <- max(0L, as.integer(round(minRaw)))
    maxRaw <- as.integer(round(maxRaw))

    if (minRaw >= maxRaw) {
      return(list(mean = as.numeric(minRaw), variance = 0, sd = 0,
                  skewness = NA_real_, kurtosis = NA_real_))
    }

    x <- minRaw:maxRaw
    # Floor: P(Y <= minRaw)
    p_floor <- pcmp(minRaw, mu = mu, nu = nu, lower.tail = TRUE)
    # Ceiling: P(Y >= maxRaw) = P(Y > maxRaw - 1)
    p_tail  <- pcmp(maxRaw - 1L, mu = mu, nu = nu, lower.tail = FALSE)

    if (maxRaw > minRaw + 1L) {
      p_mid <- dcmp((minRaw + 1L):(maxRaw - 1L), mu = mu, nu = nu)
      w <- c(p_floor, p_mid, p_tail)
    } else {
      w <- c(p_floor, p_tail)
    }
  } else {
    # Uncensored: determine upper summation bound J capturing >= 1 - 1e-12 of mass
    J <- tryCatch({
      as.integer(ceiling(qcmp(1 - 1e-12, mu = mu, nu = nu)))
    }, error = function(e) NA_integer_)

    if (is.na(J) || !is.finite(J) || J < 1L) {
      # Analytical spread fallback based on CMP variance approximation mu / nu
      J <- as.integer(ceiling(mu + 12 * sqrt(mu / nu) + 50))
    }
    J <- max(J + 10L, 50L)

    x <- 0:J
    w <- dcmp(x, mu = mu, nu = nu)
  }

  w_sum <- sum(w)
  if (!is.finite(w_sum) || w_sum <= 0) {
    return(list(mean = NA_real_, variance = NA_real_, sd = NA_real_,
                skewness = NA_real_, kurtosis = NA_real_))
  }
  w <- w / w_sum   # guard against numerical drift

  .weightedMoments(x, w)
}

#' @rdname predictMoments
#' @param censor Logical; if \code{TRUE} (default), the distribution is censored
#'   at \code{[minRaw, maxRaw]} to reflect the empirical score limits. If \code{FALSE},
#'   uncensored theoretical population moments are calculated.
#' @param minRaw,maxRaw Optional custom score bounds for censoring. If \code{NULL} (default),
#'   they are retrieved from the fitted model attributes.
#' @export
predictMoments.cnormCMP <- function(model, age, censor = TRUE,
                                    minRaw = NULL, maxRaw = NULL, ...) {
  .checkAge(age)

  if (is.null(minRaw))
    minRaw <- attr(model$result, "min")
  if (is.null(maxRaw))
    maxRaw <- attr(model$result, "max")

  if (is.null(minRaw) || is.null(maxRaw)) {
    stop("Could not retrieve score bounds (min/max) from the model object.",
         call. = FALSE)
  }

  pars <- predictCoefficients_cmp(model, age)

  momentsList <- lapply(seq_along(age), function(i) {
    .cmpMoments(mu = pars$mu[i], nu = pars$nu[i],
                censor = censor, minRaw = minRaw, maxRaw = maxRaw)
  })

  method_label <- if (censor) {
    "Conway-Maxwell-Poisson / censored discrete pmf summation"
  } else {
    "Conway-Maxwell-Poisson / exact discrete pmf summation"
  }

  .momentsDataFrame(age, momentsList, method = method_label)
}


# ---------------------------------------------------------------------------
# SHASH models
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.shashParams <- function(model, age) {
  pars <- predictCoefficients_shash(model, age)
  needed <- c("mu", "sigma", "epsilon", "delta")
  if (!all(needed %in% names(pars)))
    stop("predictCoefficients_shash did not return columns ",
         paste(needed, collapse = ", "), ".", call. = FALSE)
  pars
}

#' @rdname predictMoments
#' @export
predictMoments.cnormShash <- function(model, age, nNodes = 100, ...) {
  .checkAge(age)

  minRaw <- attr(model$result, "min")
  maxRaw <- attr(model$result, "max")
  if (is.null(minRaw) || is.null(maxRaw))
    stop("Could not retrieve minRaw/maxRaw from the model object.",
         call. = FALSE)

  gh   <- .gaussHermite(nNodes)
  p    <- pnorm(gh$z)
  pars <- .shashParams(model, age)

  momentsList <- lapply(seq_along(age), function(i) {
    x <- qshash(p,
                mu      = pars$mu[i],
                sigma   = pars$sigma[i],
                epsilon = pars$epsilon[i],
                delta   = pars$delta[i])
    # Censoring also neutralizes +/-Inf from p ~ 0 or 1 at extreme nodes
    x <- pmin(pmax(x, minRaw), maxRaw)
    .weightedMoments(x, gh$w)
  })

  .momentsDataFrame(age, momentsList,
                    method = "SHASH / Gauss-Hermite quadrature")
}
