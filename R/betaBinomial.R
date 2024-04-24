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
#' @param n The maximum number or the maximum possible value of `x`.
#'
#' @return A numeric vector containing the calculated parameters in the following order:
#' alpha (a), beta (b), mean (m), variance (var), and the maximum number (n).
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' n <- 5
#' betaCoefficients(x, n)
#'
#' @export
betaCoefficients <- function(x, n){
  m <- mean(x)
  var <- sd(x)^2

  m2 <- m*m
  m3 <- m2*m

  a <- (m2*n - m3 - m*var)/(n*var - n*m + m2)
  b <- a*((n - m)/m)

  return(c(a, b, m, var, n))
}


#' Calculate Cumulative Probabilities, Density, Percentiles, and Z-Scores for
#' Beta-Binomial Distribution
#'
#' This function computes the cumulative probabilities, the density (probability mass
#' function values), the percentiles, and the corresponding z-scores based on the specified
#' parameters of a beta-binomial distribution. The beta-binomial distribution is used to model
#' the number of successes in a fixed number of trials with success probability varying from
#' trial to trial, described by beta distribution parameters \eqn{\alpha} (alpha)
#' and \eqn{\beta} (beta).
#'
#' @param a The \eqn{\alpha} parameter of the beta distribution, indicating the shape parameter associated with successes.
#' @param b The \eqn{\beta} parameter of the beta distribution, indicating the shape parameter associated with failures.
#' @param n The number of trials in the beta-binomial distribution.
#' @param m An optional stop criterion in table generation. Positive integer lower than n.
#' @return A data frame with columns:
#' \describe{
#'   \item{x}{The number of successes (0 to n).}
#'   \item{Px}{The density (probability mass function value) for each number of successes.}
#'   \item{Pcum}{The cumulative probability up to each number of successes.}
#'   \item{Percentile}{The percentile corresponding to each number of successes.}
#'   \item{z}{The z-score corresponding to each percentile.}
#' }
#' @details
#' The function utilizes the gamma function to calculate factorial terms needed for the probability mass function (PMF)
#' and cumulative distribution function (CDF) calculations of the beta-binomial distribution. It iterates over the range
#' of possible successes (0 to n) to compute the PMF values (\eqn{Px}), cumulative probabilities (\eqn{Pcum}), and
#' mid-percentiles. These percentiles are then used to calculate the corresponding z-scores, which indicate how many
#' standard deviations an element is from the mean.
#' @examples
#' betaTable(2, 2, 45, 20)
#' @export
betaTable <- function(a, b, n, m = NULL){
  if(is.null(m))
    m <- n
  else if(m > n)
    m <- n

  fac1 <- gamma(a + b)/(gamma(a)*gamma(b)*gamma(a+b+n))
  comb <- choose(n, 0:m)
  x <- seq(from = 0, to = m)
  Px <- comb*fac1*gamma(a + x)*gamma(b+n-x)
  cum <- Px
  perc <- Px

  Px[Px < 0] <- 0
  Px[Px > 1] <- 1

  for(i in 1:length(Px)){ cum[i] <- sum(Px[1:i]) }

  cum[cum > 1] <- 1

  perc[1] <- Px[1]/2
  if(m>0) {
    for(i in 2:length(Px)){ perc[i] <- cum[i-1] + (Px[i]/2) }
  }

  z <- qnorm(perc)

  df <- data.frame(x = x, Px = Px, Pcum = cum, Percentile = perc, z = z)

  return(df)
}

#' Estimate Beta-Binomial Parameters by Group
#'
#' This function calculates the beta-binomial distribution parameters (alpha, beta, mean, variance)
#' for subsets of data grouped by a specified factor. It applies the `betaCoefficients` function
#' to each group separately, aggregating the results into a single data frame. This is particularly
#' useful for analyzing heterogeneity in success probabilities across different groups within a dataset.
#'
#' @param x A vector of non-negative integers representing the number of successes in trials for the entire dataset.
#' @param group A factor or similar object that divides `x` into groups. Each element of `x` is associated
#' with a group indicated by the corresponding element in `group`.
#' @param n The maximum number of trials, assumed to be the same for all groups.
#' @return A data frame where each row contains the beta-binomial distribution parameters (alpha `a`, beta `b`,
#' mean `m`, variance `var`) for a group, along with the group identifier. The columns are named `a`, `b`, `m`,
#' `var`, `n`, and `group`, with each row corresponding to a distinct group in the input.
#' @details
#' The function first identifies unique groups in the `group` argument and then iterates over these groups.
#' For each group, it extracts the subset of `x` corresponding to that group and computes the beta-binomial
#' distribution parameters using the `betaCoefficients` function. The results are compiled into a matrix that is
#' then converted into a data frame for easier manipulation and interpretation.
#' @examples
#' x <- elfe$raw
#' group <- elfe$group
#' n <- 26
#' betaByGroup(x, group, n)
#' @export
betaByGroup <- function(x, group, n){
  results <- matrix(data = NA, nrow = 0, ncol = 5)
  disctintGroups <- sort(unique(group))

  for(i in 1:length(disctintGroups)){
    x1 <- x[group==disctintGroups[i]]
    results <- rbind(results, betaCoefficients(x1, n))
  }

  results <- as.data.frame(results)
  colnames(results) <- c("a", "b", "m", "var", "n")
  results$group <- disctintGroups
  rownames(results) <- seq(from=1, to = nrow(results))

  return(results)
}

#' Continuous Norming with Beta-Binomial Distribution
#'
#' This function models the alpha (`a`) and beta (`b`) parameters of the beta-binomial distribution
#' across groups using polynomial regression. It then calculates the distribution's properties
#' (cumulative probabilities, density, percentiles, and z-scores) for these modeled parameters.
#' The modeling of `a` and `b` allows for the investigation of how these parameters vary with a continuous
#' group variable, allowing for continuous norming.
#'
#' @param param A data frame containing the columns `a`, `b`, `group`, and `n`. Each row should represent
#' a distinct group with its corresponding beta-binomial parameters and the group identifier. These
#' parameters can be obtained with the 'betaByGroup' function.
#' @param powerA The degree of the polynomial used to model the `a` parameter across groups. Please choose
#' \eqn{powerA \leq k} with k being the number of groups.
#' @param powerB The degree of the polynomial used to model the `b` parameter across groups. Please choose
#' \eqn{powerB \leq k} with k being the number of groups.
#' @return A list containing several components:
#' `manifestParameters` with the input parameters,
#' `powerA` and `powerB` showing the polynomial degrees used,
#' `modA` and `modB` with the polynomial regression models for `a` and `b` parameters.
#' @details
#' The function first fits polynomial regression models for `a` and `b` against a continuous group variable,
#' allowing for non-linear trends in how the shape parameters of the beta-binomial distribution change with the group.
#' It then predicts `a` and `b` for each group, using these predicted values to calculate the beta-binomial
#' distribution's properties for each group. This approach facilitates understanding the variability and
#' dynamics of the distribution across different conditions or groups.
#' @examples
#' param <- data.frame(a = c(1,2,3), b = c(2,3,4), group = c(1,2,3), n = c(30,30,30))
#' powerA <- 2
#' powerB <- 2
#' betaContinuous(param, powerA, powerB)
#' @export
betaContinuous <- function(param, powerA = Inf, powerB = Inf){
  aMod <- lm(a ~ poly(group, powerA, raw=TRUE), param)
  bMod <- lm(b ~ poly(group, powerB, raw=TRUE), param)

  if(powerA >= nrow(param))
    powerA <- nrow(param) - 1

  if(powerB >= nrow(param))
    powerB <- nrow(param) - 1

  param$aPred <- predict(aMod, param)
  param$bPred <- predict(bMod, param)
  n <- unique(param$n)

  df1 <- data.frame(x = numeric(),
                    Px = numeric(),
                    Pcum = numeric(),
                    Percentile = numeric(),
                    z = numeric(),
                    group = numeric())

  for(i in 1:nrow(param)){
    tab <- betaTable(param$aPred[i], param$bPred[i], n)
    tab$group <- rep(param[i, 6], nrow(tab))
    df1 <- rbind(df1, tab)
  }


  result <- list(beta.parameters = param, powerA = powerA, powerB = powerB, modA = aMod, modB = bMod, norms = df1)
  class(result) <- "cnormBetaBinomial"
  return(result)
}

#' Generate norm table from parametric continuous norming with Beta-Binomial Parameters
#'
#' This function generates a table of beta-binomial distribution properties (cumulative probabilities,
#' density, percentiles, and z-scores) for a specified group, using alpha (`a`) and beta (`b`) parameters
#' predicted by a model created with the `betaContinuous` function.
#'
#' @param model A list containing the components from a `betaContinuous` model output.
#' @param group A number specifying the group variable for which predictions and
#' subsequent beta-binomial distribution calculations are desired.
#' @param m An optional stop criterion in table generation. Positive integer lower than n
#' @return A data frame with columns representing the number of successes (`x`), the probability mass
#' function values (`Px`), cumulative probabilities (`Pcum`), percentiles (`Percentile`), and
#' z-scores (`z`) for the specified group based on the predicted `a` and `b` parameters.
#' @examples
#' # Determies beta parameters and models these continuously
#' param <- betaByGroup(elfe$raw, elfe$group, 26)
#' beta.model <- betaContinuous(param, 4, 4)
#'
#' # Calculates table for new group
#' newGroup <- 3.9
#' betaNormTable(beta.model, newGroup)
#' @export
betaNormTable <- function(model, group, m = NULL){
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  df1 <- data.frame(group = group)
  a <- predict.lm(model$modA, df1)
  b <- predict.lm(model$modB, df1)
  n <- unique(model$beta.parameters$n)

  if(is.null(m))
    m <- n
  else if(m > n)
    m <- n

  return(betaTable(a, b, n, m))
}


#'
#' @param model An `betaContinuous` model output
#' @param x A vector specifying the raw scores
#' @param group A vector specifying the group variables for each raw score
#' @return A data.frame with z scores and percentiles as well as predicted a and b values for the specific group
#' @examples
#' # Determies beta parameters and models these continuously
#' param <- betaByGroup(elfe$raw, elfe$group, 26)
#' beta.model <- betaContinuous(param, 4, 4)
#'
#' # Calculates z scores
#' x <- c(15, 8, 11, 18)
#' newGroup <- c(3.9, 1.2, 4.5, 6.3)
#'
#' predictBeta(beta.model, x, newGroup)
#' @export
predictBeta <- function(model, x, group){
  if (!inherits(model, "cnormBetaBinomial")) {
    stop("Wrong object. Please provide object from class 'cnormBetaBinomial'.")
  }

  n <- unique(model$beta.parameters$n)
  df1 <- data.frame(x = x, group = group)
  df1$a <- predict.lm(model$modA, df1)
  df1$b <- predict.lm(model$modB, df1)
  df1$z <- rep(NA, length(x))
  df1$Percentile <- rep(NA, length(x))

  for(i in 1:length(x)){
    tab <- betaTable(df1$a[i], df1$b[i], n, x[i])
    df1$Percentile[i] <- tab[nrow(tab), 4]
    df1$z[i] <- tab[nrow(tab), 5]
  }

  return(df1)
}
