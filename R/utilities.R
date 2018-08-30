#' Internal method for creating a skewed normal distribution
#'
#' The function generates a skewed normal distribution based on the SN
#' package by Adelchi Azzalini (http://azzalini.stat.unipd.it/SN/). It
#' is used for internal testing purposes.
#' @param n number of observations
#' @param location mean of the distribution
#' @param scale the standard deviation
#' @param slant Measure for skewness
#' @return a skewed normal distribution
#' @references Azzalini, A. with the collaboration of Capitanio, A. (2014).
#' The Skew-Normal and Related Families. Cambridge University Press,
#' IMS Monographs series.
skewedNormal <- function(n, location, scale, slant = 10) {
  u1 <- stats::rnorm(n)
  u2 <- stats::rnorm(n)
  id <- (u2 > slant * u1)
  u1[id] <- (-u1[id])
  return(as.vector(location + scale * u1))
}

#' Simulate curve linear and skewed sample data
#'
#' @param n number of observations
#' @param minAge the lower age bound
#' @param maxAge the upper age bound
#' @param minRaw the lower bound of raw values
#' @param maxRaw the upper bound of raw values
#' @param nGroups number of samples, that should be drawn
#' @param pow Power factor. Values < 1 represent root functions (e. g. pow = .5)
#' @param slant Measure for skewness within each sample. Use 0 for symetric normal distributions,
#' values > 1 for positively skewed data (e. g. 10), values lower than 0 for negatively skewed
#' distributions. The according function is based on the SN package by Azzalini (2014)
#' @param k The power paramerer for the computePowers and bestMode function
#' @return The simulated sample data set with ranking and powers already applied
#' @references Azzalini, A. with the collaboration of Capitanio, A. (2014). The Skew-Normal and Related Families. Cambridge University Press, IMS Monographs series.
#' @export
#' @examples
#' sample <- simData(100, minAge = 1, maxAge = 4, nGroups = 4)
#' model <- bestModel(sample)
#' plotPercentiles(sample, model)
simData <- function(n, minAge = 1, maxAge = 4, minRaw = 0, maxRaw = 50, nGroups = 4, pow = 1, slant = 0, k = 4) {

  # first generate linear distribution of groups, mean and sd
  groups <- seq(minAge, maxAge, length = nGroups)

  min <- (maxRaw - minRaw) / (nGroups + 1)
  max <- (maxRaw - minRaw) * (nGroups / (nGroups + 1))
  m <- seq(min, max, length = nGroups)
  sd <- rep(min / 2, nGroups)

  fac <- (m / max)^pow
  m <- fac * max
  sd <- sd * fac

  i <- 1
  sample <- data.frame(matrix(ncol = 2, nrow = 0))
  names(sample) <- c("group", "raw")

  # generate sample data, so far drawn from normal distribution
  while (i <= nGroups) {
    f <- data.frame(matrix(ncol = 2, nrow = n))
    names(f) <- c("group", "raw")

    f$group <- rep(groups[[i]], n)
    f$raw <- skewedNormal(n, m[[i]], sd[[i]], slant = slant)
    sample <- rbind(sample, f)
    i <- i + 1
  }

  # restrict value range
  sample$raw[sample$raw < minRaw] <- minRaw
  sample$raw[sample$raw > maxRaw] <- maxRaw

  # ranking and powers
  sample <- cNORM::rankByGroup(sample)
  sample <- cNORM::computePowers(sample, k)


  return(sample)
}

getPlotSeries <- function(data){
  model <- bestModel(data)
  length <- length(model$subsets$adjr2)
  i <- 1
  l <- vector("list", length)

  while(i <= length){
    m <- bestModel(data, terms = i)
    l[[i]] <- plotPercentiles(data, m, minRaw=-Inf, maxRaw=Inf)
    i <- i + 1
  }
}
