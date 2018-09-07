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
#' @param minRaw the lower bound of raw scores
#' @param maxRaw the upper bound of raw scores
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


#' Simulate mean per age
#'
#' @param age the age variable
#'
#' @return return predicted means
#'
#' @examples
#' \dontrun{
#' x <- simMean(a)
#' }
simMean <- function(age){
  return(0.82153 * age - 0.054437 * age^2 + 0.0001 * age^4)
}

#' Simulate sd per age
#'
#' @param age the age variable
#'
#' @return return predicted sd
#'
#' @examples
#' \dontrun{
#' x <- simSD(a)
#' }
simSD <- function(age){
  return((age * 0.25 + 1)^0.5)
}

#' Simulate raw test scores based on Rasch model
#'
#' The function simulates raw test scores based on a virtual Rasch based test with n results per
#' age group, an evenly distributed age variable, items.n test items with a simulated difficulty and
#' standard deviation. The development trajectories over age group are modelled by a curve linear
#' function of age, with at first fast progression, which slows down over age, and a slightly increasing
#' standard deviation in order to model a scissor effects. The item difficulties can be accessed via $theta
#' and the raw data via $data of the returned object.
#'
#' @param n The sample size per age group
#' @param minAge The minimum age (default 1)
#' @param maxAge The maximum age (default 7)
#' @param items.n The number of items of the test
#' @param items.m The mean difficulty of the items
#' @param items.sd The standard deviation of the item difficulty
#' @param randomTheta If set to true, a random item sample with normally distributed difficulty is drawn, based
#' on items.m and item.sd; if set to FALSE, the distribution is not drawn randomly but normally nonetheless
#'
#' @return a list containing the simulated data and thetas
#' \describe{
#'   \item{data}{the data.frame with only age, group and raw}
#'   \item{sim}{the complete simulated data with item level results}
#'   \item{theta}{the difficulty of the items}
#' }
#' @export
#'
#' @examples
simulateRasch <- function(n = 100, minAge = 1, maxAge=7, items.n = 21, items.m = 0, items.sd = 1, randomTheta = TRUE){
  # draw sample
  groups <- seq.int(from = minAge, to = maxAge)
  latent <- vector(mode="numeric", length=0)
  age <- vector(mode="numeric", length=0)
  group <- vector(mode="numeric", length=0)
  i <- 1

  while(i <= length(groups)){
    group <- c(group, rep(groups[i], times = n))
    age <- c(age, runif(n, min = groups[i] - 0.5, max = groups[i] + 0.5))

    i <- i + 1
  }

  data <- data.frame(age, group, mean = simMean(age), sd = simSD(age))
  data$z <- rnorm(nrow(data))
  data$latent <- data$z * data$sd + data$mean

  meanL <- mean(data$latent)
  sdL <- sd(data$latent)

  data$zOverall <- (data$latent - meanL)/sdL


  if(randomTheta == TRUE){
    theta <- rnorm(items.n, items.m, items.sd)
  }else{
    p <- seq(0, 1, length.out = items.n + 2)
    p <- p[3:length(p)-1]
    theta <- stats::qnorm(p, items.m, items.sd)
  }


  # compute propabilities
  i <- 1
  while(i <= items.n){
    prob <- exp(data$zOverall - theta[i])/(1 + exp(data$zOverall - theta[i]))
    name <- paste0("prob", i)
    data <- cbind(data, prob)
    names(data)[[ncol(data)]] <- name

    i <- i + 1
  }

  data <- transform(data, expected=rowSums(data[, 8:items.n + 7]))

  i <- 1
  # compute propabilities
  while(i <= items.n){
    rand <- runif(length(data$latent))
    item <- round((sign(data[, i + 7] - rand) + 1)/2)
    name <- paste0("item", i)
    data <- cbind(data, item)
    names(data)[[ncol(data)]] <- name

    i <- i + 1
  }
  sim <- transform(data, raw=rowSums(data[, (9 + items.n):ncol(data)]))
  dat <- sim[, c(1,2,ncol(sim))]
  return(list(data = dat, sim = sim, theta = theta))
}
