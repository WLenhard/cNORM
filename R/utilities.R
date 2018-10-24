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
simMean <- function(age) {
  return(1.5 * age - 0.05 * age^2 + 0.0001 * age^4)
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
simSD <- function(age) {
  return((1.5 * age - 0.05 * age^2 + 0.0001 * age^4) * 0.2 + 1)
}

#' Simulate raw test scores based on Rasch model
#'
#' For testing purposes only:
#' The function simulates raw test scores based on a virtual Rasch based test with n results per
#' age group, an evenly distributed age variable, items.n test items with a simulated difficulty and
#' standard deviation. The development trajectories over age group are modelled by a curve linear
#' function of age, with at first fast progression, which slows down over age, and a slightly increasing
#' standard deviation in order to model a scissor effects. The item difficulties can be accessed via $theta
#' and the raw data via $data of the returned object.
#'
#' @param data data.frame from previous simulations for recomputation (overrides n, minAge, maxAge)
#' @param n The sample size per age group
#' @param minAge The minimum age (default 1)
#' @param maxAge The maximum age (default 7)
#' @param items.n The number of items of the test
#' @param items.m The mean difficulty of the items
#' @param items.sd The standard deviation of the item difficulty
#' @param Theta irt scales difficulty parameters, either "random" for drawing a random sample,
#' "even" for evently distributed or a set auf predefined values, which then overides the item.n
#' parameters
#' @param width The width of the window size for the continuous age per group; +- 1/2 width around group
#' center
#' on items.m and item.sd; if set to FALSE, the distribution is not drawn randomly but normally nonetheless
#' @export
#' @return a list containing the simulated data and thetas
#' \describe{
#'   \item{data}{the data.frame with only age, group and raw}
#'   \item{sim}{the complete simulated data with item level results}
#'   \item{theta}{the difficulty of the items}
#' }
#'
#' @examples
#' # simulate data for a rather easy test (m = -1.0)
#' sim <- simulateRasch(n=150, minAge=1,
#'                      maxAge=7, items.n = 30, items.m = -1.0,
#'                      items.sd = 1, Theta = "random", width = 1.0)
#'
#' # Show item difficulties
#' mean(sim$theta)
#' sd(sim$theta)
#' hist(sim$theta)
#'
#' # Plot raw scores
#' boxplot(raw~group, data=sim$data)
#'
#' # Model data
#' data <- prepareData(sim$data, age="age")
#' model <- bestModel(data, k = 4)
#' printSubset(model)
#' plotSubset(model, type=0)
simulateRasch <- function(data = NULL, n = 100, minAge = 1, maxAge = 7, items.n = 21, items.m = 0, items.sd = 1, Theta = "random", width = 1) {
  # draw sample
  if (is.null(data)) {
    groups <- seq.int(from = minAge, to = maxAge)
    latent <- vector(mode = "numeric", length = 0)
    age <- vector(mode = "numeric", length = 0)
    group <- vector(mode = "numeric", length = 0)
    i <- 1

    while (i <= length(groups)) {
      group <- c(group, rep(groups[i], times = n))
      age <- c(age, runif(n, min = groups[i] - (width / 2), max = groups[i] + (width / 2)))

      i <- i + 1
    }

    data <- data.frame(age, group, mean = simMean(age), sd = simSD(age))
    data$z <- rnorm(nrow(data))
    data$latent <- data$z * data$sd + data$mean

    meanL <- mean(data$latent)
    sdL <- sd(data$latent)

    # compute standardized value over complete sample
    data$zOverall <- (data$latent - meanL) / sdL
  }

  # generate item difficulties either randomly or evenly distributed
  if (is.character(Theta)) {
    if (Theta == "random") {
      theta <- rnorm(items.n, items.m, items.sd)
    } else if (Theta == "even") {
      p <- seq(from = 0.5 / items.n, to = (items.n - 0.5) / items.n, length.out = items.n)
      theta <- qnorm(p, items.m, items.sd)
    }
  } else {
    theta <- Theta
    items.n <- length(Theta)
  }


  # compute propabilities
  i <- 1
  while (i <= items.n) {
    # prob <- exp(data$zOverall - theta[i])/(1 + exp(data$zOverall - theta[i]))
    prob <- pnorm(data$zOverall - theta[i]) # use real cumulative normal instead of logit
    name <- paste0("prob", i)
    data <- cbind(data, prob)
    names(data)[[ncol(data)]] <- name

    i <- i + 1
  }

  data <- transform(data, expected = rowSums(data[, 8:items.n + 7]))

  i <- 1
  # compute propabilities
  while (i <= items.n) {
    rand <- runif(length(data$latent))
    item <- round((sign(data[, i + 7] - rand) + 1) / 2)
    name <- paste0("item", i)
    data <- cbind(data, item)
    names(data)[[ncol(data)]] <- name

    i <- i + 1
  }
  sim <- transform(data, raw = rowSums(data[, (9 + items.n):ncol(data)]))
  dat <- sim[, c(1, 2, 5, ncol(sim))]
  return(list(data = dat, sim = sim, theta = theta))
}
