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
  return(1.5*age - 0.05 * age^2 + 0.0001 * age^4)
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
  return((1.5*age - 0.05 * age^2 + 0.0001 * age^4)*0.2+1)
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
#' @param width The width of the window size for the continuous age per group; +- 1/2 width around group
#' center
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
#' # simulate data for a rather easy test (m = -1.0)
#' sim <- simulateRasch(n=150, minAge=1,
#'                      maxAge=7, items.n = 30, items.m = -1.0,
#'                      items.sd = 1, randomTheta = TRUE, width = 1.0)
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
simulateRasch <- function(n = 100, minAge = 1, maxAge=7, items.n = 21, items.m = 0, items.sd = 1, randomTheta = TRUE, width = 1){
  # draw sample
  groups <- seq.int(from = minAge, to = maxAge)
  latent <- vector(mode="numeric", length=0)
  age <- vector(mode="numeric", length=0)
  group <- vector(mode="numeric", length=0)
  i <- 1

  while(i <= length(groups)){
    group <- c(group, rep(groups[i], times = n))
    age <- c(age, runif(n, min = groups[i] - (width/2), max = groups[i] + (width/2)))

    i <- i + 1
  }

  data <- data.frame(age, group, mean = simMean(age), sd = simSD(age))
  data$z <- stats::rnorm(nrow(data))
  data$latent <- data$z * data$sd + data$mean

  meanL <- mean(data$latent)
  sdL <- stats::sd(data$latent)

  # compute standardized value over complete sample
  data$zOverall <- (data$latent - meanL)/sdL


  # generate item difficulties either randomly or evenly distributed
  if(randomTheta == TRUE){
    theta <- stats::rnorm(items.n, items.m, items.sd)
  }else{
    p <- seq(from = 0.5 / items.n, to = (items.n - 0.5) / items.n, length.out = items.n)
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
