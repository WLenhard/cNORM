#' Weighted rank estimation
#'
#' Conducts weighted ranking on the basis of either the weighted Harrell-Davis quantile estimator
#' or an adaption of the type 7 quantile estimator of the generic quantile function in the base
#' package. Please provide a vector with raw values and an additional vector with the weight of each
#' observation. In case the weight vector is NULL, a normal ranking is done. The vectors may not
#' include NAs and the weights should be positive non-zero values.
#'
#' @param x A numerical vector
#' @param weights A numerical vector with weights; should have the same length as x
#' @param n Granularity for approximation
#' @param type Type of estimator, can either be "Harrell-Davis" (default) using a beta function to
#' approximate the weighted percentiles (Harrell & Davis, 1982) or "Type7" (Hyndman & Fan, 1996), an adaption
#' of the generic quantile function in R, including weighting. All code based on the work of Akinshin (2020).
#' @references
#' \enumerate{
#'   \item Harrell, F.E. & Davis, C.E. (1982). A new distribution-free quantile estimator. Biometrika, 69(3), 635-640.
#'   \item Hyndman, R. J. & Fan, Y. 1996. Sample quantiles in statistical packages, American Statistician 50, 361–365.
#'   \item Akinshin, A. (2020). Weighted quantile estimators. https://aakinshin.net/posts/weighted-quantiles/
#' }
#' @return the weighted ranks
#' @export
weighted.rank <- function(x, weights=NULL, n = 1000, type="Harrell-Davis"){
  if(is.null(weights))
    return(rank(x))

  probs <- seq(from = .5/n, to = (n - .5)/n, length.out = n)
  suppressWarnings(return(approx(weighted.quantile(x, probs, weights = weights, type), probs, x)$y * length(x)))
}


#' Weighted quantile estimator
#'
#' Computes weighted quantiles (code from Andrey Akinshin via https://aakinshin.net/posts/weighted-quantiles/
#' Code made available via the CC BY-NC-SA 4.0 license) on the basis of either the weighted Harrell-Davis
#' quantile estimator or an adaption of the type 7 quantile estimator of the generic quantile function in
#' the base package. Please provide a vector with raw values, the pobabilities for the quantiles and an
#' additional vector with the weight of each observation. In case the weight vector is NULL, a normal
#' quantile estimation is done. The vectors may not include NAs and the weights should be positive non-zero
#' values. Please draw on the computeWeights() function for retrieving weights in post stratification.
#'
#' @param x A numerical vector
#' @param probs Numerical vector of quantiles
#' @param type Type of estimator, can either be "Harrell-Davis" using a beta function to
#' approximate the weighted percentiles (Harrell & Davis, 1982) or "Type7" (default; Hyndman & Fan, 1996), an adaption
#' of the generic quantile function in R, including weighting. All code based on the work of Akinshin (2020).
#' @param weights A numerical vector with weights; should have the same length as x
#' @references
#' \enumerate{
#'   \item Harrell, F.E. & Davis, C.E. (1982). A new distribution-free quantile estimator. Biometrika, 69(3), 635-640.
#'   \item Hyndman, R. J. & Fan, Y. (1996). Sample quantiles in statistical packages, American Statistician 50, 361–365.
#'   \item Akinshin, A. (2020). Weighted quantile estimators. https://aakinshin.net/posts/weighted-quantiles/
#' }
#' @return the weighted quantiles
#' @export
weighted.quantile <- function(x, probs, weights = NULL, type="Type7"){
  if(is.null(weights)){
    return(quantile(x, probs))
  }else if(type=="Harrell-Davis"){
    return(weighted.quantile.harrell.davis(x, probs, weights))
  }else{
    return(weighted.quantile.type7(x, probs, weights))
  }
}

#' Weighted Harrell-Davis quantile estimator
#'
#' Computes weighted quantiles; code from Andrey Akinshin via https://aakinshin.net/posts/weighted-quantiles/
#' Code made available via the CC BY-NC-SA 4.0 license
#'
#' @param x A numerical vector
#' @param probs Numerical vector of quantiles
#' @param weights A numerical vector with weights; should have the same length as x.
#' If no weights are provided (NULL), it falls back to the base quantile function, type 7
#'
#' @return the quantiles
weighted.quantile.harrell.davis <- function(x, probs, weights = NULL) {
  if(is.null(weights)){
    return(quantile(x, probs = probs))
  }

  cdf.gen <- function(n, p) return(function(cdf.probs) {
    pbeta(cdf.probs, (n + 1) * p, (n + 1) * (1 - p))
  })
  wquantile.generic(x, probs, cdf.gen, weights)
}


#' Weighted type7 quantile estimator
#'
#' Computes weighted quantiles; code from Andrey Akinshin via https://aakinshin.net/posts/weighted-quantiles/
#' Code made available via the CC BY-NC-SA 4.0 license
#'
#' @param x A numerical vector
#' @param probs Numerical vector of quantiles
#' @param weights A numerical vector with weights; should have the same length as x.
#' If no weights are provided (NULL), it falls back to the base quantile function, type 7
#'
#' @return the quantiles
weighted.quantile.type7 <- function(x, probs, weights = NULL) {
  if(is.null(weights)){
    return(quantile(x, probs = probs))
  }

  cdf.gen <- function(n, p) return(function(cdf.probs) {
    h <- p * (n - 1) + 1
    u <- pmax((h - 1) / n, pmin(h / n, cdf.probs))
    u * n - h + 1
  })
  wquantile.generic(x, probs, cdf.gen, weights)
}

# Weighted generic quantile estimator
#
# Code made available via the CC BY-NC-SA 4.0 license from code from
# Andrey Akinshin via https://aakinshin.net/posts/weighted-quantiles/
wquantile.generic <- function(x, probs, cdf.gen, weights = NULL) {
  if(is.null(weights)){
    return(quantile(x, probs = probs))
  }

  n <- length(x)
  if (any(is.na(weights)))
    weights <- rep(1 / n, n)
  nw <- sum(weights) / max(weights)

  indexes <- order(x)
  x <- x[indexes]
  weights <- weights[indexes]

  weights <- weights / sum(weights)
  cdf.probs <- cumsum(c(0, weights))

  sapply(probs, function(p) {
    cdf <- cdf.gen(nw, p)
    q <- cdf(cdf.probs)
    w <- tail(q, -1) - head(q, -1)
    sum(w * x)
  })
}



#' Determine groups and group means
#'
#' Helps to split the continuous explanatory variable into groups and assigns
#' the group mean. The groups can be split either into groups of equal size (default)
#' or equal number of observations.
#'
#' @param x The continuous variable to be split
#' @param n The number of groups; if NULL then the function determines a number
#' of groups with usually 100 cases or 3 <= n <= 20.
#' @param equidistant If set to TRUE, builds equidistant interval, otherwise (default)
#' with equal number of observations
#'
#' @return vector with group means for each observation
#' @export
#'
#' @examples
#' x <- rnorm(1000, m = 50, sd = 10)
#' m <- getGroups(x, n = 10)
#'
getGroups <- function(x, n = NULL, equidistant = FALSE){
  if(is.null(n)){
    n <- length(x)/100

    if(n < 3)
      n <- 3
    else if(n > 20)
      n <- 20
  }

  # define grouping variable
  if(equidistant){
    groups <- as.numeric(cut(x, n))
  }else{
    groups <- findInterval(x, quantile(x, probs = seq(from = 0, to = 1, length.out = n + 1)), all.inside = TRUE)
  }

  # determine group means and assign to intervals
  return(ave(x, groups, FUN = function(x) {mean(x, na.rm = TRUE)}))
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
#' standard deviation. The development trajectories over age group are modeled by a curve linear
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
#' "even" for evenly distributed or a set of predefined values, which then overrides the item.n
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

simulate.weighting <- function(n1, m1, sd1, weight1, n2, m2, sd2, weight2){
  group1 <- data.frame(group=rep(1, length.out = n1), raw=rnorm(n1, mean=m1, sd=sd1), weights=rep(weight1, length.out = n1))
  group2 <- data.frame(group=rep(2, length.out = n2), raw=rnorm(n2, mean=m2, sd=sd2), weights=rep(weight2, length.out = n2))

  total.rep <- rbind(group1[1:(n1*weight1),], group2[1:(n2*weight2),])
  total.rep <- total.rep[sample(nrow(total.rep)),]
  total.rep$percentileReal <- rankByGroup(total.rep, group = FALSE, raw = total.rep$raw, descriptives = FALSE, na.rm = FALSE)$percentile

  total <- rbind(group1, group2)
  total <- total[sample(nrow(total)),]
  total <- total[1:nrow(total.rep),]
  total$percentileUnweighted <- rankByGroup(total, group = FALSE, raw = total$raw, descriptives = FALSE, na.rm = FALSE)$percentile
  total$percentileWeighted <- rankByGroup(total, group = FALSE, raw = total$raw, descriptives = FALSE,  weights = total$weights, na.rm = FALSE)$percentile


  total <- total[order(total$raw),]
  total.rep <- total.rep[order(total.rep$raw),]


  graphics::plot(total.rep$raw, total.rep$percentileReal, type = "l", lty = 1, main = "Simulated effects of weighted ranking", ylab = "Percentile", xlab = "Raw score")
  points(total$raw, total$percentileWeighted, type = "l", lty = 1, col="blue")
  points(total$raw, total$percentileUnweighted, type = "l", lty = 1, col="red")
  legend("bottomright", legend = c("Real percentile", "Weighted", "Unweighted"), col = c("black", "blue", "red"), pch = 19)

}
