#' Plot manifest and fitted raw scores
#'
#' The function plots the raw data against the fitted scores from
#' the regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' regression line.
#' @param data The raw data within a data.frame
#' @param model The regression model
#' @param group The grouping variable
#' @param raw The raw score variable
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotValues(normData, m, group="group", raw="raw")
#' @export
plotValues <- function(data, model, group = "group", raw = "raw") {
  if (!(group %in% colnames(data))) {
    stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
  }
  if (!(raw %in% colnames(data))) {
    stop(paste(c("ERROR: Raw variable '", raw, "' does not exist in data object."), collapse = ""))
  }


  d <- data
  d$fitted <- model$fitted.values
  d$group <- data[[group]]
  d$group <- as.factor(d$group)
  d$raw <- data[raw]
  lattice::xyplot(fitted ~ raw | group, d,
    main = paste("Manifest vs. Fitted Raw Scores by ", group),
    ylab = "Fitted Scores",
    xlab = "Manifest Scores",
    grid = TRUE,
    auto.key = TRUE,
    abline = c(0, 1), lwd = 1
  )
}

#' Plot norm curves
#'
#' The function plots the norm curves based on the regression model.
#' Please check the function for inconsistent curves: The different
#' curves should not intersect. Violations of this assumption are a strong
#' indication for problems
#' in modeling the relationship between raw and norm scores. There are
#' several reasons, why this might occur:
#' \enumerate{
#'   \item Vertical extrapolation: Choosing extreme norm scores, e. g. scores
#'   -3 <= x and x >= 3 In order to model these extreme scores, a large sample
#'   dataset is necessary.
#'   \item Horizontal extrapolation: Taylor polynomials converge in a certain
#'   radius. Using the model scores outside the original dataset may
#'   lead to inconsistent results.
#'   \item The data cannot be modeled with Taylor polynomials, or you need
#'   another power parameter (k) or R2 for the model.
#'  }
#' In general, extrapolation (point 1 and 2) can carefully be done to a
#' certain degree outside the original sample, but it should in general
#' be handled with caution.
#' checkConsistency and derivationPlot can be used to further inspect the model.
#' @param model The model from the bestModel function
#' @param normList Vector with norm scores to display
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param step Stepping parameter for the age check, usually 1 or 0.1; lower
#' scores indicate higher precision / closer checks
#' @param minRaw Lower end of the raw score range, used for clipping implausible results
#' (default = 0)
#' @param maxRaw Upper end of the raw score range, used for clipping implausible results
#' @seealso checkConsistency, derivationPlot, plotPercentiles
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotNormCurves(m, minAge=2, maxAge=5)
#' @export
plotNormCurves <- function(model, normList = c(30, 40, 50, 60, 70),
                           minAge = 2,
                           maxAge = 5,
                           step = 0.1,
                           minRaw = 0,
                           maxRaw = 1000) {
  valueList <- data.frame(n = factor(), raw = double(), age = double())
  n <- length(normList)


  for (i in 1:n) {
    normCurve <-
      cNORM::getNormCurve(
        normList[[i]],
        model,
        minAge = minAge,
        maxAge = maxAge,
        step = step,
        minRaw = minRaw,
        maxRaw = maxRaw
      )

    currentDataFrame <- data.frame(n = normCurve$norm, raw = normCurve$raw, age = normCurve$age)
    valueList <- rbind(valueList, currentDataFrame)
  }

  # generate variable names
  NAMES <- paste("Norm ", normList, sep = "")

  # lattice display options
  COL <- grDevices::rainbow(length(normList))
  panelfun <- function(..., type, group.number) {
    lattice::panel.lines(...)
  }

  lattice::xyplot(raw ~ age,
    data = valueList, groups = n,
    panel = function(...)
      lattice::panel.superpose(..., panel.groups = panelfun),
    main = "Norm Curves",
    ylab = "Raw Score", xlab = "Age",
    col = COL, lwd = 2, grid = TRUE,
    key = list(
      corner = c(0.99, 0.1),
      lines = list(col = COL, lwd = 2),
      text = list(NAMES)
    )
  )
}




#' Plot norm curves against actual percentiles
#'
#' The function plots the norm curves based on the regression model against
#' the actual percentiles from the raw data. As in 'plotNormCurves',
#' please check for inconsistent curves, especially intersections.
#' Violations of this assumption are a strong
#' indication for problems
#' in modeling the relationship between raw and norm scores.
#' In general, extrapolation (point 1 and 2) can carefully be done to a
#' certain degree outside the original sample, but it should in general
#' be handled with caution.
#' The original percentiles are displayed as distinct points in the according
#' color, the model based projection of percentiles are drawn as lines.
#' Please note, that the estimation of the percentiles of the raw data is done with
#' the stats::quantile function with the default settings. Please consult help(quantile)
#' and change the 'type' parameter accordingly.
#' @param data The raw data including the percentiles and norm scores
#' @param model The model from the bestModel function
#' @param minRaw Lower bound of the raw score (default = 0)
#' @param maxRaw Upper bound of the raw score
#' @param minAge Variable to restrict the lower bound of the plot to a specific age
#' @param maxAge Variable to restrict the upper bound of the plot to a specific age
#' @param raw The name of the raw variable
#' @param group The name of the grouping variable; the distinct groups are automatically
#' determined
#' @param percentiles Vector with percentile scores, ranging from 0 to 1 (exclusive)
#' @param scale The norm scale, either 'T' (default), 'IQ', 'z', 'percentile' or
#' self defined with a double vector with the mean and standard deviation,
#' f. e. c(10, 3) for Wechsler scale index points
#' @param type The type parameter of the quantile function to estimate the percentiles
#' of the raw data (default 7)
#' @seealso plotNormCurves
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotPercentiles(normData, m, raw="raw", group="group")
#' @export
plotPercentiles <- function(data,
                            model,
                            minRaw = 0,
                            maxRaw = 1000,
                            minAge = NULL,
                            maxAge = NULL,
                            raw = "raw",
                            group = "group",
                            percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                            scale = "T",
                            type = 7) {
  if (!(raw %in% colnames(data))) {
    stop(paste(c("ERROR: Raw score variable '", raw, "' does not exist in data object."), collapse = ""))
  }

  if (!(group %in% colnames(data))) {
    stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
  }

  if(is.null(minAge)){
    minAge <- model$minA1
  }


  if(is.null(maxAge)){
    maxAge <- model$maxA1
  }

  # compute norm scores from percentile vector
  if ((typeof(scale) == "double" && length(scale) == 2)) {
    T <- stats::qnorm(percentiles, scale[1], scale[2])
  } else if (scale == "IQ") {
    T <- stats::qnorm(percentiles, 100, 15)
  } else if (scale == "z") {
    T <- stats::qnorm(percentiles)
  } else if (scale == "T") {
    T <- stats::qnorm(percentiles, 50, 10)
  } else {
    # no transformation
    T <- percentiles
  }

  # generate variable names
  NAMES <- paste("PR", percentiles * 100, sep = "")
  NAMESP <- paste("PredPR", percentiles * 100, sep = "")

  # build function for xyplot and aggregate actual percentiles per group
  if (typeof(group) == "logical" && !group) {
    message("The plotPercentiles-function does not work without a grouping variable.")
  } else {
    xyFunction <- paste(paste(NAMES, collapse = " + "),
      paste(NAMESP, collapse = " + "),
      sep = " + ", collapse = " + "
    )
    xyFunction <- paste(xyFunction, group, sep = " ~ ")
    percentile.actual <- do.call(
      data.frame,
      stats::aggregate(data[, raw],
        list(data[, group]),
        FUN = function(x) stats::quantile(x,
            probs = percentiles,
            type = type
          )
      )
    )
  }
  # compute percetile table
  colnames(percentile.actual) <- c(c(group), NAMES)

  # build finer grained grouping variable for prediction
  gr <- percentile.actual[, group]
  leng <- length(gr)
  FIRST <- gr[[1]]
  LAST <- gr[[leng]]
  AGEP <- seq(FIRST, LAST, length.out = leng * 2 - 1)

  # fitt predicted percentiles
  percentile.fitted <- data.frame(matrix(NA,
    nrow = length(AGEP),
    ncol = length(T) + 1
  ))
  percentile.fitted[, 1] <- AGEP
  colnames(percentile.fitted) <- c(c(group), NAMESP)

  i <- 1
  while (i <= length(AGEP)) {
    j <- 1

    while (j <= length(T)) {
      percentile.fitted[i, j + 1] <- cNORM::predictRaw(
        T[[j]],
        AGEP[[i]],
        model$coefficients,
        minRaw, maxRaw
      )

      j <- j + 1
    }
    i <- i + 1
  }

  # merge actual and predicted scores und plot them show lines
  # for predicted scores and dots for actual scores
  percentile <- merge(percentile.actual, percentile.fitted,
    by = group, all.y = TRUE
  )

  END <- 5 / 6
  COL1 <- grDevices::rainbow(length(percentiles), end = END)
  COL2 <- c(grDevices::rainbow(length(percentiles), end = END), grDevices::rainbow(length(percentiles), end = END))

  panelfun <- function(..., type, group.number) {
    if (group.number > length(T)) {
      lattice::panel.lines(...)
    } else {
      lattice::panel.points(..., type = "p")
    }
  }

  plot <- lattice::xyplot(stats::formula(xyFunction), percentile,
    panel = function(...)
      lattice::panel.superpose(..., panel.groups = panelfun),
    main = "Manifest and Fitted Percentile Curves",
    ylab = paste0("Raw Score (", raw, ")"), xlab = paste0("Explanatory Variable (", group, ")"),
    col = COL2, lwd = 2, grid = TRUE,
    key = list(
      corner = c(0.99, 0.01),
      lines = list(col = COL1, lwd = 2),
      text = list(NAMES)
    )
  )

  print(plot)
  return(plot)
}

#' Evaluate information criteria for regression model
#'
#' Plots the information criterion - either Cp (default) or BIC - against
#' the adjusted R square of the feature selection in the modeling process.
#' Both BIC and Mallow's Cp are measures to avoid over-fitting. Please
#' choose the model that has a high information criterion, while modeling
#' the original data as close as possible. R2 adjusted values of ~ .99 might
#' work well, depending on your scenario. In other words: Look out for the
#' elbow in the curve and choose th model where the information criterion
#' begins to drop. Nonetheless, inspect the according model with \code{plotPercentiles(data, group)}
#' to visually inspect the course of the percentiles.
#' In the plot, Mallow's Cp is log, transformed and the BIC is always highly
#' negative. The R2 cutoff that was specified in the bestModel function is
#' displayed as a dashed line.
#' @param model The regression model from the bestModel function
#' @param type Type of chart with 0 = adjusted R2 by number of predictors,
#' 1 = log transformed Mallow's Cp by adjusted R2 and 2 = Bayesian Information
#' Criterion (BIC) by adjusted R2
#' @seealso bestModel, plotPercentiles, printSubset
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotSubset(m)
#' @export
plotSubset <- function(model, type = 1) {
  message("Hint: Select the model with the highest BIC or Cp score while simultaneously optimizing R2.")
  dataFrameTMP <- data.frame(adjr2=model$subsets$adjr2, bic=model$subsets$bic, cp = model$subsets$cp, nr = seq(1, length(model$subsets$adjr2), by=1))
  if (type == 1) {
    lattice::xyplot(cp ~ adjr2,
      data = dataFrameTMP, type = "b",
      col.line = "lightblue", lwd = 1,
      grid = TRUE, scales = list(y = list(log = 10)),
      main = "Information Function",
      ylab = "log-transformed Mallows's Cp",
      xlab = "Adjusted R2",
      key = list(
        corner = c(
          0.1,
          0.1
        ), lines = list(
          col = c("lightblue", "#9933FF"),
          lty = c(1, 2), lwd = 2
        ),
        text = list(c(
          "Model in Ascending Order",
          "Cutoff Value"
        ))
      ), panel = function(x, y, ...) {
        lattice::panel.abline(
          v = model$cutoff,
          lwd = 2, lty = "longdash",
          col = "#9933FF", label = model$cutoff
        )
        lattice::panel.xyplot(x, y, ...)
      }
    )
  } else if(type==2){
    lattice::xyplot(bic ~ adjr2,
      data = dataFrameTMP, type = "b",
      col.line = "lightblue", lwd = 1,
      grid = TRUE,
      main = "Information Function",
      ylab = "BIC",
      xlab = "Adjusted R2",
      key = list(
        corner = c(
          0.1,
          0.1
        ), lines = list(
          col = c("lightblue", "#9933FF"),
          lty = c(1, 2), lwd = 2
        ),
        text = list(c(
          "Model in Ascending Order",
          "cutoff Value"
        ))
      ), panel = function(x, y, ...) {
        lattice::panel.abline(
          v = model$cutoff,
          lwd = 2, lty = "longdash",
          col = "#9933FF", label = model$cutoff
        )
        lattice::panel.xyplot(x, y, ...)
      }
    )
  }else{
    lattice::xyplot(adjr2 ~ nr,
                    data = dataFrameTMP, type = "b",
                    col.line = "lightblue", lwd = 1,
                    grid = TRUE,
                    main = "Information Function",
                    ylab = "Adjusted R2",
                    xlab = "Number of predictors",
                    key = list(
                      corner = c(
                        0.9,
                        0.1
                      ), lines = list(
                        col = c("#9933FF"),
                        lty = c(2), lwd = 2
                      ),
                      text = list(c("Cutoff Value"
                      ))
                    ), panel = function(x, y, ...) {
                      lattice::panel.abline(
                        h = model$cutoff,
                        lwd = 2, lty = "longdash",
                        col = "#9933FF", label = model$cutoff
                      )
                      lattice::panel.xyplot(x, y, ...)
                    }
    )

  }
}

#' Plot first order derivative of regression model
#'
#' Plots the values obtained via the first order derivative of the regression model
#' in dependence of the norm score. The results indicate the progression of the
#' norm scores within each age group. The regression based modeling approach
#' relies on the assumption of a linear progression of the norm scores.
#' Negative scores in the first order derivative indicate a violation of this
#' assumption. Scores near zero
#' are typical for bottom and ceiling effects in the raw data.
#' The regression models usually converge within the range of the original
#' values. In case of vertical and horizontal extrapolation, with increasing
#' distance to the original data, the risk of assumption violation increases
#' as well.
#' ATTENTION: plotDerivative is currently still incompatible with reversed raw
#' score scales ('descent' option)
#' @param model The model from the bestModel function
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param stepAge Stepping parameter for the age check, usually 1 or 0.1; lower
#' values indicate higher precision / closer checks
#' @param minNorm Lower end of the norm score range, in case of T scores, 25 might be good
#' @param maxNorm Upper end of the norm score range, in case of T scores, 25 might be good
#' @param stepNorm Stepping parameter for norm scores
#' @param descend Reverse raw score order. If set to TRUE, lower raw scores
#' indicate higher performance. Relevant f. e. in case of modelling errors
#' @seealso checkConsistency, bestModel, derive
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotDerivative(m, minAge=2, maxAge=5, step=.2, minNorm=25, maxNorm=75, stepNorm=1)
#' @export
plotDerivative <- function(model,
                           minAge = 2,
                           maxAge = 5,
                           minNorm = 25,
                           maxNorm = 75,
                           stepAge = 0.2,
                           stepNorm = 1,
                           descend = FALSE) {
  print(cNORM::rangeCheck(model, minAge, maxAge, minNorm, maxNorm))
  rowS <- c(seq(minNorm, maxNorm, length.out = 1 + (maxNorm - minNorm) / stepNorm))
  colS <- c(seq(minAge, maxAge, length.out = 1 + (maxAge - minAge) / stepAge))
  coeff <- cNORM::derive(model)
  devFrame <- data.frame(matrix(NA, nrow = length(rowS), ncol = length(colS)))
  dev2 <- data.frame()

  colnames(devFrame) <- colS
  rownames(devFrame) <- rowS

  i <- 1
  while (i <= ncol(devFrame)) {
    j <- 1
    while (j <= nrow(devFrame)) {
      devFrame[j, i] <- cNORM::predictRaw(rowS[[j]], colS[[i]], coeff, descend)
      colList <- c(rowS[[j]], colS[[i]], devFrame[j, i])
      dev2 <- rbind(dev2, colList)
      j <- j + 1
    }
    i <- i + 1
  }
  colnames(dev2) <- c("X", "Y", "Z")

  # define range and colors
  min <- min(dev2$Z) - .1
  max <- max(dev2$Z) + .1
  step <- (max - min) / 1000
  regions <- grDevices::rainbow(1000, end = .8)
  key <- list(at = seq(min, max, by = step))
  sequence <- seq(min, max, by = step)

  if (requireNamespace("latticeExtra", quietly = TRUE)) {
    p1 <- lattice::levelplot(Z ~ Y * X,
      data = dev2,
      at = sequence, region = T,
      colorkey = key,
      col.regions = regions,
      panel = latticeExtra::panel.2dsmoother,
      main = "Slope of the Regression Function\n(1st Order Derivative)",
      ylab = "First Order Derivate of Norm Score",
      xlab = "Age"
    )
  } else {
    p1 <- lattice::levelplot(Z ~ Y * X,
      data = dev2,
      at = sequence, region = T,
      colorkey = key,
      col.regions = regions,
      main = "Slope of the Regression Function\n(1st Order Derivative)",
      ylab = "1st Order Derivate of Norm Score",
      xlab = "Age"
    )
  }
  p2 <- lattice::contourplot(Z ~ Y * X, data = dev2)

  p3 <- p1 + p2
  p3
}
