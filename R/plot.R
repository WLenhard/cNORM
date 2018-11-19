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
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotRaw(normData, m, group="group")
#' @export
plotRaw <- function(data, model, group = NULL, raw = NULL, type = 0) {
  if (is.null(raw)) {
    raw <- attr(data, "raw")
  }


  if (group != "" && !is.null(group) && !(group %in% colnames(data))) {
    warning(paste(c("Grouping variable '", group, "' does not exist in data object. Please check variable names and fix 'group' parameter in function call."), collapse = ""))
    group <- NULL
  }

  if (!(raw %in% colnames(data))) {
    stop(paste(c("ERROR: Raw variable '", raw, "' does not exist in data object."), collapse = ""))
  }


  d <- data
  d$raw <- data[[raw]]
  d$fitted <- model$fitted.values
  d$diff <- d$fitted - d$raw
  mse <- round(model$rmse, digits=4)
  r <- round(cor(d$fitted, d$raw,
                 use = "pairwise.complete.obs"), digits = 4)
  d <- as.data.frame(d)
  if (group != "" && !is.null(group)) {
    d$group <- data[[group]]
    d$group <- as.factor(d$group)

    if (type == 0) {
      lattice::xyplot(fitted ~ raw | group, d,
        main = paste("Observed vs. Fitted Raw Scores by ", group, "\nr = ", r, ", RMSE = ", mse),
        ylab = "Fitted Scores",
        xlab = "Observed Score",
        grid = TRUE,
        auto.key = TRUE,
        abline = c(0, 1), lwd = 1
      )
    } else {
      lattice::xyplot(diff ~ raw | group, d,
        main = paste("Observed Raw Scores vs. Difference Scores by ", group, "\nr = ", r, ", RMSE = ", mse),
        ylab = "Difference Scores",
        xlab = "Observed Score",
        grid = TRUE,
        auto.key = TRUE,
        panel = function(...) {
          panel.xyplot(...)
          panel.abline(h = .0, col = 2, lty = 2)
        }
      )
    }
  } else {
    if (type == 0) {
      lattice::xyplot(fitted ~ raw, d,
        main = paste("Observed vs. Fitted Raw Scores\nr = ", r, ", RMSE = ", mse),
        ylab = "Fitted Scores",
        xlab = "Observed Score",
        grid = TRUE,
        auto.key = TRUE,
        abline = c(0, 1), lwd = 1
      )
    } else {
      lattice::xyplot(diff ~ raw, d,
        main = paste("Observed Raw Scores vs. Difference Scores\nr = ", r, ", RMSE = ", mse),
        ylab = "Difference",
        xlab = "Observed Score",
        grid = TRUE,
        auto.key = TRUE,
        panel = function(...) {
          panel.xyplot(...)
          panel.abline(h = .0, col = 2, lty = 2)
        }
      )
    }
  }
}

#' Plot manifest and fitted norm scores
#'
#' The function plots the manifest norm score against the fitted norm score from
#' the inverse regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' regression line. The computation of the standard error is based on Ooserhuis, van der
#' Ark and Sijtsma (2016).
#' @param data The raw data within a data.frame
#' @param model The regression model
#' @param group The grouping variable, use empty string "" for no group
#' @param minNorm lower bound of fitted norm scores
#' @param maxNorm upper bound of fitted norm scores
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#' @references Oosterhuis, H. E. M., van der Ark, L. A., & Sijtsma, K. (2016). Sample Size Requirements for Traditional and Regression-Based Norms. Assessment, 23(2), 191–202. https://doi.org/10.1177/1073191115580638
#' @examples
#' # Load example data set, compute model and plot results
#' \dontrun{
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotNorm(normData, m, group="group", minNorm=25, maxNorm=75)
#' }
#' @export
plotNorm <- function(data, model, group = "", minNorm = NULL, maxNorm = NULL, type = 0) {
  if (is.null(minNorm)) {
    # warning("minNorm not specified, taking absolute minimum norm score from modeling...")
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)) {
    # warning("maxNorm not specified, taking absolute maximum norm score from modeling...")
    maxNorm <- model$maxL1
  }

  if (group != "" && !is.null(group) && !(group %in% colnames(data))) {
    warning(paste(c("Grouping variable '", group, "' does not exist in data object. Please check variable names and fix 'group' parameter in function call."), collapse = ""))
    group <- NULL
  }

  d <- data
  raw <- data[[model$raw]]
  age <- data[[model$age]]
  d$fitted <- predictNorm(raw, age, model, minNorm = minNorm, maxNorm = maxNorm)
  d$diff <- d$fitted - data$normValue
  d <- d[!is.na(d$fitted), ]
  d <- d[!is.na(d$diff), ]

  se <- round(sum(sqrt(d$diff^2))/(nrow(d)-2), digits = 4)
  r <- round(cor(d$fitted, d$normValue, use = "pairwise.complete.obs"), digits = 4)

  if (group != "" && !is.null(group)) {
    d$group <- data[[group]]
    d$group <- as.factor(d$group)
    if (type == 0) {
      lattice::xyplot(fitted ~ normValue | group, d,
        main = paste("Observed vs. Fitted Norm Scores by ", group, "\nr = ",
                     r, ", SE = ", se),
        ylab = "Fitted Scores",
        xlab = "Observed Scores",
        grid = TRUE,
        auto.key = TRUE,
        abline = c(0, 1), lwd = 1
      )
    } else {
      lattice::xyplot(diff ~ normValue | group, d,
        main = paste("Observed Norm Scores vs. Difference Scores by ", group, "\nr = ",
                     r, ", SE = ", se),
        ylab = "Difference",
        xlab = "Observed Scores",
        grid = TRUE,
        auto.key = TRUE,
        abline = c(0, 1), lwd = 1,
        panel = function(...) {
          panel.xyplot(...)
          panel.abline(h = .0, col = 2, lty = 2)
        }
      )
    }
  } else {
    if (type == 0) {
      lattice::xyplot(fitted ~ normValue, d,
        main = paste("Observed vs. Fitted Norm Scores\nr = ",
                     r, ", SE = ", se),
        ylab = "Fitted Scores",
        xlab = "Observed Scores",
        grid = TRUE,
        auto.key = TRUE,
        abline = c(0, 1), lwd = 1
      )
    } else {
      lattice::xyplot(diff ~ normValue, d,
        main = paste("Observed Norm Scores vs. Difference Scores\nr = ",
                     r, ", SE = ", se),
        ylab = "Difference",
        xlab = "Observed Scores",
        grid = TRUE,
        auto.key = TRUE,
        abline = c(0, 1), lwd = 1,
        panel = function(...) {
          panel.xyplot(...)
          panel.abline(h = .0, col = 2, lty = 2)
        }
      )
    }
  }
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
                           minAge = NULL,
                           maxAge = NULL,
                           step = 0.1,
                           minRaw = NULL,
                           maxRaw = NULL) {
  if (is.null(minAge)) {
    minAge <- model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- model$maxA1
  }

  if (is.null(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  valueList <- data.frame(n = factor(), raw = double(), age = double())
  n <- length(normList)


  for (i in 1:n) {
    normCurve <-
      getNormCurve(
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
  COL <- rainbow(length(normList))
  panelfun <- function(..., type, group.number) {
    lattice::panel.lines(...)
  }

  lattice::xyplot(raw ~ age,
    data = valueList, groups = n,
    panel = function(...)
      lattice::panel.superpose(..., panel.groups = panelfun),
    main = "Norm Curves",
    ylab = "Raw Score", xlab = "Explanatory Variable",
    col = COL, lwd = 1.5, grid = TRUE,
    key = list(
      corner = c(0.99, 0.1),
      lines = list(col = COL, lwd = 1.5),
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
#' the quantile function with the default settings. Please consult help(quantile)
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
#' @param scale The norm scale, either 'T', 'IQ', 'z', 'percentile' or
#' self defined with a double vector with the mean and standard deviation,
#' f. e. c(10, 3) for Wechsler scale index points; if NULL, scale information from the
#' data preparation is used (default)
#' @param type The type parameter of the quantile function to estimate the percentiles
#' of the raw data (default 7)
#' @param title custom title for plot
#' @seealso plotNormCurves, plotPercentileSeries
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotPercentiles(normData, m)
#' @export
plotPercentiles <- function(data,
                            model,
                            minRaw = NULL,
                            maxRaw = NULL,
                            minAge = NULL,
                            maxAge = NULL,
                            raw = NULL,
                            group = NULL,
                            percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                            scale = NULL,
                            type = 7,
                            title = NULL) {
  if (is.null(group)) {
    group <- attr(data, "group")
  }

  if (is.null(minAge)) {
    minAge <- model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- model$maxA1
  }

  if (is.null(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  if (is.null(raw)) {
    raw <- model$raw
  }

  if (!(raw %in% colnames(data))) {
    stop(paste(c("ERROR: Raw score variable '", raw, "' does not exist in data object."), collapse = ""))
  }

  if (!(group %in% colnames(data))) {
    stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
  }

  # compute norm scores from percentile vector
  if (is.null(scale)) {
    # fetch scale information from model
    T <- qnorm(percentiles, model$scaleM, model$scaleSD)
  } else if ((typeof(scale) == "double" && length(scale) == 2)) {
    T <- qnorm(percentiles, scale[1], scale[2])
  } else if (scale == "IQ") {
    T <- qnorm(percentiles, 100, 15)
  } else if (scale == "z") {
    T <- qnorm(percentiles)
  } else if (scale == "T") {
    T <- qnorm(percentiles, 50, 10)
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
      aggregate(data[, raw],
        list(data[, group]),
        FUN = function(x) quantile(x,
            probs = percentiles,
            type = type,
            na.rm = TRUE
          )
      )
    )
  }
  # compute percentile table
  colnames(percentile.actual) <- c(c(group), NAMES)

  # build finer grained grouping variable for prediction
  AGEP <- unique(data[, group])
  lines <- length(AGEP)

  for (m in 1:lines - 1) {
    share <- (AGEP[m + 1] - AGEP[m]) / 5
    additional <- c(share + AGEP[m], 2 * share + AGEP[m], 3 * share + AGEP[m], 4 * share + AGEP[m])
    AGEP <- c(AGEP, additional)
  }


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
      percentile.fitted[i, j + 1] <- predictRaw(
        T[[j]],
        AGEP[[i]],
        model$coefficients,
        minRaw, maxRaw
      )

      j <- j + 1
    }
    i <- i + 1
  }

  # Merge actual and predicted scores and plot them show lines
  # for predicted scores and dots for actual scores
  percentile <- merge(percentile.actual, percentile.fitted,
    by = group, all.y = TRUE
  )

  END <- 5 / 6
  COL1 <- rainbow(length(percentiles), end = END)
  COL2 <- c(rainbow(length(percentiles), end = END), rainbow(length(percentiles), end = END))

  panelfun <- function(..., type, group.number) {
    if (group.number > length(T)) {
      lattice::panel.lines(...)
    } else {
      lattice::panel.points(..., type = "p")
    }
  }

  if (is.null(title)) {
    title <- "Manifest and Fitted Percentile Curves"
  }
  plot <- lattice::xyplot(formula(xyFunction), percentile,
    panel = function(...)
      lattice::panel.superpose(..., panel.groups = panelfun),
    main = title,
    ylab = paste0("Raw Score (", raw, ")"), xlab = paste0("Explanatory Variable (", group, ")"),
    col = COL2, lwd = 1.5, grid = TRUE,
    key = list(
      corner = c(0.99, 0.01),
      lines = list(col = COL1, lwd = 1.5),
      text = list(NAMES)
    )
  )

  print(plot)
  return(plot)
}


#' Plot the density function per group by raw score
#'
#' The function plots the density  curves based on the regression model against
#' the actual percentiles from the raw data. As in 'plotNormCurves',
#' please check for inconsistent curves, especially curves showing implausible shapes.
#' @param model The model from the bestModel function
#' @param minRaw Lower bound of the raw score
#' @param maxRaw Upper bound of the raw score
#' @param minNorm Lower bound of the norm score
#' @param maxNorm Upper bound of the norm score
#' @param group Column of groups to plot
#' @seealso plotNormCurves, plotPercentiles
#' @examples
#' # Load example data set, compute model and plot results for age values 2, 4 and 6
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotDensity(m, group = c (2, 4, 6))
#' @export
plotDensity <- function(model,
                        minRaw = NULL,
                        maxRaw = NULL,
                        minNorm = NULL,
                        maxNorm = NULL,
                        group = NULL) {
  if (is.null(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- model$maxL1
  }

  if (is.null(minRaw)) {
    minRaw <- model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- model$maxRaw
  }

  if (is.null(group)) {
    group <- c(model$minA1, (model$maxA1 + model$minA1) / 2, model$maxA1)
  }

  step <- (maxNorm - minNorm) / 100

  i <- 1
  while (i <= length(group)) {
    norm <- normTable(group[[i]], model, minNorm, maxNorm, minRaw, maxRaw, step = step)
    norm$group <- rep(group[[i]], length.out = nrow(norm))

    if (i == 1) {
      matrix <- norm
    } else {
      matrix <- rbind(matrix, norm)
    }

    i <- i + 1
  }
  matrix <- matrix[matrix$norm > minNorm & matrix$norm < maxNorm, ]
  matrix <- matrix[matrix$raw > minRaw & matrix$raw < maxRaw, ]
  matrix$density <- dnorm(matrix$norm, mean = model$scaleM, sd = model$scaleSD)

  # lattice display options
  COL <- rainbow(length(group))
  NAMES <- paste("Group ", group, sep = "")
  panelfun <- function(..., type, group.number) {
    lattice::panel.lines(...)
  }

  plot <- lattice::xyplot(density ~ raw,
    data = matrix, groups = group,
    panel = function(...)
      lattice::panel.superpose(..., panel.groups = panelfun),
    main = "Density function",
    ylab = "Density", xlab = "Raw Score",
    col = COL, lwd = 1.5, grid = TRUE,
    key = list(
      corner = c(0, 1),
      lines = list(col = COL, lwd = 1.5),
      text = list(NAMES)
    )
  )
  print(plot)
  return(matrix)
}


#' Generates a series of plots with number curves by percentile for different models
#'
#' This functions makes use of 'plotPercentiles' to generate a series of plots
#' with different number of predictors. It draws on the information provided by the model object
#' to determine the bounds of the modeling (age and standard score range). It can be used as an
#' additional model check to determine the best fitting model. Please have a look at the
#'' plotPercentiles' function for further information.
#' @param data The raw data including the percentiles and norm scores
#' @param model The model from the bestModel function
#' @param start Number of predictors to start with
#' @param end Number of predictors to end with
#' @param group The name of the grouping variable; the distinct groups are automatically
#' determined
#' @param percentiles Vector with percentile scores, ranging from 0 to 1 (exclusive)
#' @param type The type parameter of the quantile function to estimate the percentiles
#' of the raw data (default 7)
#' @param filename Prefix of the filename. If specified, the plots are saves as
#' png files in the directory of the workspace, instead of displaying them
#' @seealso plotPercentiles
#' @return the complete list of plots
#' @export
#'
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData(elfe)
#' model <- bestModel(data = normData)
#' plotPercentileSeries(normData, model, start=1, end=5, group="group")
plotPercentileSeries <- function(data, model, start = 1, end = NULL, group = NULL,
                                 percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                                 type = 7,
                                 filename = NULL) {
  d <- as.data.frame(data)
  if ((is.null(end)) || (end > length(model$subsets$rss))) {
    end <- length(model$subsets$rss)
  }

  if (start < 1) {
    start <- 1
  }

  if (start > end) {
    start <- end
  }

  minR <- min(d[, model$raw])
  maxR <- max(d[, model$raw])
  l <- list()

  while (start <= end) {
    message(paste0("Plotting model ", start))
    # compute model
    text <- paste0(model$raw, " ~ ")
    names <- colnames(model$subsets$outmat)

    j <- 1
    nr <- 0
    while (j <= length(names)) {
      if (model$subsets$outmat[start, j] == "*") {
        text1 <- names[j]
        if (nr == 0) {
          text <- paste(text, text1, sep = "")
        } else {
          text <- paste(text, text1, sep = " + ")
        }

        nr <- nr + 1
      }
      j <- j + 1
    }

    bestformula <- lm(text, d)
    bestformula$ideal.model <- model$ideal.model
    bestformula$cutoff <- model$cutoff
    bestformula$subsets <- model$subsets

    bestformula$maxA1 <- model$maxA1
    bestformula$minA1 <- model$minA1
    bestformula$minL1 <- model$minL1
    bestformula$maxL1 <- model$maxL1
    bestformula$minRaw <- minR
    bestformula$maxRaw <- maxR
    bestformula$raw <- model$raw
    bestformula$scaleSD <- attributes(d)$scaleSD
    bestformula$scaleM <- attributes(d)$scaleM
    bestformula$descend <- attributes(d)$descend
    bestformula$group <- attributes(d)$group
    bestformula$age <- attributes(d)$age
    bestformula$k <- attributes(d)$k


    l[[length(l) + 1]] <- cNORM::plotPercentiles(d, bestformula,
      minAge = model$minA1, maxAge = model$maxA1,
      minRaw = minR,
      maxRaw = maxR,
      percentiles = percentiles,
      scale = NULL,
      group = group,
      title = paste0("Manifest and Fitted Percentile Curves\nModel with ", start, " predictors, R2=", round(bestformula$subsets$adjr2[[start]], digits = 4))
    )

    if (!is.null(filename)) {
      lattice::trellis.device(device = "png", filename = paste0(filename, start, ".png"))
      print(l[[length(l)]])
      dev.off()
    }
    start <- start + 1
  }
  return(l)
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
#' In the plot, Mallow's Cp is log transformed and the BIC is always highly
#' negative. The R2 cutoff that was specified in the bestModel function is
#' displayed as a dashed line.
#' @param model The regression model from the bestModel function
#' @param type Type of chart with 0 = adjusted R2 by number of predictors,
#' 1 = log transformed Mallow's Cp by adjusted R2, 2 = Bayesian Information
#' Criterion (BIC) by adjusted R2 and 3 = Root Mean Square Error (RMSE) by number
#' of predictors
#' @seealso bestModel, plotPercentiles, printSubset
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotSubset(m)
#' @export
plotSubset <- function(model, type = 1) {
  message("Hint: Select the model with the highest BIC or Cp score while simultaneously optimizing R2.")
  dataFrameTMP <- data.frame(adjr2 = model$subsets$adjr2, bic = model$subsets$bic, cp = model$subsets$cp, RMSE = sqrt(model$subsets$rss / length(model$fitted.values)), nr = seq(1, length(model$subsets$adjr2), by = 1))
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
  } else if (type == 2) {
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
  } else if(type == 3){
    lattice::xyplot(RMSE ~ nr,
                    data = dataFrameTMP, type = "b",
                    col.line = "lightblue", lwd = 1,
                    grid = TRUE,
                    main = "Information Function",
                    ylab = "Root Means Square Error",
                    xlab = "Number of Predictors" )
  } else {
    lattice::xyplot(adjr2 ~ nr,
                    data = dataFrameTMP, type = "b",
                    col.line = "lightblue", lwd = 1,
                    grid = TRUE,
                    main = "Information Function",
                    ylab = "Adjusted R2",
                    xlab = "Number of Predictors",
                    key = list(
                      corner = c(
                        0.9,
                        0.1
                      ), lines = list(
                        col = c("#9933FF"),
                        lty = c(2), lwd = 2
                      ),
                      text = list(c("Cutoff Value"))
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
#' Plots the scores obtained via the first order derivative of the regression model
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
#' @param order Degree of the derivative (default = 1)
#' @param descend Reverse raw score order. If set to TRUE, lower raw scores
#' indicate higher performance. Relevant f. e. in case of modeling errors
#' @seealso checkConsistency, bestModel, derive
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotDerivative(m, minAge=2, maxAge=5, step=.2, minNorm=25, maxNorm=75, stepNorm=1)
#' @export
plotDerivative <- function(model,
                           minAge = NULL,
                           maxAge = NULL,
                           minNorm = NULL,
                           maxNorm = NULL,
                           stepAge = 0.2,
                           stepNorm = 1,
                           order = 1,
                           descend = FALSE) {
  if (is.null(minAge)) {
    minAge <- model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- model$maxA1
  }

  if (is.null(minNorm)) {
    minNorm <- model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- model$maxL1
  }

  rowS <- c(seq(minNorm, maxNorm, length.out = 1 + (maxNorm - minNorm) / stepNorm))
  colS <- c(seq(minAge, maxAge, length.out = 1 + (maxAge - minAge) / stepAge))
  coeff <- cNORM::derive(model, order)
  cat(paste0(cNORM::rangeCheck(model, minAge, maxAge, minNorm, maxNorm), " Coefficients from the ", order, " order derivative function:\n\n"))
  print(coeff)

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
  min <- min(dev2$Z)
  max <- max(dev2$Z)
  diff <- (max - min) / 10
  min <- min - diff
  max <- max + diff
  step <- (max - min) / 1000
  regions <- rainbow(1000, end = .8)
  key <- list(at = seq(min, max, by = step))
  sequence <- seq(min, max, by = step)

  desc <- "(1st Order Derivative)"
  if (order == 2) {
    desc <- "(2nd Order Derivative)"
  } else if (order == 3) {
    desc <- "(3rd Order Derivative)"
  } else if (order > 2) {
    desc <- paste0(order, "th Order Derivative)")
  }


  if (requireNamespace("latticeExtra", quietly = TRUE)) {
    p1 <- lattice::levelplot(Z ~ Y * X,
      data = dev2,
      at = sequence,
      colorkey = key,
      region = T,
      col.regions = regions,
      panel = latticeExtra::panel.2dsmoother,
      main = paste0("Slope of the Regression Function\n", desc),
      ylab = "First Order Derivate of Norm Score",
      xlab = "Explanatory Variable"
    )
  } else {
    p1 <- lattice::levelplot(Z ~ Y * X,
      data = dev2,
      at = sequence, region = T,
      colorkey = key,
      col.regions = regions,
      main = paste0("Slope of the Regression Function\n", desc),
      ylab = "1st Order Derivate of Norm Score",
      xlab = "Explanatory Variable"
    )
  }
  p2 <- lattice::contourplot(Z ~ Y * X, data = dev2)

  p3 <- p1 + p2
  p3
}
