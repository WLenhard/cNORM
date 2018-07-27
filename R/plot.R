#' Plot actual and predicted raw values
#'
#' The function plots the raw data against the predicted values from
#' the regression model per group. This helps to inspect the precision
#' of the modeling process. The values should not deviate too far from
#' regression line.
#' @param data The raw data within a data.frame
#' @param model The regression model
#' @param group The grouping variable
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotValues(normData, m, group="group")
#' @export
plotValues <- function(data, model, group = "group") {

  d <- data
    d$fitted <- model$fitted.values
    d$group <- data[[group]]
    d$group <- base::as.factor(d$group)
    lattice::xyplot(fitted ~ raw | group, d,
           main = paste("Actual vs. predicted raw values by ", group),
           ylab = "Predicted values",
           xlab = "Actual values",
           grid = TRUE,
           auto.key = TRUE,
           abline = c(0, 1), lwd = 1)
}

#' Plot norm curves
#'
#' The function plots the norm curves based on the regression model.
#' Please check the function for inconsistent curves: The different
#' curves should not intersect. Violations of this assumption are a strong
#' indication for problems
#' in modeling the relationship between raw and norm values. There are
#' several reasons, why this might occur:
#' \enumerate{
#'   \item Vertical extrapolation: Choosing extreme norm values, e. g. values
#'   -3 <= x and x >= 3 In order to model these extreme values, a large sample
#'   dataset is necessary.
#'   \item Horizontal extrapolation: Taylor polynomials converge in a certain
#'   radius. Using the model values outside the original dataset may
#'   lead to inconsistent results.
#'   \item The data cannot be modeled with Taylor polynomials, or you need
#'   another power parameter (k) or R2 for the model.
#'  }
#' In general, extrapolation (point 1 and 2) can carefully be done to a
#' certain degree outside the original sample, but it should in general
#' be handled with caution.
#' checkConsistency and derivationPlot can be used to further inspect the model.
#' @param model The model from the bestModel function
#' @param normList Vector with norm values to display
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param step Stepping parameter for the age check, usually 1 or 0.1; lower
#' values indicate higher precision / closer checks
#' @param minRaw Lower end of the raw value range, used for clipping implausible results
#' (default = 0)
#' @param maxRaw Upper end of the raw value range, used for clipping implausible results
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

    valueList <- list()
    n <- length(normList)
    i <- 1

    norm <- list()
    raw <- list()
    age <- list()

    while (i <= n) {
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
      norm <- base::c(norm, normCurve$norm)
      raw <- base::c(raw, normCurve$raw)
      age <- base::c(age, normCurve$age)

      i <- i + 1
    }

    valueList$n <- norm
    valueList$raw <- raw
    valueList$age <- age
    dataFrame <- base::as.data.frame(valueList, row.names = NULL,
                               optional = FALSE,
                               cut.names = FALSE,
                               col.names = names(base::c("n", "raw", "age")),
        fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors())

    age <- norm
    lattice::xyplot(raw ~ age, data = valueList, grid = TRUE)
}




#' Plot norm curves against actual percentiles
#'
#' The function plots the norm curves based on the regression model against
#' the actual percentiles from the raw data. As in 'plotNormCurves',
#' please check for inconsistent curves, especially intersections.
#' Violations of this assumption are a strong
#' indication for problems
#' in modeling the relationship between raw and norm values.
#' In general, extrapolation (point 1 and 2) can carefully be done to a
#' certain degree outside the original sample, but it should in general
#' be handled with caution.
#' The original percentiles are displayed as distinct points in the according
#' color, the model based projection of percentiles are drawn as lines.
#' Please note, that the estimation of the percentiles of the raw data is done with
#' the stats::quantile function with the default settings. Please consult help(quantile)
#' and change the 'type' parameter accordingly.
#' @param data The raw data including the percentiles and norm values
#' @param model The model from the bestModel function
#' @param minRaw Lower bound of the raw value scale (default = 0)
#' @param maxRaw Upper bound of the raw value scale
#' @param raw The name of the raw variable
#' @param group The name of the grouping variable; the distinct groups are automatically
#' determined
#' @param percentiles Vector with percentile values, ranging from 0 to 1 (exclusive)
#' @param scale The norm value scale, either 'T' (default), 'IQ' or 'z'
#' @param type The type parameter of the quantile function to estimate the percentiles
#' of the raw data (default 7)
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
                            raw = "raw",
                            group = "group",
                            percentiles = base::c(0.02, 0.05, 0.2, 0.5, 0.8, 0.95, 0.98),
                            scale = "T",
                            type = 7) {

  # compute norm values from percentile vector
    if (scale == "IQ") {
        T <- stats::qnorm(percentiles, 100, 15)
    } else if (scale == "z") {
        T <- stats::qnorm(percentiles)
    } else {
        T <- stats::qnorm(percentiles, 50, 10)
    }

    # generate variable names
    NAMES <- base::paste("PR", percentiles * 100, sep = "")
    NAMESP <- base::paste("PredPR", percentiles * 100, sep = "")

    # build function for xyplot and aggregate actual percentiles per group
    if(typeof(group)=="logical"&&!group){
      message("The plotPercentiles-function does not work without a grouping variable.")
    }else{
      xyFunction <- base::paste(base::paste(NAMES, collapse = " + "),
                              base::paste(NAMESP, collapse = " + "),
                        sep = " + ", collapse = " + ")
    xyFunction <- base::paste(xyFunction, group, sep = " ~ ")
    percentile.actual <- do.call(data.frame,
                                 stats::aggregate(data[, raw],
                                                  list(data[, group]),
                                                  FUN = function(x) stats::quantile(x,
                                                  probs = percentiles,
                                                  type = type)))

    }
    # compute percetile table
    colnames(percentile.actual) <- base::c(base::c("group"), NAMES)

    # build finer grained grouping variable for prediction
    gr <- percentile.actual$group
    leng <- base::length(gr)
    FIRST <- gr[[1]]
    LAST <- gr[[leng]]
    AGEP <- base::seq(FIRST, LAST, length.out = leng * 2 - 1)

    # fitt predicted percentiles
    percentile.fitted <- base::data.frame(base::matrix(NA,
                                           nrow = base::length(AGEP),
                                           ncol = base::length(T) + 1))
    percentile.fitted[, 1] <- AGEP
    colnames(percentile.fitted) <- base::c(base::c("group"), NAMESP)

    i <- 1
    while (i <= base::length(AGEP)) {
        j <- 1

        while (j <= base::length(T)) {
            percentile.fitted[i, j + 1] <- cNORM::predictRaw(T[[j]],
                                                      AGEP[[i]],
                                                      model$coefficients,
                                                      minRaw, maxRaw)

            j <- j + 1
        }
        i <- i + 1
    }

    # merge actual and predicted values und plot them show lines

    # for predicted values and dots for actual values
    percentile <- base::merge(percentile.actual, percentile.fitted,
                        by = "group", all.y = TRUE)

    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      COL1 <- RColorBrewer::brewer.pal(length(percentiles), "Spectral")
      COL2 <-
        c(RColorBrewer::brewer.pal(length(percentiles), "Spectral"),
          RColorBrewer::brewer.pal(length(percentiles), "Spectral"))
    } else {
      COL1 <- base::seq(1, length(percentiles))
      COL2 <-
        base::c(base::seq(1, base::length(percentiles)),
                base::seq(1, base::length(percentiles)))
    }

    panelfun <- function(..., type, group.number) {
        if (group.number > length(T)) {
          lattice::panel.lines(...)
        } else {
          lattice::panel.points(..., type = "p")
        }
    }

    lattice::xyplot(stats::formula(xyFunction), percentile, panel = function(...)
      lattice::panel.superpose(..., panel.groups = panelfun),
      main = "Actual and predicted percentile curves",
      ylab = "Raw value", xlab = "Explanatory Variable",
      col = COL2, lwd = 2, grid = TRUE,
      key = list(corner = c(0.99, 0.01),
                 lines = list(col = COL1, lwd = 2),
                 text = list(NAMES)))
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
#' negative. The R2 cutoff, that was specified in the bestModel function is
#' displayed as a dashed line.
#' @param model The regression model from the bestModel function
#' @param bic Display the log transformed Mallow's Cp (bic = FALSE; default)
#' or the Bayesian Information Criterion (BIC)
#' @examples
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotSubset(m)
#' @export
plotSubset <- function(model, bic = FALSE) {
  base::message("Hint: Select the model with the highest BIC or Cp score while simultaneously optimizing R2.")
  if(!bic){
    dataFrameTMP <- base::data.frame(model$subsets$adjr2, model$subsets$cp)
    lattice::xyplot(log(model.subsets.cp) ~ model.subsets.adjr2, data = dataFrameTMP, type = "b",
           col.line = "lightblue", lwd = 1,
           grid = TRUE, scales = list(y = list(log = 10)),
           main = "Information Function",
           ylab = "log-transformed Mallows's Cp",
           xlab = "Adjusted R2",
           key = list(corner = c(0.1,
            0.1), lines = list(col = c("lightblue", "#9933FF"),
                               lty = c(1, 2), lwd = 2),
            text = list(c("model in ascending order",
            "cutoff value"))), panel = function(x, y, ...) {
              lattice::panel.abline(v = model$cutoff,
                         lwd = 2, lty = "longdash",
                         col = "#9933FF", label = model$cutoff)
              lattice:: panel.xyplot(x, y, ...)
        })
  }else{
    dataFrameTMP <- base::data.frame(model$subsets$adjr2, model$subsets$bic)
    lattice::xyplot(model.subsets.bic ~ model.subsets.adjr2, data = dataFrameTMP, type = "b",
                    col.line = "lightblue", lwd = 1,
                    grid = TRUE,
                    main = "Information Function",
                    ylab = "BIC",
                    xlab = "Adjusted R2",
                    key = list(corner = c(0.1,
                                          0.1), lines = list(col = c("lightblue", "#9933FF"),
                                                             lty = c(1, 2), lwd = 2),
                               text = list(c("model in ascending order",
                                             "cutoff value"))), panel = function(x, y, ...) {
                                               lattice::panel.abline(v = model$cutoff,
                                                                     lwd = 2, lty = "longdash",
                                                                     col = "#9933FF", label = model$cutoff)
                                               lattice:: panel.xyplot(x, y, ...)
                                             })
  }

}

#' Plot first order derivation of regression model
#'
#' Plots the values obtained via the first derivation of the regression model
#' in dependence of the norm value. The results indicate the progression of the
#' norm values within each age group. The regression based modeling approach
#' relies on the assumption of a linear progression of the norm values.
#' Negative values in the first order derivation indicate a violation of this
#' assumption. Values near zero
#' are typical for bottom and ceiling effects in the raw data.
#' The regression models usually converge within the range of the original
#' values. In case of vertical and horizontal extrapolation, with increasing
#' distance to the original data, the risk of assumption violation increases
#' as well.
#' @param model The model from the bestModel function
#' @param minAge Age to start with checking
#' @param maxAge Upper end of the age check
#' @param stepAge Stepping parameter for the age check, usually 1 or 0.1; lower
#' values indicate higher precision / closer checks
#' @param minNorm Lower end of the norm value range, in case of T values, 25 might be good
#' @param maxNorm Upper end of the norm value range, in case of T values, 25 might be good
#' @param stepNorm Stepping parameter for norm values
#' @examples
#' # Load example data set, compute model and plot results
#' normData <- prepareData()
#' m <- bestModel(data = normData)
#' plotDerivate(m, minAge=2, maxAge=5, step=.2, minNorm=25, maxNorm=75, stepNorm=1)
#' @export
plotDerivate <- function(model, minAge = 2, maxAge = 5, minNorm = 25,
                         maxNorm = 75, stepAge = 0.2, stepNorm = 1){
  rowS <- base::c(base::seq(minNorm, maxNorm, length.out = 1 + (maxNorm-minNorm)/stepNorm))
  colS <- base::c(base::seq(minAge, maxAge, length.out = 1 + (maxAge-minAge)/stepAge))
  coeff <- cNORM::derive(model)
  devFrame <- base::data.frame(matrix(NA, nrow = length(rowS), ncol = length(colS)))
  dev2 <- base::data.frame()

  colnames(devFrame) <- colS
  rownames(devFrame) <- rowS

  i <- 1
  while(i <= base::ncol(devFrame)){
    j <- 1
    while(j <= base::nrow(devFrame)){
      devFrame[j, i] <- cNORM::predictRaw(rowS[[j]], colS[[i]], coeff, min = -0.2, max = 100000)
      colList <- base::c(rowS[[j]], colS[[i]], devFrame[j, i])
      dev2 <- base::rbind(dev2, colList)
      j <- j + 1
    }
    i <- i + 1

  }
  colnames(dev2) <- base::c("X", "Y", "Z")
  max <- base::max(dev2$Z) + 0.01
  n <- (max + 0.21)*1000
  if (requireNamespace("latticeExtra", quietly = TRUE)) {
  p1 <- lattice::levelplot(Z ~ Y*X, data=dev2,
              at=seq(-.21,max,by=0.001), region=T,
              colorkey=list(at=seq(-.21,max,by=.001)),
              col.regions=rainbow(n, end=.8),
            panel = latticeExtra::panel.2dsmoother,
            main = "Slope of the Regression Function\n(1st order derivation)",
            ylab = "1st order derivate of norm value",
            xlab = "Age"
            )
  }else{
    p1 <- lattice::levelplot(Z ~ Y*X, data=dev2,
                    at=seq(-.21,max,by=0.001), region=T,
                    colorkey=list(at=seq(-.21,max,by=.001)),
                    col.regions=rainbow(n, end=.8),
                    main = "Slope of the Regression Function\n(1st order derivation)",
                    ylab = "1st order derivate of norm value",
                    xlab = "Age"
    )
  }
  p2 <- lattice::contourplot(Z ~ Y*X, data=dev2)

  p3 <- p1 + p2
  p3
}

