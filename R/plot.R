#' Plot manifest and fitted raw scores
#'
#' The function plots the raw data against the fitted scores from
#' the regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' regression line.
#' @param model The regression model from the 'cnorm' function
#' @param group Should the fit be displayed by group?
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#' @examples
#' # Compute model with example dataset and plot results
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotRaw(result)
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @family plot
plotRaw <- function(model, group = FALSE, raw = NULL, type = 0) {

  if(inherits(model, "cnormBetaBinomial2")||inherits(model, "cnormBetaBinomial")){
    stop("This function is not applicable for beta-binomial models.")
  }

  if(!inherits(model, "cnorm")){
    stop("Please provide a cnorm object.")
  }

  d <- model$data
  model <- model$model

  d$fitted <- model$fitted.values
  d$diff <- d$fitted - d$raw
  mse <- round(model$rmse, digits=4)
  r <- round(cor(d$fitted, d$raw, use = "pairwise.complete.obs"), digits = 4)
  d <- as.data.frame(d)

  if (isTRUE(group)) {
    if("group" %in% colnames(d)){
      d$group <- as.factor(d$group)
    } else {
      d$group <- as.factor(getGroups(d$age))
    }
  }

  if (type == 0) {
    p <- ggplot(d, aes_string(x = .data$raw, y = .data$fitted)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(
        title = if(isTRUE(group)) "Observed vs. Fitted Raw Scores by Group" else "Observed vs. Fitted Raw Scores",
        subtitle = paste("r =", r, ", RMSE =", mse),
        x = "Observed Score",
        y = "Fitted Scores"
      )
  } else {
    p <- ggplot(d, aes(x = .data$raw, y = .data$diff)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = if(isTRUE(group)) "Observed Raw Scores vs. Difference Scores by Group" else "Observed Raw Scores vs. Difference Scores",
        subtitle = paste("r =", r, ", RMSE =", mse),
        x = "Observed Score",
        y = "Difference Scores"
      )
  }

  if (isTRUE(group)) {
    p <- p + facet_wrap(~ group)
  }

  p <- p + theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}

#' @title Plot manifest and fitted norm scores
#'
#' @description
#' This function plots the manifest norm score against the fitted norm score from
#' the inverse regression model per group. This helps to inspect the precision
#' of the modeling process. The scores should not deviate too far from
#' the regression line. Applicable for Taylor polynomial models.
#'
#' @param model The regression model, usually from the 'cnorm' or 'cnorm.betabinomial' function
#' @param age In case of beta binomial model, please provide the age vector
#' @param score In case of beta binomial model, please provide the score vector
#' @param group On optional grouping variable, use empty string for no group, the variable name
#'              for Taylor polynomial models or a vector with the groups for beta binomial models
#' @param minNorm lower bound of fitted norm scores
#' @param maxNorm upper bound of fitted norm scores
#' @param type Type of display: 0 = plot manifest against fitted values, 1 = plot
#' manifest against difference values
#'
#' @return A ggplot object representing the norm scores plot.
#'
#' @examples
#' # Load example data set, compute model and plot results
#' \dontrun{
#' # Taylor polynomial model
#' model <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotNorm(model, group="group")
#'
#' # Beta binomial models
#' model.bb <- cnorm.betabinomial(elfe$group, elfe$raw)
#' plotNorm(model.bb, age = elfe$group, score = elfe$raw)
#'
#' }
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @export
#' @family plot
plotNorm <- function(model, age = NULL, score = NULL, weights = NULL, group = "", minNorm = NULL, maxNorm = NULL, type = 0) {


    if(inherits(model, "cnorm")) {
    data <- model$data
    model <- model$model

    if (is.null(minNorm)) {
      minNorm <- model$minL1
    }

    if (is.null(maxNorm)) {
      maxNorm <- model$maxL1
    }

    if (group != "" && !is.null(group) && !(group %in% colnames(data))) {
      warning(paste0("Grouping variable '", group, "' does not exist in data object. Please check variable names and fix 'group' parameter in function call."))
      group <- NULL
    }

    d <- data
    raw <- data[[model$raw]]
    if (attr(data, "useAge"))
      age <- data[[model$age]]
    else
      age <- rep(0, length=nrow(data))

    d$fitted <- predictNorm(raw, age, model, minNorm = minNorm, maxNorm = maxNorm)

    if (group != "" && !is.null(group)) {
      d$group <- as.factor(d[[group]])
    }

  } else if(inherits(model, "cnormBetaBinomial") || inherits(model, "cnormBetaBinomial2")) {
    if(is.null(age) || is.null(score)) {
      stop("Please provide age and score vectors for beta-binomial models.")
    }

    d <- data.frame(age = age, score = score)
    d$groups <- getGroups(age)
    width <- (max(age) - min(age)) / length(unique(d$groups))

    if(is.null(weights))
      d <- rankBySlidingWindow(d, "age", "score", width = width)
    else
      d <- rankBySlidingWindow(d, "age", "score", weights = weights, width = width)

    d$fitted <- predict.cnormBetaBinomial(model, age, score)

    if (is.numeric(group)) {
      d$group <- as.factor(group)
    }

  } else {
    stop("Please provide an object of type cnorm, cnormBetaBinomial or cnormBetaBinomial2.")
  }

  if(!"normValue" %in% colnames(d)) {
    stop("The 'normValue' column is missing from the data. Please ensure it's present for both cnorm and beta-binomial models.")
  }

  d$diff <- d$fitted - d$normValue
  d <- d[!is.na(d$fitted) & !is.na(d$diff), ]

  rmse <- round(sqrt(mean(d$diff^2)), digits = 4)
  r <- round(cor(d$fitted, d$normValue, use = "pairwise.complete.obs"), digits = 4)

  if (type == 0) {
    if(inherits(model, "cnorm")) {
      title <- if(group != "" && !is.null(group)) paste("Observed vs. Fitted Norm Scores by", group) else "Observed vs. Fitted Norm Scores"
    }else{
      title <- if(is.numeric(group)) paste("Observed vs. Fitted Norm Scores by group") else "Observed vs. Fitted Norm Scores"
    }

    p <- ggplot(d, aes(x = .data$normValue, y = .data$fitted)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(
        title = title,
        subtitle = paste("r =", r, ", RMSE =", rmse),
        x = "Observed Scores",
        y = "Fitted Scores"
      )
  } else {
    if(inherits(model, "cnorm")) {
      title <- if(group != "" && !is.null(group)) paste("Observed Norm Scores vs. Difference Scores by", group) else "Observed Norm Scores vs. Difference Scores"
    }else{
      title <- if(is.numeric(group)) paste("Observed Norm Scores vs. Difference Scores by group") else "Observed Norm Scores vs. Difference Scores"
    }

    p <- ggplot(d, aes(x = .data$normValue, y = .data$diff)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = title,
        subtitle = paste("r =", r, ", RMSE =", rmse),
        x = "Observed Scores",
        y = "Difference"
      )
  }

  if(inherits(model, "cnorm")) {
    if (group != "" && !is.null(group)) {
      p <- p + facet_wrap(~ group)
    }
  }else{
    if (is.numeric(group)) {
      p <- p + facet_wrap(~ group)
    }
  }



  p <- p + theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}


#' @importFrom rlang .data
#' @import ggplot2
#' @export
#' @family plot
#'
#' @title Plot norm curves
#'
#' @description
#' This function plots the norm curves based on the regression model. It supports both
#' Taylor polynomial models and beta-binomial models.
#'
#' @param model The model from the bestModel function, a cnorm object, or a cnormBetaBinomial / cnormBetaBinomial2 object.
#' @param normList Vector with norm scores to display. If NULL, default values are used.
#' @param minAge Age to start with checking. If NULL, it's automatically determined from the model.
#' @param maxAge Upper end of the age check. If NULL, it's automatically determined from the model.
#' @param step Stepping parameter for the age check, usually 1 or 0.1; lower scores indicate higher precision.
#' @param minRaw Lower end of the raw score range, used for clipping implausible results. If NULL, it's automatically determined from the model.
#' @param maxRaw Upper end of the raw score range, used for clipping implausible results. If NULL, it's automatically determined from the model.
#'
#' @details
#' Please check the function for inconsistent curves: The different curves should not intersect.
#' Violations of this assumption are a strong indication of violations of model assumptions in
#' modeling the relationship between raw and norm scores.
#'
#' Common reasons for inconsistencies include:
#' 1. Vertical extrapolation: Choosing extreme norm scores (e.g., scores <= -3 or >= 3).
#' 2. Horizontal extrapolation: Using the model scores outside the original dataset.
#' 3. The data cannot be modeled with the current approach, or you need another power parameter (k) or R2 for the model.
#'
#' @return A ggplot object representing the norm curves.
#'
#' @seealso \code{\link{checkConsistency}}, \code{\link{plotDerivative}}, \code{\link{plotPercentiles}}
#'
#' @examples
#' \dontrun{
#' # For Taylor continuous norming model
#' m <- cnorm(raw = ppvt$raw, group = ppvt$group)
#' plotNormCurves(m, minAge=2, maxAge=5)
#'
#' # For beta-binomial model
#' bb_model <- cnorm.betabinomial(age = ppvt$age, score = ppvt$raw, n = 228)
#' plotNormCurves(bb_model)
#' }
plotNormCurves <- function(model,
                           normList = NULL,
                           minAge = NULL,
                           maxAge = NULL,
                           step = 0.1,
                           minRaw = NULL,
                           maxRaw = NULL) {

  if(inherits(model, "cnorm")){
    model <- model$model
  }

  is_beta_binomial <- inherits(model, "cnormBetaBinomial2")

  if(!is_beta_binomial && !model$useAge){
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
  }

  # Get scale information
  if(is_beta_binomial) {
    scaleMean <- attr(model$result, "scaleMean")
    scaleSD <- attr(model$result, "scaleSD")
  } else {
    scaleMean <- model$scaleM
    scaleSD <- model$scaleSD
  }

  if(is.null(normList)){
    normList <- c(-2, -1, 0, 1, 2) * scaleSD + scaleMean
  }

  if (is.null(minAge)) {
    minAge <- if(is_beta_binomial) attr(model$result, "age_mean") - 2 * attr(model$result, "age_sd") else model$minA1
  }

  if (is.null(maxAge)) {
    maxAge <- if(is_beta_binomial) attr(model$result, "age_mean") + 2 * attr(model$result, "age_sd") else model$maxA1
  }

  if (is.null(minRaw)) {
    minRaw <- if(is_beta_binomial) 0 else model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- if(is_beta_binomial) attr(model$result, "max") else model$maxRaw
  }

  valueList <- data.frame(n = factor(), raw = double(), age = double())

  for (norm in normList) {
    if(is_beta_binomial) {
      ages <- seq(minAge, maxAge, by = step)
      raws <- sapply(ages, function(age) {
        pred <- predictCoefficients2(model, age, attr(model$result, "max"))
        qbeta(pnorm((norm - scaleMean) / scaleSD), pred$a, pred$b) * attr(model$result, "max")
      })
      currentDataFrame <- data.frame(n = norm, raw = raws, age = ages)
    } else {
      normCurve <- getNormCurve(norm, model, minAge = minAge, maxAge = maxAge,
                                step = step, minRaw = minRaw, maxRaw = maxRaw)
      currentDataFrame <- data.frame(n = norm, raw = normCurve$raw, age = normCurve$age)
    }
    valueList <- rbind(valueList, currentDataFrame)
  }

  # Create rainbow color palette
  n_colors <- length(unique(valueList$n))
  color_palette <- rainbow(n_colors)

  # Create ggplot
  p <- ggplot(valueList, aes(x = .data$age, y = .data$raw, color = factor(.data$n))) +
    geom_line(size = 1) +
    scale_color_manual(name = "Norm Score",
                       values = color_palette,
                       labels = paste("Norm", normList)) +
    labs(title = "Norm Curves",
         x = "Explanatory Variable (Age)",
         y = "Raw Score") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
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
#' In case, you get 'jagged' or disorganized percentile curve, try to reduce the 'k'
#' parameter in modeling.
#' @param data The raw data including the percentiles and norm scores or a cnorm object
#' @param model The model from the bestModel function (optional)
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
#' @param covariate In case, a covariate has been used, please specify the degree of the covariate /
#' the specific value here. If no covariate is specified, both degrees will be plotted.
#' @seealso plotNormCurves, plotPercentileSeries
#' @examples
#' # Load example data set, compute model and plot results
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotPercentiles(result)
#' @export
#' @family plot
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
                            title = NULL, covariate = NULL) {

  if(inherits(data, "cnorm")){
    model <- data$model
    data <- data$data
  }


  if (!inherits(model, "cnormBetaBinomial")&&!model$useAge){
    # plot
    data1 <- unique(data)
    data1 <- data1[order(data1$raw),]
    step = (model$maxRaw - model$minRaw)/100

    rt <- rawTable(0, model, minRaw = model$minRaw, maxRaw = model$maxRaw)
    plot(normValue ~ raw, data = data1, ylab = "Norm Score", xlab = "Raw Score", col="black",
         main = "Norm Score Plot",
         sub = paste0("Solution: ", model$ideal.model , ", RMSE = ", round(model$rmse, digits = 4)))
    legend(x = "bottomright", legend=c("Manifest Scores", "Regression Model"),
           col=c("black", "blue"), lty=1:2, cex=0.8)
    lines(norm ~ raw, data = rt, col = "blue")

    cat("\nRegresion-based norm table:\n")
    print(rt)

    return()
  }


  if(!is.null(covariate)&&is.null(model$covariate)){
    warning("Covariate specified but no covariate available in the model. Setting covariate to NULL.")
    covariate = NULL
  }else if(is.null(covariate)&&!is.null(model$covariate)){
    degree <- unique(data[, attr(data, "covariate")])

    if (is.null(title)) {
      title <- paste0("Observed and Predicted Percentile Curves\nModel: ", model$ideal.model, ", R2 = ", round(model$subsets$adjr2[[model$ideal.model]], digits = 4), ", Covariates: ", degree[[1]], " versus ", degree[[2]])
    }

    trel <- c(plotPercentiles(data, model, covariate = degree[[1]],
                              minRaw = minRaw, maxRaw = maxRaw,
                              minAge = minAge, maxAge = maxAge,
                              raw = raw, group = group, percentiles = percentiles,
                              scale = scale, title = title),
              plotPercentiles(data, model, covariate = degree[[2]],
              minRaw = minRaw, maxRaw = maxRaw,
              minAge = minAge, maxAge = maxAge,
              raw = raw, group = group, percentiles = percentiles,
              scale = scale, title = title))
    return(base::print(trel))
  }

  if(!is.null(model$covariate)){
    d <- data[data[[model$covariate]] == covariate, ]
    model$fitted.values <- model$fitted.values[data[[model$covariate]] == covariate]
    data <- d
  }



  if (is.null(group)) {
    group <- attr(data, "group")
  }

  if(is.null(data[[group]])){
    data$group <- getGroups(data[, attributes(data)$age])
    group <- "group"
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

  if (typeof(group) == "logical" && !group) {
    stop("The plotPercentiles-function does not work without a grouping variable.")
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
    xyFunction <- paste(paste(NAMES, collapse = " + "),
      paste(NAMESP, collapse = " + "),
      sep = " + ", collapse = " + "
    )
    xyFunction <- paste(xyFunction, group, sep = " ~ ")

  w <- attributes(data)$weights
  data[, group] <- round(data[, group], digits=3)
  AGEP <- unique(data[, group])

  # get actual percentiles
  if(!is.null(attr(data, "descend"))&&attr(data, "descend")){
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data[, group]), function(df){weighted.quantile(df[, raw], probs = 1 - percentiles, weights = df$w)})))
  }else{
    percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data[, group]), function(df){weighted.quantile(df[, raw], probs = percentiles, weights = df$w)})))
  }
  percentile.actual$group <- as.numeric(rownames(percentile.actual))
  colnames(percentile.actual) <- c(NAMES, c(group))
  rownames(percentile.actual) <- AGEP

  # build finer grained grouping variable for prediction and fit predicted percentiles
  share <- seq(from = model$minA1, to = model$maxA1, length.out = 100)
  AGEP <- c(AGEP, share)
  percentile.fitted <- data.frame(matrix(NA,
                                         nrow = length(AGEP),
                                         ncol = length(T)
  ))

  for(i in 1:length(AGEP)){
    percentile.fitted[i, ] <- predictRaw(T, AGEP[[i]], model$coefficients, minRaw = minRaw, maxRaw = maxRaw)
  }

  percentile.fitted$group <- AGEP
  percentile.fitted <- percentile.fitted[!duplicated(percentile.fitted$group), ]
  colnames(percentile.fitted) <- c(NAMESP, c(group))
  rownames(percentile.fitted) <- percentile.fitted$group

  # Merge actual and predicted scores and plot them show lines
  # for predicted scores and dots for actual scores
  percentile <- merge(percentile.actual, percentile.fitted,
    by = group, all = TRUE
  )

  END <- .8
  COL1 <- rainbow(length(percentiles), end = END)
  COL2 <- c(rainbow(length(percentiles), end = END), rainbow(length(percentiles), end = END))

  panelfun <- function(..., type, group.number) {
    if (group.number > length(T)) {
      panel.lines(...)
    } else {
      panel.points(..., type = "p")
    }
  }

  if (is.null(title)) {
    title <- paste0("Observed and Predicted Percentile Curves\nModel: ", model$ideal.model, ", R2 = ", round(model$subsets$adjr2[[model$ideal.model]], digits = 4))
  }

  chart <- xyplot(formula(xyFunction), percentile,
    panel = function(...)
      panel.superpose(..., panel.groups = panelfun),
    main = title,
    ylab = paste0("Raw Score (", raw, ")"), xlab = paste0("Explanatory Variable (", group, ")"),
    col = COL2, lwd = 1.5, grid = TRUE,
    key = list(
      corner = c(0.99, 0.01),
      lines = list(col = COL1, lwd = 1.5),
      text = list(NAMES)
    )
  )

  base::print(chart)
  return(chart)
}


#' Plot the density function per group by raw score
#'
#' This function plots density curves based on the regression model against the raw scores.
#' It supports both traditional continuous norming models and beta-binomial models.
#' The function allows for customization of the plot range and groups to be displayed.
#'
#' @param model The model from the bestModel function, a cnorm object, or a cnormBetaBinomial or cnormBetaBinomial2 object.
#' @param minRaw Lower bound of the raw score. If NULL, it's automatically determined based on the model type.
#' @param maxRaw Upper bound of the raw score. If NULL, it's automatically determined based on the model type.
#' @param minNorm Lower bound of the norm score. If NULL, it's automatically determined based on the model type.
#' @param maxNorm Upper bound of the norm score. If NULL, it's automatically determined based on the model type.
#' @param group Numeric vector specifying the age groups to plot. If NULL, groups are automatically selected.
#'
#' @return A ggplot object representing the density functions.
#'
#' @details
#' The function generates density curves for specified age groups, allowing for easy comparison of score distributions
#' across different ages.
#'
#' For beta-binomial models, the density is based on the probability mass function, while for
#' traditional models, it uses a normal distribution based on the norm scores.
#'
#' @note
#' Please check for inconsistent curves, especially those showing implausible shapes
#' such as violations of biuniqueness in the cnorm models.
#'
#' @seealso \code{\link{plotNormCurves}}, \code{\link{plotPercentiles}}
#'
#' @examples
#' \dontrun{
#' # For traditional continuous norming model
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotDensity(result, group = c(2, 4, 6))
#'
#' # For beta-binomial model
#' bb_model <- cnorm.betabinomial(age = ppvt$age, score = ppvt$raw, n = 228)
#' plotDensity(bb_model)
#' }
#'
#' @import ggplot2
#' @export
#' @family plot
plotDensity <- function(model,
                        minRaw = NULL,
                        maxRaw = NULL,
                        minNorm = NULL,
                        maxNorm = NULL,
                        group = NULL) {

  if(inherits(model, "cnorm")){
    model <- model$model
  }

  is_beta_binomial <- inherits(model, "cnormBetaBinomial")||inherits(model, "cnormBetaBinomial2")

  if (is.null(minNorm)) {
    minNorm <- if(is_beta_binomial) -3 else model$minL1
  }

  if (is.null(maxNorm)) {
    maxNorm <- if(is_beta_binomial) 3 else model$maxL1
  }

  if (is.null(minRaw)) {
    minRaw <- if(is_beta_binomial) 0 else model$minRaw
  }

  if (is.null(maxRaw)) {
    maxRaw <- if(is_beta_binomial) attr(model$result, "max") else model$maxRaw
  }

  if (is.null(group)) {
    if(is_beta_binomial) {
      age_min <- attr(model$result, "age_mean") - 2 * attr(model$result, "age_sd")
      age_max <- attr(model$result, "age_mean") + 2 * attr(model$result, "age_sd")
      group <- round(seq(from = age_min, to = age_max, length.out = 4), digits = 3)
    } else if(model$useAge) {
      group <- round(seq(from = model$minA1, to = model$maxA1, length.out = 4), digits = 3)
    } else {
      group <- c(1)
    }
  }

  step <- (maxNorm - minNorm) / 100

  matrix_list <- lapply(group, function(g) {
    if(is_beta_binomial) {
      norm <- normTable.betabinomial(model, g, attr(model$result, "max"))[[1]]
      norm$group <- rep(g, length.out = nrow(norm))
      colnames(norm)[colnames(norm) == "x"] <- "raw"
      colnames(norm)[colnames(norm) == "norm"] <- "norm1"
      colnames(norm)[colnames(norm) == "z"] <- "norm"
    } else {
      norm <- normTable(g, model = model, minNorm = minNorm, maxNorm = maxNorm, minRaw = minRaw, maxRaw = maxRaw, step = step, pretty = FALSE)
      norm$group <- rep(g, length.out = nrow(norm))
    }
    return(norm)
  })

  matrix <- do.call(rbind, matrix_list)
  matrix <- matrix[matrix$norm > minNorm & matrix$norm < maxNorm, ]
  matrix <- matrix[matrix$raw > minRaw & matrix$raw < maxRaw, ]

  if(is_beta_binomial) {
    matrix$density <- matrix$Px
  } else {
    matrix$density <- dnorm(matrix$norm, mean = model$scaleM, sd = model$scaleSD)
  }

  # Create ggplot
  title <- ""
  if(is_beta_binomial) {
    title <- "Density Functions (Beta-Binomial)"
  } else {
    title <- "Density Functions (Taylor Polynomial)"
  }
  p <- ggplot(matrix, aes(x = .data$raw, y = .data$density, color = factor(.data$group))) +
    geom_line(size = 1, na.rm = TRUE) +
    scale_color_viridis_d(name = "Age",
                          labels = paste("Age", group),
                          option = "plasma") +
    labs(title = title,
         x = "Raw Score",
         y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  return(p)
}


#' Generates a series of plots with number curves by percentile for different models
#'
#' This functions makes use of 'plotPercentiles' to generate a series of plots
#' with different number of predictors. It draws on the information provided by the model object
#' to determine the bounds of the modeling (age and standard score range). It can be used as an
#' additional model check to determine the best fitting model. Please have a look at the
#'' plotPercentiles' function for further information.
#' @param data The raw data including the percentiles and norm scores or a cnorm object
#' @param model The model from the bestModel function (optional)
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
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotPercentileSeries(result, start=1, end=5, group="group")
#' @family plot
plotPercentileSeries <- function(data, model, start = 1, end = NULL, group = NULL,
                                 percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                                 type = 7,
                                 filename = NULL) {

  is_beta_binomial <- inherits(model, "cnormBetaBinomial2")||inherits(model, "cnormBetaBinomial")
  if(is_beta_binomial){
    stop("This function is not applicable for beta-binomial models. Please use the plotDensity function instead.")
  }

  if(inherits(data, "cnorm")){
    model <- data$model
    d <- data$data
  }else{
    d <- as.data.frame(data)
  }

  if(!is.null(model$covariate)){
    stop("This function us currently not able to handle models with covariates.")
  }


  if (!attr(d, "useAge")){
    stop("Age or group variable explicitely set to FALSE in dataset. No plotting available.")
  }

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
    bestformula$useAge <- model$useAge
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


    l[[length(l) + 1]] <- plotPercentiles(d, bestformula,
      minAge = model$minA1, maxAge = model$maxA1,
      minRaw = minR,
      maxRaw = maxR,
      percentiles = percentiles,
      scale = NULL,
      group = group,
      title = paste0("Observed and Predicted Percentiles\nModel with ", bestformula$subsets$numberOfTerms[[start]], " predictors, R2=", round(bestformula$subsets$adjr2[[start]], digits = 4))
    )

    if (!is.null(filename)) {
      trellis.device(device = "png", filename = paste0(filename, start, ".png"))
      base::print(l[[length(l)]])
      dev.off()
    }
    start <- start + 1
  }
  return(l)
}


#' Evaluate information criteria for regression model
#'
#' This function plots various information criteria and model fit statistics against
#' the number of predictors or adjusted R-squared, depending on the type of plot selected.
#' It helps in model selection by visualizing different aspects of model performance.
#'
#' @param model The regression model from the bestModel function or a cnorm object.
#' @param type Integer specifying the type of plot to generate:
#'   \itemize{
#'     \item 0: Adjusted R² by number of predictors (default)
#'     \item 1: Log-transformed Mallow's Cp by adjusted R²
#'     \item 2: Bayesian Information Criterion (BIC) by adjusted R²
#'     \item 3: Root Mean Square Error (RMSE) by number of predictors
#'     \item 4: Residual Sum of Squares (RSS) by number of predictors
#'     \item 5: F-test statistic for consecutive models by number of predictors
#'     \item 6: p-value for model tests by number of predictors
#'   }
#'
#' @return A ggplot object representing the selected information criterion plot.
#'
#' @details
#' The function generates different plots to help in model selection:
#'
#' - For types 1 and 2 (Mallow's Cp and BIC), look for the "elbow" in the curve where
#'   the information criterion begins to drop. This often indicates a good balance
#'   between model fit and complexity.
#' - For type 0 (Adjusted R2), higher values indicate better fit, but be cautious
#'   of overfitting with values approaching 1.
#' - For types 3 and 4 (RMSE and RSS), lower values indicate better fit.
#' - For type 5 (F-test), higher values suggest significant improvement with added predictors.
#' - For type 6 (p-values), values below the significance level (typically 0.05)
#'   suggest significant improvement with added predictors.
#'
#' The R² cutoff specified in the bestModel function is displayed as a dashed line
#' where applicable.
#'
#' @note
#' It's important to balance statistical measures with practical considerations and
#' to visually inspect the model fit using functions like \code{plotPercentiles}.
#'
#' @seealso \code{\link{bestModel}}, \code{\link{plotPercentiles}}, \code{\link{printSubset}}
#'
#' @examples
#' \dontrun{
#' # Compute model with example data and plot information function
#' cnorm.model <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotSubset(cnorm.model)
#'
#' # Plot BIC against adjusted R-squared
#' plotSubset(cnorm.model, type = 2)
#'
#' # Plot RMSE against number of predictors
#' plotSubset(cnorm.model, type = 3)
#' }
#'
#' @import ggplot2
#' @export
#' @family plot
plotSubset <- function(model, type = 0) {
  if(inherits(model, "cnorm")){
    model <- model$model
  }

  # Compute F and significance
  RSS1 <- c(NA, model$subsets$rss)
  RSS2 <- c(model$subsets$rss, NA)
  k1 <- seq(from = 1, to = length(RSS1))
  k2 <- seq(from = 2, to = length(RSS1) + 1)
  df1 <- k2 - k1
  df2 <- length(model$fitted.values) - k2
  F <- ((RSS1-RSS2)/df1)/(RSS2/df2)
  p <- 1 - pf(F, df1, df2)

  dataFrameTMP <- data.frame(
    adjr2 = model$subsets$adjr2,
    bic = model$subsets$bic,
    cp = model$subsets$cp,
    RSS = model$subsets$rss,
    RMSE = sqrt(model$subsets$rss / length(model$fitted.values)),
    F = head(F, -1),
    p = head(p, -1),
    nr = seq(1, length(model$subsets$adjr2), by = 1)
  )

  # Improved base theme
  theme_custom <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )

  # Base plot
  p <- ggplot(dataFrameTMP) + theme_custom

  # Custom color palette
  custom_colors <- c("Model in Ascending Order" = "#1f77b4", "Cutoff Value" = "#d62728", "p = .05" = "#d62728")

  # Define plot based on type
  if (type == 1) {
    p <- p +
      geom_line(aes(x = .data$adjr2, y = .data$cp, color = "Model in Ascending Order"), size = .75) +
      geom_point(aes(x = .data$adjr2, y = .data$cp), size = 2.5, color = "#1f77b4") +
      scale_y_log10() +
      labs(title = "Information Function: Mallows's Cp",
           x = "Adjusted R²",
           y = "log-transformed Mallows's Cp") +
      geom_vline(aes(xintercept = model$cutoff, color = "Cutoff Value"), linetype = "dashed", size = 1) +
      scale_color_manual(values = custom_colors)
  } else if (type == 2) {
    p <- p +
      geom_line(aes(x = .data$adjr2, y = .data$bic, color = "Model in Ascending Order"), size = .75) +
      geom_point(aes(x = .data$adjr2, y = .data$bic), size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: BIC",
           x = "Adjusted R²",
           y = "Bayesian Information Criterion (BIC)") +
      geom_vline(aes(xintercept = model$cutoff, color = "Cutoff Value"), linetype = "dashed", size = 1) +
      scale_color_manual(values = custom_colors)
  } else if (type == 3) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$RMSE, color = "Model in Ascending Order"), size = .75) +
      geom_point(aes(x = .data$nr, y = .data$RMSE), size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: RMSE",
           x = "Number of Predictors",
           y = "Root Mean Square Error (Raw Score)") +
      scale_color_manual(values = custom_colors)
  } else if (type == 4) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$RSS, color = "Model in Ascending Order"), size = .75) +
      geom_point(aes(x = .data$nr, y = .data$RSS), size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: RSS",
           x = "Number of Predictors",
           y = "Residual Sum of Squares (RSS)") +
      scale_color_manual(values = custom_colors)
  } else if (type == 5) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$F, color = "Model in Ascending Order"), na.rm = TRUE, size = .75) +
      geom_point(aes(x = .data$nr, y = .data$F), na.rm = TRUE, size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: F-test Statistics",
           x = "Number of Predictors",
           y = "F-test Statistics for Consecutive Models") +
      scale_color_manual(values = custom_colors)
  } else if (type == 6) {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$p, color = "Model in Ascending Order"), na.rm = TRUE, size = .75) +
      geom_point(aes(x = .data$nr, y = .data$p), na.rm = TRUE, size = 2.5, color = "#1f77b4") +
      ylim(-0.005, 0.11) +
      labs(title = "Information Function: p-values",
           x = "Number of Predictors",
           y = "p-values for Tests on R² adj. of Consecutive Models") +
      geom_hline(aes(yintercept = 0.05, color = "p = .05"), linetype = "dashed", size = 1) +
      scale_color_manual(values = custom_colors)
  } else {
    p <- p +
      geom_line(aes(x = .data$nr, y = .data$adjr2, color = "Model in Ascending Order"), na.rm = TRUE, size = .75) +
      geom_point(aes(x = .data$nr, y = .data$adjr2), na.rm = TRUE, size = 2.5, color = "#1f77b4") +
      labs(title = "Information Function: Adjusted R²",
           x = "Number of Predictors",
           y = "Adjusted R²") +
      geom_hline(aes(yintercept = model$cutoff, color = "Cutoff Value"), linetype = "dashed", size = 1) +
      scale_color_manual(values = custom_colors)
  }

  # Add legend title
  p <- p + labs(color = "")

  return(p)
}

#'
#' @title Plot first order derivative of regression model
#'
#' @description
#' This function plots the scores obtained via the first order derivative of the regression model
#' in dependence of the norm score.
#'
#' @param model The model from the bestModel function, a cnorm object.
#' @param minAge Minimum age to start checking. If NULL, it's automatically determined from the model.
#' @param maxAge Maximum age for checking. If NULL, it's automatically determined from the model.
#' @param minNorm Lower end of the norm score range. If NULL, it's automatically determined from the model.
#' @param maxNorm Upper end of the norm score range. If NULL, it's automatically determined from the model.
#' @param stepAge Stepping parameter for the age check, usually 1 or 0.1; lower values indicate higher precision.
#' @param stepNorm Stepping parameter for norm scores.
#' @param order Degree of the derivative (default = 1).
#'
#' @details
#' The results indicate the progression of the norm scores within each age group. The regression-based
#' modeling approach relies on the assumption of a linear progression of the norm scores. Negative scores
#' in the first order derivative indicate a violation of this assumption. Scores near zero are typical
#' for bottom and ceiling effects in the raw data.
#'
#' The regression models usually converge within the range of the original values. In case of vertical
#' and horizontal extrapolation, with increasing distance to the original data, the risk of assumption
#' violation increases as well.
#'
#' @note
#' This function is currently incompatible with reversed raw score scales ('descent' option).
#'
#' @return A ggplot object representing the derivative of the regression function.
#'
#' @seealso \code{\link{checkConsistency}}, \code{\link{bestModel}}, \code{\link{derive}}
#'
#' @examples
#' # For traditional continuous norming model
#' result <- cnorm(raw = elfe$raw, group = elfe$group)
#' plotDerivative(result, minAge=2, maxAge=5, stepAge=.2, minNorm=25, maxNorm=75, stepNorm=1)
#'
#'
#' @import ggplot2
#' @export
#' @family plot
plotDerivative <- function(model,
                           minAge = NULL,
                           maxAge = NULL,
                           minNorm = NULL,
                           maxNorm = NULL,
                           stepAge = 0.2,
                           stepNorm = 1,
                           order = 1) {

  if(inherits(model, "cnorm")){
    model <- model$model
  }

  is_beta_binomial <- inherits(model, "cnormBetaBinomial2")||inherits(model, "cnormBetaBinomial")
  if(is_beta_binomial){
    stop("This function is not applicable for beta-binomial models. Please use the plotDensity function instead.")
  }

  if (!model$useAge){
    stop("Age or group variable explicitly set to FALSE in dataset. No plotting available.")
  }

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

  rowS <- seq(minNorm, maxNorm, by = stepNorm)
  colS <- seq(minAge, maxAge, by = stepAge)

  coeff <- derive(model, order)
  cat(paste0(rangeCheck(model, minAge, maxAge, minNorm, maxNorm), " Coefficients from the ", order, " order derivative function:\n\n"))
  print(coeff)


  dev2 <- expand.grid(X = rowS, Y = colS)
  dev2$Z <- mapply(function(norm, age) predictRaw(norm, age, coeff), dev2$X, dev2$Y)

  desc <- paste0(order, switch(order, "st", "nd", "rd", "th"), " Order Derivative")

  custom_palette <- c("#2D1160", "#3B28B1", "#0C7BDC", "#24C7C7", "#66E64D", "#CFEA44", "#FEFD54")

  p <- ggplot(dev2, aes(x = .data$Y, y = .data$X, z = .data$Z)) +
    geom_tile(aes(fill = .data$Z)) +
    geom_contour(color = "white", alpha = 0.5) +
    scale_fill_gradientn(colors = custom_palette) +
    labs(title = paste("Slope of the Regression Function - ", desc),
         x = "Explanatory Variable (Age)",
         y = "Norm Score",
         fill = "Derivative") +
    theme_minimal() +
    theme(legend.position = "right")

  return(p)
}

#' General convencience plotting function
#'
#' @param x a cnorm object
#' @param y the type of plot as a string, can be one of
#' 'raw' (1), 'norm' (2), 'curves' (3), 'percentiles' (4), 'series' (5), 'subset' (6),
#' or 'derivative' (7), either as a string or the according index
#' @param ... additional parameters for the specific plotting function
#'
#' @export
plotCnorm <- function(x, y, ...){
  if(!inherits(x, "cnorm")||!is.character(y)){
    message("Please provide a cnorm object as parameter x and the type of plot as a string for parameter y, which can be 'raw', 'norm', 'curves', 'percentiles', 'series', 'subset', or 'derivative'.")
    return()
  }

  if(y == "raw" || y == 1){
    plotRaw(x, ...)
  }else if(y == "norm" || y == 2){
    plotNorm(x, ...)
  }else if(y == "curves" || y == 3){
    plotNormCurves(x, ...)
  }else if(y == "percentiles" || y == 4){
    plotPercentiles(x, ...)
  }else if(y == "density" || y == 5){
    plotDensity(x, ...)
  }else if(y == "series" || y == 6){
    plotPercentileSeries(x, ...)
  }else if(y == "subset" || y == 7){
    plotSubset(x, ...)
  }else if(y == "derivative" || y == 8){
    plotDerivative(x, ...)
  }else{
    plotPercentiles(x, ...)
    message("Please provide the type of plot as a string for parameter y, which can be 'raw', 'norm', 'curves', 'percentiles', 'series', 'subset', 'derivative' or the according index.")
  }
}
