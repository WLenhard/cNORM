#' Fit a LASSO Regression Model for Continuous Norming with Standardization
#'
#' This function fits a LASSO regression model for continuous norming, with options for
#' ensuring monotonicity and cross-validation. It standardizes age, location, and raw scores
#' before fitting the model.
#'
#' @param raw A numeric vector of raw scores.
#' @param age A numeric vector of age values corresponding to the raw scores.
#' @param weights An optional numeric vector of weights for the observations.
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as
#' well, be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param k The maximum power for location terms in the model (default is 5).
#' @param t The maximum power for age terms in the model (default is 3).
#' @param nfolds The number of folds to use in cross-validation (default is 10).
#' @param max_attempts The maximum number of lambda values to try when searching for a monotonic model (default is 100).
#' @param plot Logical; if TRUE, plots the fitted model (default is FALSE).
#'
#' @return An object of class "cnormLasso" containing the fitted model and various model details.
#'
#' @import glmnet
#' @export
cnorm.lasso <- function(raw, age, weights = NULL, scale = "T", descend = FALSE, k = 5, t = 3, nfolds = 10, max_attempts = 100, plot = TRUE) {
  require(glmnet)

  # Rank data
  data <- data.frame(raw = raw, group = age)
  ### TODO, decide between norm by group or by sliding window
  ### TODO descend
  ### TODO scale info scaleM, scaleSD correctl attributed
  data <- rankByGroup(data, weights=weights, scale=scale, descend = descend)
  location <- data$normValue
  age = data$group
  raw <- data$raw
  minNorm <- min(data$normValue)
  maxNorm <- max(data$normValue)

  # Prepare the matrix with standardized variables
  X <- prepare_matrix(location, age, k, t)

  # Function to check monotonicity (using standardized variables)
  check_monotonicity <- function(coef) {
    min_age <- min(age)
    max_age <- max(age)

    # Function to predict raw score (standardized)
    predict_raw <- function(loc, age) {
      terms <- c(1, sapply(1:k, function(i) loc^i),
                 sapply(1:t, function(j) age^j),
                 sapply(1:k, function(i)
                   sapply(1:t, function(j) loc^i * age^j)))
      sum(coef * terms)
    }

    # Check monotonicity at min and max age
    loc_seq <- seq(min(location), max(location), length.out = 100)

    pred_min_age <- sapply(loc_seq, predict_raw, age = min_age)
    pred_max_age <- sapply(loc_seq, predict_raw, age = max_age)

    is_monotonic_min <- all(diff(pred_min_age) >= 0) || all(diff(pred_min_age) <= 0)
    is_monotonic_max <- all(diff(pred_max_age) >= 0) || all(diff(pred_max_age) <= 0)

    return(is_monotonic_min && is_monotonic_max)
  }

  # Perform cross-validation
  cv_fit <- cv.glmnet(X, raw, nfolds = nfolds, alpha = .5)

  # Get the lambdas sorted by their mean cross-validated error (ascending order)
  lambda_sequence <- cv_fit$lambda[order(cv_fit$cvm)]

  # Try different lambda values until a monotonic model is found
  for (i in 1:min(max_attempts, length(lambda_sequence))) {
    # Extract coefficients for the current lambda
    coef <- as.vector(coef(cv_fit, s = lambda_sequence[i]))

    # Perform variable selection
    selected_vars <- which(abs(coef[-1]) > 0)  # Exclude intercept
    coef_selected <- coef[c(1, selected_vars + 1)]  # Include intercept

    # Check monotonicity
    if (check_monotonicity(coef_selected)) {
      # Monotonic model found
      coef_names <- c("Intercept", colnames(X)[selected_vars])
      coef_named <- setNames(coef_selected, coef_names)

      model <- list(
        coefficients = coef_named,
        cv_fit = cv_fit,
        lambda = lambda_sequence[i],
        lambda_index = i,
        is_monotonic = TRUE,
        selected_vars = coef_names[-1],  # Exclude intercept from selected variables list
        raw = data$raw,
        age = data$group,
        location = data$normValue,
        weights = weights,
        minRaw = min(raw),
        maxRaw = max(raw),
        scaleM = 50,
        scaleSD = 10,
        minNorm = minNorm,
        maxNorm = maxNorm,
        k = k,
        t = t
      )

      class(model) <- "cnormLasso"

      if(plot) {
        p <- plot(model)
        print(p)
      }
      return(model)
    }
  }

  # If we've reached this point, no monotonic model was found
  warning("No monotonic model found after ", min(max_attempts, length(lambda_sequence)), " attempts. Returning best fitting model.")

  # Use the lambda that gives minimum mean cross-validated error
  best_lambda <- cv_fit$lambda.min
  coef <- as.vector(coef(cv_fit, s = best_lambda))

  # Perform variable selection
  selected_vars <- which(abs(coef[-1]) > 0)  # Exclude intercept
  coef_selected <- coef[c(1, selected_vars + 1)]  # Include intercept

  # Create a named vector of selected coefficients
  coef_names <- c("Intercept", colnames(X)[selected_vars])
  coef_named <- setNames(coef_selected, coef_names)

  model <- list(
    coefficients = coef_named,
    cv_fit = cv_fit,
    lambda = best_lambda,
    lambda_index = which(cv_fit$lambda == best_lambda),
    is_monotonic = FALSE,
    selected_vars = coef_names[-1],  # Exclude intercept from selected variables list
    raw = data$raw,
    age = data$group,
    location = data$normValue,
    weights = weights,
    minRaw = min(raw),
    maxRaw = max(raw),
    scaleM = 50,
    scaleSD = 10,
    minNorm = minNorm,
    maxNorm = maxNorm,
    k = k,
    t = t
  )

  class(model) <- "cnormLasso"
  if(plot) {
    p <- plot(model)
    print(p)
  }
  return(model)
}

plot.cnormLasso <- function(model, percentiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), points = TRUE) {
  if(!inherits(model, "cnormLasso"))
    stop("Input must be a 'cnormLasso' object.")

  score <- model$raw
  age <- model$age
  weights <- model$weights

  # Generate prediction points
  n_points <- 100
  data <- data.frame(age = age, score = score)
  if (!is.null(weights)) {
    data$w <- weights
  } else {
    data$w <- rep(1, length(age))
  }

  age_range <- range(age)
  pred_ages <- seq(age_range[1], age_range[2], length.out = n_points)

  # Get predictions for percentiles
  percentile_lines <- lapply(percentiles, function(p) {
    norm_score <- qnorm(p) * model$scaleSD + model$scaleM
    predictRaw(rep(norm_score, n_points), pred_ages, model$coefficients, minRaw = model$minRaw, maxRaw = model$maxRaw)
  })

  percentile_data <- do.call(cbind, percentile_lines)
  colnames(percentile_data) <- paste0("P", percentiles * 100)

  plot_data <- data.frame(
    age = pred_ages,
    percentile_data
  )

  # Create the plot
  p <- ggplot()

  if (points)
    p <- p + geom_point(
      data = data,
      aes(x = age, y = score),
      alpha = 0.2,
      size = 0.6
    )

  # Calculate and add manifest percentiles
  if (length(age) / length(unique(age)) > 50) {
    # Distinct age groups
    data$group <- age
  } else {
    # Use cut function to create age groups
    data$group <- cut(age, breaks = 10)
  }

  # get actual percentiles
  NAMES <- paste("PR", percentiles * 100, sep = "")
  percentile.actual <- as.data.frame(do.call("rbind", lapply(split(data, data$group), function(df) {
    c(age = mean(df$age),
      quantile(df$score, probs = percentiles, weights = df$w))
  })))
  colnames(percentile.actual) <- c("age", NAMES)
  manifest_data <- percentile.actual

  # Add percentile lines and points with proper legend
  for (i in seq_along(percentiles)) {
    p <- p +
      geom_line(
        data = plot_data,
        aes(
          x = .data$age,
          y = .data[[paste0("P", percentiles[i] * 100)]],
          color = !!NAMES[i]
        ),
        size = 0.6
      ) +
      geom_point(
        data = manifest_data,
        aes(
          x = .data$age,
          y = .data[[NAMES[i]]],
          color = !!NAMES[i]
        ),
        size = 2,
        shape = 18
      )
  }

  # Customize the plot
  p <- p +
    theme_minimal() +
    labs(
      title = "Percentile Plot (LASSO Model)",
      x = "Age",
      y = "Score",
      color = "Percentile"
    ) +
    scale_color_manual(
      values = setNames(rainbow(length(percentiles)), NAMES),
      breaks = NAMES,
      labels = paste0(percentiles * 100, "%")
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = rep("solid", length(NAMES)),
      shape = rep(18, length(NAMES))
    )))

  p <- p +
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

#' Summary of cnormLasso Model
#'
#' This function provides a summary of a cnormLasso model, including various measures of model fit,
#' LASSO details, model parameters, selected variables, and coefficients.
#'
#' @param object An object of class "cnormLasso", typically the result of a call to cnorm.lasso().
#'
#' @return Prints a summary of the cnormLasso model and invisibly returns NULL.
#'
#' @details The summary includes the following information:
#' \itemize{
#'   \item Model Fit Statistics: R-squared, Adjusted R-squared, RMSE, MAE, AIC, and BIC
#'   \item LASSO Details: Lambda value, Lambda index, and monotonicity status
#'   \item Model Parameters: Maximum powers for location and age, number of predictors
#'   \item Selected Variables: List of variables selected by LASSO
#'   \item Coefficients: Estimated coefficients for all terms in the model
#' }
#'
#' @examples
#' model <- cnorm.lasso(elfe$raw, elfe$group)
#' summary(model)
#'
#' @export
summary.cnormLasso <- function(object, ...) {
  if (!inherits(object, "cnormLasso")) {
    stop("Object must be of class 'cnormLasso'")
  }

  # Prepare standardized input data
  location <- object$location
  age <- object$age

  # Calculate predicted values (standardized)
  predicted <- predictRaw(location, age, object$coefficients)

  # Calculate R-squared
  SST <- sum((object$raw - mean(object$raw))^2)
  SSE <- sum((object$raw - predicted)^2)
  R_squared <- 1 - SSE / SST

  # Calculate adjusted R-squared
  n <- length(object$raw)
  p <- length(object$coefficients) - 1  # number of predictors (excluding intercept)
  adj_R_squared <- 1 - (1 - R_squared) * (n - 1) / (n - p - 1)

  # Calculate RMSE
  RMSE <- sqrt(mean((object$raw - predicted)^2))

  # Calculate MAE
  MAE <- mean(abs(object$raw - predicted))

  # Calculate AIC and BIC
  RSS <- sum((object$raw - predicted)^2)
  AIC <- n * log(RSS/n) + 2 * p
  BIC <- n * log(RSS/n) + log(n) * p

  # Create summary output
  cat("Summary of cnormLasso Model:\n\n")
  cat("Model Fit Statistics:\n")
  cat("  R-squared:       ", round(R_squared, 4), "\n")
  cat("  Adjusted R-squared:", round(adj_R_squared, 4), "\n")
  cat("  RMSE:            ", round(RMSE, 4), "\n")
  cat("  MAE:             ", round(MAE, 4), "\n")
  cat("  AIC:             ", round(AIC, 2), "\n")
  cat("  BIC:             ", round(BIC, 2), "\n\n")

  cat("LASSO Details:\n")
  cat("  Lambda:          ", round(object$lambda, 6), "\n")
  cat("  Lambda index:    ", object$lambda_index, "\n")
  cat("  Is monotonic:    ", object$is_monotonic, "\n\n")

  cat("Model Parameters:\n")
  cat("  k (location power):   ", object$k, "\n")
  cat("  t (age power):        ", object$t, "\n")
  cat("  Number of predictors: ", p, "\n\n")

  cat("Selected Variables:\n")
  print(object$selected_vars)
  cat("\n")

  cat("Coefficients (standardized scale):\n")
  print(object$coefficients)
}
