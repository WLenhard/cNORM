set.seed(123)
age <- ppvt$age
score <- ppvt$raw
marginals <- data.frame(var = c("sex", "sex", "migration", "migration"),
                        level = c(1,2,0,1),
                        prop = c(0.51, 0.49, 0.65, 0.35))
weights <- computeWeights(ppvt, marginals)
n <- 228

context("Modelling")
test_that("ranking functions work without warnings or errors", {
  skip_on_cran()
  expect_silent(rankByGroup(elfe, group = "group", raw = "raw"))
  expect_silent(rankBySlidingWindow(ppvt, age = ppvt$age, width=1, raw=ppvt$raw))
  expect_silent(rankByGroup(ppvt, raw=ppvt$raw, weights = ppvt$sex))
})

test_that("comprehensive function works without warnings or errors", {
  skip_on_cran()
  expect_output(cnorm(raw = elfe$raw, group = elfe$group))
  expect_output(cnorm(raw = ppvt$raw, group = ppvt$group, weights = weights))
  expect_output(cnorm(raw = ppvt$raw, age = ppvt$age, width=1))
})

test_that("plotting works without warnings or errors", {
  skip_on_cran()
  cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
  expect_silent(plot(cnorm.elfe, "raw"))
  expect_silent(plot(cnorm.elfe, "percentiles"))
  expect_output(plot(cnorm.elfe, "derivative"))
  expect_silent(plot(cnorm.elfe, "curves"))
  expect_silent(plot(cnorm.elfe, "subset"))
})

test_that("functions return expected output types", {
  skip_on_cran()
  expect_is(rankByGroup(elfe, group = "group", raw = "raw"), "data.frame")
  expect_is(rankBySlidingWindow(ppvt, age = ppvt$age, width=1, raw=ppvt$raw), "data.frame")
  expect_is(cnorm(raw = elfe$raw, group = elfe$group), "cnorm")
})

test_that("conventional norming works", {
  skip_on_cran()
  expect_output(cnorm(raw = elfe$raw))
})

test_that("functions handle edge cases correctly", {
  skip_on_cran()
  expect_error(rankByGroup(elfe, group = "nonexistent_column", raw = "raw"))
  expect_error(cnorm(raw = numeric(0), group = character(0)))
  expect_error(plot(cnorm.elfe, "invalid_plot_type"))
})


context("Beta Binomial Regression Functions")
test_that("cnorm.betabinomial works correctly", {
  skip_on_cran()
  model <- cnorm.betabinomial(age, score, n = n, plot = FALSE)

  expect_s3_class(model, "cnormBetaBinomial2")
  expect_length(model$alpha_est, model$alpha_degree + 1)
  expect_length(model$beta_est, model$beta_degree + 1)
  expect_true(all(!is.na(model$se)))

  # Test with weights
  model_weighted <- cnorm.betabinomial(age, score, n = n, weights = weights, plot = FALSE)
  expect_s3_class(model_weighted, "cnormBetaBinomial2")
})

test_that("predict.cnormBetaBinomial works correctly", {
  skip_on_cran()
  model <- cnorm.betabinomial(age, score, n = n, plot = FALSE)

  new_ages <- c(7, 10, 13)
  new_scores <- c(25, 30, 35)

  predictions <- predict(model, new_ages, new_scores)

  expect_length(predictions, length(new_ages))
  expect_true(all(!is.na(predictions)))
})

test_that("plot.cnormBetaBinomial works correctly", {
  skip_on_cran()
  model <- cnorm.betabinomial(age, score, n = n, plot = FALSE)

  p <- plot(model, age, score)

  expect_s3_class(p, "ggplot")
})

test_that("summary.cnormBetaBinomial works correctly", {
  skip_on_cran()
  model <- cnorm.betabinomial(age, score, n = n, plot = FALSE)

  summary_output <- capture.output(summary(model))

  expect_true(any(grepl("Beta-Binomial Continuous Norming Model", summary_output)))
  expect_true(any(grepl("Model Fit:", summary_output)))
  expect_true(any(grepl("Convergence:", summary_output)))
  expect_true(any(grepl("Parameter Estimates:", summary_output)))
})

test_that("diagnostics.betabinomial works correctly", {
  skip_on_cran()
  model <- cnorm.betabinomial(age, score, n = n, plot = FALSE)

  diag <- diagnostics.betabinomial(model, age, score)

  expect_type(diag, "list")
  expect_true("AIC" %in% names(diag))
  expect_true("BIC" %in% names(diag))
  expect_true("R2" %in% names(diag))
})

test_that("normTable.betabinomial works correctly", {
  skip_on_cran()
  model <- cnorm.betabinomial(age, score, n = n, plot = FALSE)

  norm_table <- normTable.betabinomial(model, c(7, 10, 13))

  expect_type(norm_table, "list")
  expect_length(norm_table, 3)
  expect_true(all(sapply(norm_table, is.data.frame)))
})

test_that("betaCoefficients works correctly", {
  skip_on_cran()
  coef <- betaCoefficients(score, n = n)

  expect_length(coef, 5)
  expect_true(all(!is.na(coef)))
})
