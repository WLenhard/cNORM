# =============================================================================
# cNORM — Comprehensive test suite (Updated for testthat 3e)
# =============================================================================
library(testthat)

# Use 3rd edition to match modern devtools standards
local_edition(3)

# ---------------------------------------------------------------------------
# 0. Shared test fixtures
# ---------------------------------------------------------------------------
set.seed(42)

.age     <- ppvt$age
.score   <- ppvt$raw
.n_items <- 228

.marginals <- data.frame(
  var   = c("sex",  "sex",  "migration", "migration"),
  level = c(1,      2,      0,           1),
  prop  = c(0.51,   0.49,   0.65,        0.35)
)

.elfe_cnorm  <- NULL
.ppvt_cnorm  <- NULL
.bb_model    <- NULL
.shash_model <- NULL

get_elfe <- function() {
  if (is.null(.elfe_cnorm)) {
    suppressMessages({
      .elfe_cnorm <<- cnorm(raw = elfe$raw, group = elfe$group, plot = FALSE)
    })
  }
  .elfe_cnorm
}

get_ppvt <- function() {
  if (is.null(.ppvt_cnorm)) {
    suppressMessages({
      .ppvt_cnorm <<- cnorm(raw = ppvt$raw, group = ppvt$group, plot = FALSE)
    })
  }
  .ppvt_cnorm
}

get_bb <- function() {
  if (is.null(.bb_model)) {
    suppressMessages({
      .bb_model <<- cnorm.betabinomial(.age, .score, n = .n_items, plot = FALSE)
    })
  }
  .bb_model
}

get_shash <- function() {
  if (is.null(.shash_model)) {
    suppressMessages({
      .shash_model <<- cnorm.shash(.age, .score, plot = FALSE)
    })
  }
  .shash_model
}

# =============================================================================
# 1. DATA PREPARATION
# =============================================================================

test_that("rankByGroup returns data.frame with expected columns", {
  skip_on_cran()
  suppressMessages({
    d <- rankByGroup(elfe, group = "group", raw = "raw")
  })
  expect_s3_class(d, "data.frame")
  expect_true(all(c("percentile", "normValue", "n", "m", "md", "sd") %in% colnames(d)))
})

test_that("rankByGroup percentiles lie strictly in (0, 1)", {
  skip_on_cran()
  suppressMessages({
    d <- rankByGroup(elfe, group = "group", raw = "raw")
  })
  expect_true(all(d$percentile > 0 & d$percentile < 1))
})

test_that("rankByGroup descend = TRUE inverts the ranking direction", {
  skip_on_cran()
  d_asc  <- suppressMessages(rankByGroup(elfe, group = "group", raw = "raw", descend = FALSE))
  d_desc <- suppressMessages(rankByGroup(elfe, group = "group", raw = "raw", descend = TRUE))
  expect_true(cor(d_asc$normValue, d_desc$normValue) < 0)
})

test_that("rankBySlidingWindow returns data.frame with expected columns", {
  skip_on_cran()
  d <- rankBySlidingWindow(ppvt, age = "age", raw = "raw", width = 1)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("percentile", "normValue") %in% colnames(d)))
})

test_that("computePowers adds L and A columns", {
  skip_on_cran()
  d <- suppressMessages(rankByGroup(elfe, group = "group", raw = "raw"))
  d <- computePowers(d, k = 4, t = 3)
  expect_true("L4" %in% colnames(d))
  expect_true("A3" %in% colnames(d))
})

test_that("getGroups returns numeric vector of same length", {
  skip_on_cran()
  x <- rnorm(300, 50, 10)
  g <- getGroups(x, n = 5)
  expect_length(g, 300)
})

# =============================================================================
# 2. WEIGHTING AND RAKING
# =============================================================================

test_that("computeWeights returns a positive numeric vector", {
  skip_on_cran()
  w <- suppressMessages(computeWeights(ppvt, .marginals))
  expect_length(w, nrow(ppvt))
  expect_true(all(w > 0))
})

# =============================================================================
# 3. TAYLOR-POLYNOMIAL MODELLING
# =============================================================================

test_that("cnorm returns an object of class 'cnorm'", {
  skip_on_cran()
  expect_s3_class(get_elfe(), "cnorm")
})

test_that("regressionFunction returns a valid string", {
  skip_on_cran()
  f <- regressionFunction(get_elfe())
  expect_true(grepl("raw", f))
})

test_that("checkConsistency returns a logical value", {
  skip_on_cran()
  result <- checkConsistency(get_elfe(), minNorm = 25, maxNorm = 75, silent = TRUE)
  expect_true(is.logical(result))
})

# =============================================================================
# 4. PREDICTION
# =============================================================================

test_that("predictRaw respects clipping bounds", {
  skip_on_cran()
  m <- get_elfe()
  r_high <- predictRaw(95, 3, m, minRaw = 0, maxRaw = 28)
  expect_lte(r_high, 28)
})

test_that("normTable returns data.frame", {
  skip_on_cran()
  tab <- normTable(3, get_elfe(), minNorm = 25, maxNorm = 75)
  expect_s3_class(tab, "data.frame")
})

# =============================================================================
# 5. PARAMETRIC MODELS (Beta-Binomial & ShaSh)
# =============================================================================

test_that("cnorm.betabinomial returns correct class", {
  skip_on_cran()
  expect_s3_class(get_bb(), "cnormBetaBinomial2")
})

test_that("cnorm.shash returns correct class", {
  skip_on_cran()
  expect_s3_class(get_shash(), "cnormShash")
})

test_that("predict.cnormShash returns finite scores", {
  skip_on_cran()
  pred <- predict(get_shash(), age = c(7, 10), score = c(100, 130))
  expect_length(pred, 2)
  expect_true(all(is.finite(pred)))
})

# =============================================================================
# 6. PLOTTING & COMPARISON
# =============================================================================

test_that("plotPercentiles returns a ggplot", {
  skip_on_cran()
  p <- plotPercentiles(get_elfe())
  expect_s3_class(p, "ggplot")
})

test_that("compare() returns ggplot", {
  skip_on_cran()
  suppressMessages({
    p <- compare(get_ppvt(), get_bb(), age = .age, score = .score)
  })
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# 7. ERROR HANDLING
# =============================================================================

test_that("rankByGroup stops on missing columns", {
  skip_on_cran()
  expect_error(rankByGroup(elfe, group = "missing", raw = "raw"))
})

test_that("predict.cnormShash stops when score is missing", {
  skip_on_cran()
  expect_error(predict(get_shash(), age = c(7, 10)))
})
