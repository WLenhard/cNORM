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
#    Thoroughly tests beta binomial modelling
# =============================================================================

# ---- Helper: simulate beta-binomial data with age trend -------------------
simulate_bb <- function(N = 800, n_items = 40, seed = 42) {
  set.seed(seed)
  age <- runif(N, 6, 12)
  # increasing ability with age, moderate overdispersion
  a <- exp(0.2 + 0.25 * (age - 9))
  b <- exp(1.0 - 0.15 * (age - 9))
  p <- rbeta(N, a, b)
  score <- rbinom(N, size = n_items, prob = p)
  list(age = age, score = score, n = n_items)
}

sim <- simulate_bb()

# Fit once, reuse across tests (mode 2 = default path)
model <- suppressMessages(
  cnorm.betabinomial(sim$age, sim$score, n = sim$n, plot = FALSE)
)


test_that("cnorm.betabinomial (mode 2) fits and converges on simulated data", {
  expect_s3_class(model, "cnormBetaBinomial2")
  expect_equal(model$result$convergence, 0)
  expect_equal(attr(model$result, "max"), sim$n)
  expect_equal(attr(model$result, "N"), length(sim$score))

  d <- diagnostics.betabinomial(model)
  expect_true(d$converged)
  expect_true(is.finite(d$BIC))
})


test_that("log_likelihood2 with precomputed lchoose is identical", {
  X <- bb_design_matrix(standardize(sim$age), 3)
  Z <- bb_design_matrix(standardize(sim$age), 3)
  params <- c(model$alpha_est, model$beta_est)
  lch <- lchoose(sim$n, sim$score)

  expect_equal(
    log_likelihood2(params, X, Z, sim$score, sim$n),
    log_likelihood2(params, X, Z, sim$score, sim$n, lch = lch)
  )
})


test_that("bb_distribution is a proper mid-p distribution", {
  dist <- bb_distribution(a = 2.5, b = 4.0, n = 30)
  expect_equal(sum(dist$Px), 1, tolerance = 1e-12)
  expect_equal(dist$cum[31], 1)
  expect_true(all(diff(dist$perc) > 0))          # strictly increasing
  expect_true(all(dist$perc > 0 & dist$perc < 1))

  # invalid parameters yield NA, not errors
  bad <- bb_distribution(a = -1, b = 2, n = 30)
  expect_true(all(is.na(bad$Px)))
})


test_that("normTable.betabinomial: truncation via m does not renormalize", {
  ages <- c(7, 10)
  full  <- normTable.betabinomial(model, ages, CI = NULL)
  trunc <- normTable.betabinomial(model, ages, m = 20, CI = NULL)

  # truncated table must be an exact head of the full table (bugfix check)
  expect_equal(nrow(trunc[[1]]), 21)
  expect_equal(trunc[[1]]$Px,         full[[1]]$Px[1:21])
  expect_equal(trunc[[1]]$Pcum,       full[[1]]$Pcum[1:21])
  expect_equal(trunc[[1]]$Percentile, full[[1]]$Percentile[1:21])
})


test_that("predict is consistent with normTable and unique-age grouping", {
  ages  <- rep(c(7, 10), each = 3)
  raws  <- c(10, 20, 30, 10, 20, 30)
  preds <- predict(model, ages, raws)

  # cross-check against normTable norm scores
  tab <- normTable.betabinomial(model, c(7, 10), CI = NULL)
  expect_equal(preds[1:3], tab[["7"]]$norm[raws[1:3] + 1],  tolerance = 1e-10)
  expect_equal(preds[4:6], tab[["10"]]$norm[raws[4:6] + 1], tolerance = 1e-10)

  # monotonicity in raw score within age
  expect_true(all(diff(preds[1:3]) > 0))

  # out-of-range and non-integer scores yield NA with warning
  expect_warning(p2 <- predict(model, c(8, 8), c(sim$n + 5, 12.5)))
  expect_true(all(is.na(p2)))
})


test_that("mode 1 (cnorm.betabinomial1) remains functional (compatibility)", {
  m1 <- suppressMessages(
    cnorm.betabinomial(sim$age, sim$score, n = sim$n, mode = 1, plot = FALSE)
  )
  expect_s3_class(m1, "cnormBetaBinomial")

  p <- predict(m1, c(7, 9, 11), c(15, 20, 25))
  expect_true(all(is.finite(p)))

  # mean-preserving fallback in predictCoefficients: mu/(a+b) relation holds
  pc <- predictCoefficients(m1, c(7, 9, 11))
  expect_true(all(pc$a > 0 & pc$b > 0))
  expect_equal(sim$n * pc$a / (pc$a + pc$b), pc$mu, tolerance = 0.05)
})


test_that("betaCoefficients recovers parameters approximately", {
  set.seed(1)
  x <- rbinom(50000, 25, rbeta(50000, 3, 5))
  cf <- betaCoefficients(x, 25)
  expect_equal(cf[1], 3, tolerance = 0.25)
  expect_equal(cf[2], 5, tolerance = 0.35)
})

test_that("analytic beta-binomial gradient matches numerical gradient", {
  set.seed(1)
  n <- 60
  age <- runif(400, 6, 12); a_std <- (age - mean(age))/sd(age)
  y <- rbinom(400, n, rbeta(400, 4, 3))
  X <- cNORM:::bb_design_matrix(a_std, 3)
  Z <- cNORM:::bb_design_matrix(a_std, 3)
  p0 <- c(log(4), 0.2, -0.1, 0.05, log(3), -0.15, 0.1, 0)
  g_ana <- cNORM:::gradient_log_likelihood2(p0, X, Z, y, n)
  g_num <- numDeriv::grad(cNORM:::log_likelihood2, p0, X = X, Z = Z, y = y, n = n)
  expect_equal(g_ana, g_num, tolerance = 1e-6)
})



test_that("plot and summary run without error", {
  expect_s3_class(
    plot(model, age = sim$age, score = sim$score),
    "ggplot"
  )
  out <- capture.output(
    d <- summary(model, age = sim$age, score = sim$score)
  )
  expect_true(d$R2 > 0.9)   # norm score recovery on well-behaved data
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

# =============================================================================
# 8. WEIGHTED MODELLING
# =============================================================================

test_that("bestModel works with case weights", {
  data <- prepareData(elfe)
  w <- runif(nrow(data), 0.5, 2)
  m <- bestModel(data, weights = w, plot = FALSE)
  expect_s3_class(m, "cnormModel")
  expect_false(anyNA(m$coefficients))

  # weights = FALSE must ignore existing weights without error
  m2 <- bestModel(data, weights = FALSE, plot = FALSE)
  expect_s3_class(m2, "cnormModel")
})
