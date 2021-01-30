context("Modelling")



test_that("ranking produces no error", {
  skip_on_cran()
  expect_warning(rankByGroup(elfe, group = "group", raw = "raw"), NA)
  expect_warning(rankBySlidingWindow(ppvt, age = ppvt$age, width=1, raw=ppvt$raw), NA)
  expect_warning(rankByGroup(ppvt, raw=ppvt$raw, weights = ppvt$sex), NA)
})

test_that("comprehensive function works", {
  skip_on_cran()
  expect_warning(cnorm(raw = elfe$raw, group = elfe$group), NA)
  expect_error(cnorm(raw = ppvt$raw, group = ppvt$group, weight = ppvt$sex), NA)
  expect_warning(cnorm(raw = ppvt$raw, age = ppvt$age, width=1), NA)
})

test_that("plotting works", {
  cnorm.elfe <- cnorm(raw = elfe$raw, group = elfe$group)
  expect_warning(plot(cnorm.elfe, "raw"), NA)
  expect_warning(plot(cnorm.elfe, "percentiles"), NA)
  expect_warning(plot(cnorm.elfe, "derivative"), NA)
  expect_warning(plot(cnorm.elfe, "curves"), NA)
  expect_warning(plot(cnorm.elfe, "subset"), NA)
})
