context("simulate_tweedie")
library(GlmSimulatoR)
set.seed(1)

###############################################
# Run code
###############################################
default <- simulate_tweedie()
test_that("Run default. Check structure.", {
  expect_true(all(class(default) == c("tbl_df", "tbl", "data.frame")))
  expect_true(nrow(default) == 10000)
  expect_true(all(colnames(default) == c("Y", "X1")))
  expect_true(min(default$X1) >= 1)
  expect_true(max(default$X1) <= 2)
})
rm(default)

test_that("Returns the correct number of rows.", {
  expect_equal(nrow(simulate_tweedie(N = 10)), 10)
  expect_equal(nrow(simulate_tweedie(N = 100)), 100)
  expect_equal(nrow(simulate_tweedie(N = 1000)), 1000)
  expect_equal(nrow(simulate_tweedie(N = 10000)), 10000)
})

test_that("Returns the correct number of predictors.", {
  expect_equal(ncol(simulate_tweedie(weights = .5)), 2)
  expect_equal(ncol(simulate_tweedie(weights = c(.1, .2))), 3)
  expect_equal(ncol(simulate_tweedie(weights = c(.1, .2, .3))), 4)
})

test_that("Returns the correct number of unrelated variables.", {
  expect_equal(ncol(simulate_tweedie(weights = 1, unrelated = 0)), 2)
  expect_equal(ncol(simulate_tweedie(weights = 1, unrelated = 1)), 3)
  expect_equal(ncol(simulate_tweedie(weights = 1, unrelated = 2)), 4)
  expect_equal(ncol(simulate_tweedie(weights = 1, unrelated = 3)), 5)

  expect_equal(ncol(simulate_tweedie(weights = 1:2, unrelated = 0)), 3)
  expect_equal(ncol(simulate_tweedie(weights = 1:2, unrelated = 1)), 4)
  expect_equal(ncol(simulate_tweedie(weights = 1:2, unrelated = 2)), 5)
  expect_equal(ncol(simulate_tweedie(weights = 1:2, unrelated = 3)), 6)
})

test_that("All links execute", {
  expect_true(all(class(simulate_tweedie(link = "log")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_tweedie(link = "identity")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_tweedie(link = "sqrt")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_tweedie(link = "inverse")) == c("tbl_df", "tbl", "data.frame")))
})

test_that("Ancillary parameter function works if ancillary is provided", {
  expect_true(simulate_tweedie(ancillary = 1) %>% nrow() > 0)
  expect_true(simulate_tweedie(ancillary = 1.5) %>% nrow() > 0)
  expect_true(simulate_tweedie(ancillary = 2) %>% nrow() > 0)
})

###############################################
# Input checking
###############################################
test_that("Confirm input checing works.", {
  expect_error(simulate_tweedie(N = -1), NULL)
  expect_error(simulate_tweedie(N = c(100, 200)), NULL)
  expect_error(simulate_tweedie(link = "1/mu^2"), NULL)
  expect_error(simulate_tweedie(weights = c()), NULL)
  expect_error(simulate_tweedie(unrelated = -1), NULL)
  expect_error(simulate_tweedie(unrelated = c(10, 20)), NULL)
  expect_error(simulate_tweedie(ancillary = .9), NULL)
  expect_error(simulate_tweedie(ancillary = 2.1), NULL)
  expect_error(simulate_tweedie(ancillary = c(1, 2)), NULL)
})
