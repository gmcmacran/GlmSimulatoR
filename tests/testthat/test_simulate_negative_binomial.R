context("simulate_negative_binomial")
library(GlmSimulatoR)
set.seed(1)

###############################################
# Run code
###############################################
default <- simulate_negative_binomial()
test_that("Run default. Check structure.", {
  expect_true(all(class(default) == c("tbl_df", "tbl", "data.frame")))
  expect_true(nrow(default) == 10000)
  expect_true(all(colnames(default) == c("Y", "X1", "X2")))
  expect_true(min(default$X1) >= 1)
  expect_true(max(default$X1) <= 2)
  expect_true(min(default$X2) >= 1)
  expect_true(max(default$X2) <= 2)
})
rm(default)

test_that("Returns the correct number of rows.", {
  expect_equal(nrow(simulate_negative_binomial(N = 10)), 10)
  expect_equal(nrow(simulate_negative_binomial(N = 100)), 100)
  expect_equal(nrow(simulate_negative_binomial(N = 1000)), 1000)
  expect_equal(nrow(simulate_negative_binomial(N = 10000)), 10000)
})

test_that("Returns the correct number of predictors.", {
  expect_equal(ncol(simulate_negative_binomial(weights = 1)), 2)
  expect_equal(ncol(simulate_negative_binomial(weights = 1:2)), 3)
  expect_equal(ncol(simulate_negative_binomial(weights = 1:3)), 4)
})

test_that("Returns the correct range for x.", {
  expect_true(max(simulate_negative_binomial(weights = .5, xrange = 0)[, 2]) <= 1)
  expect_true(min(simulate_negative_binomial(weights = .5, xrange = 0)[, 2]) >= 1)
  expect_true(max(simulate_negative_binomial(weights = .5, xrange = 2)[, 2]) <= 3)
  expect_true(min(simulate_negative_binomial(weights = .5, xrange = 2)[, 2]) >= 1)
  expect_true(max(simulate_negative_binomial(weights = .5, xrange = 3)[, 2]) <= 4)
  expect_true(min(simulate_negative_binomial(weights = .5, xrange = 3)[, 2]) >= 1)
  expect_true(max(simulate_negative_binomial(weights = c(.5, 1), xrange = 0)[, 3]) <= 1)
  expect_true(min(simulate_negative_binomial(weights = c(.5, 1), xrange = 0)[, 3]) >= 1)
  expect_true(max(simulate_negative_binomial(weights = c(.5, 1), xrange = 2)[, 3]) <= 3)
  expect_true(min(simulate_negative_binomial(weights = c(.5, 1), xrange = 2)[, 3]) >= 1)
  expect_true(max(simulate_negative_binomial(weights = c(.5, 1), xrange = 3)[, 3]) <= 4)
  expect_true(min(simulate_negative_binomial(weights = c(.5, 1), xrange = 3)[, 3]) >= 1)
})

test_that("Returns the correct number of unrelated variables.", {
  expect_equal(ncol(simulate_negative_binomial(weights = 1, unrelated = 0)), 2)
  expect_equal(ncol(simulate_negative_binomial(weights = 1, unrelated = 1)), 3)
  expect_equal(ncol(simulate_negative_binomial(weights = 1, unrelated = 2)), 4)
  expect_equal(ncol(simulate_negative_binomial(weights = 1, unrelated = 3)), 5)

  expect_equal(ncol(simulate_negative_binomial(weights = 1:2, unrelated = 0)), 3)
  expect_equal(ncol(simulate_negative_binomial(weights = 1:2, unrelated = 1)), 4)
  expect_equal(ncol(simulate_negative_binomial(weights = 1:2, unrelated = 2)), 5)
  expect_equal(ncol(simulate_negative_binomial(weights = 1:2, unrelated = 3)), 6)
})

test_that("All links execute", {
  expect_true(all(class(simulate_negative_binomial(link = "log")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_negative_binomial(link = "identity")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_negative_binomial(link = "sqrt")) == c("tbl_df", "tbl", "data.frame")))
})

test_that("Ancillary parameter works as expected", {
  expect_true(simulate_negative_binomial()$Y %>% sd() > simulate_negative_binomial(ancillary = 5)$Y %>% sd())
})

###############################################
# Input checking
###############################################
test_that("Confirm input checing works.", {
  expect_error(simulate_negative_binomial(N = -1), NULL)
  expect_error(simulate_negative_binomial(N = c(100, 200)), NULL)
  expect_error(simulate_negative_binomial(link = "1/mu^2"), NULL)
  expect_error(simulate_negative_binomial(weights = c()), NULL)
  expect_error(simulate_negative_binomial(xrange = "asdf"), NULL)
  expect_error(simulate_negative_binomial(xrange = c()), NULL)
  expect_error(simulate_negative_binomial(xrange = c(1, 2)), NULL)
  expect_error(simulate_negative_binomial(xrange = -1), NULL)
  expect_error(simulate_negative_binomial(unrelated = -1), NULL)
  expect_error(simulate_negative_binomial(unrelated = c(10, 20)), NULL)
  expect_error(simulate_negative_binomial(ancillary = -1), NULL)
  expect_error(simulate_negative_binomial(ancillary = c(10, 20)), NULL)
})
