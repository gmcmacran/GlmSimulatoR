context("simulate_binomial")
library(GlmSimulatoR)
set.seed(1)

###############################################
# Run code
###############################################
default <- simulate_binomial()
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
  expect_equal(nrow(simulate_binomial(N = 10)), 10)
  expect_equal(nrow(simulate_binomial(N = 100)), 100)
  expect_equal(nrow(simulate_binomial(N = 1000)), 1000)
  expect_equal(nrow(simulate_binomial(N = 10000)), 10000)
})

test_that("Returns the correct number of predictors.", {
  expect_equal(ncol(simulate_binomial(weights = 1)), 2)
  expect_equal(ncol(simulate_binomial(weights = 1:2)), 3)
  expect_equal(ncol(simulate_binomial(weights = 1:3)), 4)
  expect_equal(ncol(simulate_binomial(weights = 1:4)), 5)
})

test_that("Returns the correct number of unrelated variables.", {
  expect_equal(ncol(simulate_binomial(weights = 1, unrelated = 0)), 2)
  expect_equal(ncol(simulate_binomial(weights = 1, unrelated = 1)), 3)
  expect_equal(ncol(simulate_binomial(weights = 1, unrelated = 2)), 4)
  expect_equal(ncol(simulate_binomial(weights = 1, unrelated = 3)), 5)

  expect_equal(ncol(simulate_binomial(weights = 1:2, unrelated = 0)), 3)
  expect_equal(ncol(simulate_binomial(weights = 1:2, unrelated = 1)), 4)
  expect_equal(ncol(simulate_binomial(weights = 1:2, unrelated = 2)), 5)
  expect_equal(ncol(simulate_binomial(weights = 1:2, unrelated = 3)), 6)
})

test_that("All links execute", {
  expect_true(all(class(simulate_binomial(link = "logit")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "probit")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "cauchit")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "log", weights = -.01)) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "cloglog", weights = .1)) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "loglog", weights = .1)) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "logc", weights = .1)) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_binomial(link = "identity", weights = .1)) == c("tbl_df", "tbl", "data.frame")))
})

test_that("Ancillary parameter function works if ancillary is provided", {
  expect_true(simulate_binomial(ancillary = 10) %>% nrow() > 0)
})

###############################################
# Input checking
###############################################
test_that("Confirm input checing works.", {
  expect_error(simulate_binomial(N = -1), NULL)
  expect_error(simulate_binomial(N = c(100, 200)), NULL)
  expect_error(simulate_binomial(link = "sqrt"), NULL)
  expect_error(simulate_binomial(weights = c()), NULL)
  expect_error(simulate_binomial(unrelated = -1), NULL)
  expect_error(simulate_binomial(unrelated = c(10, 20)), NULL)
  expect_error(simulate_binomial(ancillary = -1), NULL)
  expect_error(simulate_binomial(ancillary = c(10, 20)), NULL)
})
