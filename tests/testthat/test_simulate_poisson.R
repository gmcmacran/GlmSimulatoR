library(GlmSimulatoR)
set.seed(1)

###############################################
# Run code
###############################################
default <- simulate_poisson()

model <- glm(formula = Y ~ X1 + X2, data = default, family = poisson())
params <- c(0.5, 1)
params <- c(max(params), params)

test_that("Run default. Check structure.", {
  expect_true(all(class(default) == c("tbl_df", "tbl", "data.frame")))
  expect_true(nrow(default) == 10000)
  expect_true(all(colnames(default) == c("Y", "X1", "X2")))
  expect_true(min(default$X1) >= 1)
  expect_true(max(default$X1) <= 2)
  expect_true(min(default$X2) >= 1)
  expect_true(max(default$X2) <= 2)
  expect_true(all(max(abs(model$coefficients - params)) <= .1))
})
rm(default, model, params)

test_that("Returns the correct number of rows.", {
  expect_equal(nrow(simulate_poisson(N = 10)), 10)
  expect_equal(nrow(simulate_poisson(N = 100)), 100)
  expect_equal(nrow(simulate_poisson(N = 1000)), 1000)
  expect_equal(nrow(simulate_poisson(N = 10000)), 10000)
})

test_that("Returns the correct number of predictors.", {
  expect_equal(ncol(simulate_poisson(weights = 1)), 2)
  expect_equal(ncol(simulate_poisson(weights = 1:2)), 3)
  expect_equal(ncol(simulate_poisson(weights = 1:3)), 4)
})

test_that("Returns the correct range for x.", {
  expect_true(max(simulate_poisson(weights = .5, x_range = 0)[, 2]) <= 1)
  expect_true(min(simulate_poisson(weights = .5, x_range = 0)[, 2]) >= 1)
  expect_true(max(simulate_poisson(weights = .5, x_range = 2)[, 2]) <= 3)
  expect_true(min(simulate_poisson(weights = .5, x_range = 2)[, 2]) >= 1)
  expect_true(max(simulate_poisson(weights = .5, x_range = 3)[, 2]) <= 4)
  expect_true(min(simulate_poisson(weights = .5, x_range = 3)[, 2]) >= 1)
  expect_true(max(simulate_poisson(weights = c(.5, 1), x_range = 0)[, 3]) <= 1)
  expect_true(min(simulate_poisson(weights = c(.5, 1), x_range = 0)[, 3]) >= 1)
  expect_true(max(simulate_poisson(weights = c(.5, 1), x_range = 2)[, 3]) <= 3)
  expect_true(min(simulate_poisson(weights = c(.5, 1), x_range = 2)[, 3]) >= 1)
  expect_true(max(simulate_poisson(weights = c(.5, 1), x_range = 3)[, 3]) <= 4)
  expect_true(min(simulate_poisson(weights = c(.5, 1), x_range = 3)[, 3]) >= 1)
})

test_that("Returns the correct number of unrelated variables.", {
  expect_equal(ncol(simulate_poisson(weights = 1, unrelated = 0)), 2)
  expect_equal(ncol(simulate_poisson(weights = 1, unrelated = 1)), 3)
  expect_equal(ncol(simulate_poisson(weights = 1, unrelated = 2)), 4)
  expect_equal(ncol(simulate_poisson(weights = 1, unrelated = 3)), 5)

  expect_equal(ncol(simulate_poisson(weights = 1:2, unrelated = 0)), 3)
  expect_equal(ncol(simulate_poisson(weights = 1:2, unrelated = 1)), 4)
  expect_equal(ncol(simulate_poisson(weights = 1:2, unrelated = 2)), 5)
  expect_equal(ncol(simulate_poisson(weights = 1:2, unrelated = 3)), 6)
})

test_that("All links execute", {
  expect_true(all(class(simulate_poisson(link = "log")) ==
    c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_poisson(link = "identity")) ==
    c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_poisson(link = "sqrt")) ==
    c("tbl_df", "tbl", "data.frame")))
})

###############################################
# Input checking
###############################################
test_that("Confirm input checing works.", {
  expect_error(simulate_poisson(N = -1), NULL)
  expect_error(simulate_poisson(N = c(100, 200)), NULL)
  expect_error(simulate_poisson(link = "1/mu^2"), NULL)
  expect_error(simulate_poisson(weights = c()), NULL)
  expect_error(simulate_poisson(x_range = "asdf"), NULL)
  expect_error(simulate_poisson(x_range = c()), NULL)
  expect_error(simulate_poisson(x_range = c(1, 2)), NULL)
  expect_error(simulate_poisson(x_range = -1), NULL)
  expect_error(simulate_poisson(unrelated = -1), NULL)
  expect_error(simulate_poisson(unrelated = c(10, 20)), NULL)
})
