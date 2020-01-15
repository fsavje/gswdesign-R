library(gswdesign)
context("simple tests")

gswdesign_setup()

set.seed(123456789)

test_covs <- matrix(runif(1000), nrow = 100)

set_julia_seed(12345L)

assignments1 <- sample_gsw(test_covs)

assignments2 <- sample_gsw(test_covs)

set_julia_seed(12345L)

assignments3 <- sample_gsw(test_covs)

test_that("test calls to sample_gsw and set seed", {
  expect_false(isTRUE(all.equal(assignments1, assignments2)))
  expect_identical(assignments1, assignments3)
})
