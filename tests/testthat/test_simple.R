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

assignments4 <- sample_gsw(test_covs, lambda = 0.3)
assignments5 <- sample_gsw(test_covs, lambda = 0.8)

assignments6 <- sample_gsw(test_covs, balanced = TRUE)
assignments7 <- sample_gsw(test_covs, lambda = 0.3, balanced = TRUE)
assignments8 <- sample_gsw(test_covs, lambda = 0.8, balanced = TRUE)

test_that("test so balanced", {
  expect_identical(sum(assignments6), 50L)
  expect_identical(sum(assignments7), 50L)
  expect_identical(sum(assignments8), 50L)
})

#assignments3 <- sample_gsw(test_covs, num_samples = 10L)
#assignments4 <- sample_gsw(test_covs, lambda = 0.3)
#assignments5 <- sample_gsw(test_covs, lambda = 0.8)

#assignments6 <- sample_gsw(test_covs, balanced = TRUE)
#assignments7 <- sample_gsw(test_covs, lambda = 0.3, balanced = TRUE)
#assignments8 <- sample_gsw(test_covs, lambda = 0.8, balanced = TRUE)
