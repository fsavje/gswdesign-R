library(gswdesign)
context("simple tests")

gswdesign_setup()

set.seed(123456789)

test_covs <- matrix(runif(1000), nrow = 100)

set_julia_seed(12345L)

assignments1 <- sample_gs_walk(test_covs, 0.5)

assignments2 <- sample_gs_walk(test_covs, 0.5)

set_julia_seed(12345L)

assignments3 <- sample_gs_walk(test_covs, 0.5)

test_that("test calls to sample_gs_walk and set seed", {
  expect_false(isTRUE(all.equal(assignments1, assignments2)))
  expect_identical(assignments1, assignments3)
})

assignments4 <- sample_gs_walk(test_covs, lambda = 0.3)
assignments5 <- sample_gs_walk(test_covs, lambda = 0.8)

assignments6 <- sample_gs_walk(test_covs, 0.5, balanced = TRUE)
assignments7 <- sample_gs_walk(test_covs, lambda = 0.3, balanced = TRUE)
assignments8 <- sample_gs_walk(test_covs, lambda = 0.8, balanced = TRUE)

test_that("test so balanced", {
  expect_identical(sum(assignments6), 50L)
  expect_identical(sum(assignments7), 50L)
  expect_identical(sum(assignments8), 50L)
})

assignments9 <- sample_gs_walk(test_covs, 0.5, num_samples = 10L)
assignments10 <- sample_gs_walk(test_covs, lambda = 0.3, num_samples = 10L)
assignments11 <- sample_gs_walk(test_covs, lambda = 0.8, num_samples = 10L)

assignments12 <- sample_gs_walk(test_covs, 0.5, balanced = TRUE, num_samples = 10L)
assignments13 <- sample_gs_walk(test_covs, lambda = 0.3, balanced = TRUE, num_samples = 10L)
assignments14 <- sample_gs_walk(test_covs, lambda = 0.8, balanced = TRUE, num_samples = 10L)
