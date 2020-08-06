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

assignments4 <- sample_gs_walk(test_covs, phi = 0.3)
assignments5 <- sample_gs_walk(test_covs, phi = 0.8)

assignments6 <- sample_gs_walk(test_covs, 0.5, balanced = TRUE)
assignments7 <- sample_gs_walk(test_covs, phi = 0.3, balanced = TRUE)
assignments8 <- sample_gs_walk(test_covs, phi = 0.8, balanced = TRUE)

test_that("test so balanced", {
  expect_identical(sum(assignments6), 50L)
  expect_identical(sum(assignments7), 50L)
  expect_identical(sum(assignments8), 50L)
})

assignments9 <- sample_many_gs_walk(test_covs, 0.5, 10L)
assignments10 <- sample_many_gs_walk(test_covs, phi = 0.3, 10L)
assignments11 <- sample_many_gs_walk(test_covs, phi = 0.8, 10L)

assignments12 <- sample_many_gs_walk(test_covs, 0.5, 10L, balanced = TRUE)
assignments13 <- sample_many_gs_walk(test_covs, phi = 0.3, 10L, balanced = TRUE)
assignments14 <- sample_many_gs_walk(test_covs, phi = 0.8, 10L, balanced = TRUE)

test_that("test so balanced", {
  expect_true(all(sapply(assignments12, sum) == 50L))
  expect_true(all(sapply(assignments13, sum) == 50L))
  expect_true(all(sapply(assignments14, sum) == 50L))
})

unequal_probs <- rep(c(0.9, 0.1), each = 50)

zdiff <- function(z) sum(z[1:50]) - sum(z[51:100])

assignments15 <- sample_many_gs_walk(test_covs, 0.5, 1000L, treatment_probs = unequal_probs)
assignments16 <- sample_many_gs_walk(test_covs, 0.3, 1000L, treatment_probs = unequal_probs)
assignments17 <- sample_many_gs_walk(test_covs, 0.8, 1000L, treatment_probs = unequal_probs)

# 0.9 * 50 - 0.1 * 50 = 45 - 5 = 40
test_that("test uneq probs", {
  expect_gte(mean(sapply(assignments15, zdiff)), 39)
  expect_lte(mean(sapply(assignments15, zdiff)), 41)
  expect_gte(mean(sapply(assignments16, zdiff)), 39)
  expect_lte(mean(sapply(assignments16, zdiff)), 41)
  expect_gte(mean(sapply(assignments17, zdiff)), 39)
  expect_lte(mean(sapply(assignments17, zdiff)), 41)
})

assignments18 <- sample_many_gs_walk(test_covs, 0.5, 1000L, balanced = TRUE, treatment_probs = unequal_probs)
assignments19 <- sample_many_gs_walk(test_covs, 0.3, 1000L, balanced = TRUE, treatment_probs = unequal_probs)
assignments20 <- sample_many_gs_walk(test_covs, 0.8, 1000L, balanced = TRUE, treatment_probs = unequal_probs)

test_that("test uneq probs", {
  expect_gte(mean(sapply(assignments18, zdiff)), 39)
  expect_lte(mean(sapply(assignments18, zdiff)), 41)
  expect_gte(mean(sapply(assignments19, zdiff)), 39)
  expect_lte(mean(sapply(assignments19, zdiff)), 41)
  expect_gte(mean(sapply(assignments20, zdiff)), 39)
  expect_lte(mean(sapply(assignments20, zdiff)), 41)
})

test_that("test so balanced", {
  expect_true(all(sapply(assignments18, sum) == 50L))
  expect_true(all(sapply(assignments19, sum) == 50L))
  expect_true(all(sapply(assignments20, sum) == 50L))
})
