# ==============================================================================
# gswdesign -- Gram-Schmidt walk design
# https://github.com/fsavje/gswdesign-R
#
# Copyright (C) 2020
# Christopher Harshaw, Fredrik Savje, Daniel Spielman & Peng Zhang
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/
# ==============================================================================

#' Sample from the Gram-Schmidt Walk design.
#'
#' An fast implementation for sampling from the Gram-Schmidt Walk Design.
#' Maintains a cholesky factorization of (I + A * A^T ) for faster repeated linear
#' system solves and has a recursive component for more effective memory allocation.
#'
#' @param X
#'    a n-by-x matrix with covariates to balance
#' @param phi
#'    a real value in (0,1) specifying the balancing weight
#' @param balanced
#'    set true to run the balanced version of the GSW design
#' @param treatment_probs
#'    a vector of marginal assignment probabilities. If scalar, the probabilities
#'    for all units are set to the provided value.
#' @param num_samples
#'    number of treatment assignments to sample.
#'
#' @return
#'    \code{sample_gs_walk} returns a single logical vector containing the assignments.
#'    \code{sample_many_gs_walk} returns a list of \code{num_samples} such vectors.
#'
#' @examples
#' \dontrun{
#' gswdesign_setup()
#' set_julia_seed(123456789L)
#' X <- matrix(rnorm(1000), nrow = 100)
#' assignment <- sample_gs_walk(X, 0.3)
#' assignments <- sample_many_gs_walk(X, 0.3, 1000L)
#' }
#'
#' @export
sample_gs_walk <- function(X, phi, balanced = FALSE, treatment_probs = 0.5) {
  sample_many_gs_walk(X, phi, 1L, balanced, treatment_probs)[[1]]
}


#' @rdname sample_gs_walk
#' @export
sample_many_gs_walk <- function(X, phi, num_samples, balanced = FALSE, treatment_probs = 0.5) {
  if (!julia_running())
    stop("Julia is not running. Please call 'gswdesign_setup()'.")

  if (is.vector(X))
    X <- matrix(X, nrow = length(X))
  if (is.numeric(X) && !is.double(X))
    storage.mode(X) <- "double"

  X <- unname(X)

  stopifnot(
    is.matrix(X),
    is.double(X),
    nrow(X) >= 2L,
    !any(is.na(X))
  )

  if (is.numeric(phi) && !is.double(phi))
    storage.mode(phi) <- "double"

  stopifnot(
    is.vector(phi),
    is.double(phi),
    length(phi) == 1L,
    !is.na(phi),
    phi > 0.0,
    phi < 1.0
  )

  stopifnot(
    is.vector(balanced),
    is.logical(balanced),
    length(balanced) == 1L,
    !is.na(balanced)
  )

  if (is.numeric(treatment_probs) && !is.double(treatment_probs))
    storage.mode(treatment_probs) <- "double"

  if (length(treatment_probs) == 1L)
    treatment_probs <- rep.int(treatment_probs, nrow(X))

  stopifnot(
    is.vector(treatment_probs),
    is.double(treatment_probs),
    length(treatment_probs) == nrow(X),
    !any(is.na(treatment_probs)),
    all(treatment_probs > 0.0),
    all(treatment_probs < 1.0)
  )

  if (is.numeric(num_samples) && !is.integer(num_samples))
    num_samples <- as.integer(num_samples)

  stopifnot(
    is.vector(num_samples),
    is.integer(num_samples),
    length(num_samples) == 1L,
    !is.na(num_samples),
    num_samples >= 1L
  )

  gsw_arg <- list(
    X,
    phi,
    num_samples,
    "balanced" = balanced,
    "treatment_probs" = treatment_probs
  )

  as.list(.gsw_intenv$julia$do.call("sample_gs_walk", gsw_arg, need_return = "Julia"))
}


#' Set RNG seed in Julia
#'
#' Sets the seed for the random number generator in Julia.
#'
#' @param seed
#'    the seed to use
#'
#' @examples
#' \dontrun{
#' # Set Julia seed to "12345"
#' set_julia_seed(12345)
#' }
#'
#' @export
set_julia_seed <- function(seed) {
  if(!julia_running())
    stop("Julia is not running. Please call 'gswdesign_setup()'.")

  .gsw_intenv$julia$call("Random.seed!", seed, need_return = "None")
}
