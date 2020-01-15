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
#' @param lambda
#'    a real value in (0,1) specifying weight
#' @param balanced
#'    set true to run the balanced GSW
#' @param num_samples
#'    number of treatment assignments to sample
#'
#' @examples
#' \dontrun{
#' gswdesign_setup()
#' X <- matrix(rnorm(1000), nrow = 100)
#' sample_gsw(X, lambda = 0.3)
#' }
#'
#' @export
sample_gsw <- function(X, lambda = 0.5, balanced = FALSE, num_samples = 1L) {

  gsw_arg <- list(
    X,
    "alpha" = lambda,
    "balanced" = balanced,
    "num_samples" = num_samples
  )

  .gsw_intenv$julia$do.call("sample_gs_walk", gsw_arg, need_return = "R")
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
  .gsw_intenv$julia$call("Random.seed!", seed, need_return = "None")
}
