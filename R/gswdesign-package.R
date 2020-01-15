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

# Internal environment
.gsw_intenv <- new.env(parent = emptyenv())

#' gswdesign: The Gram-Schmidt walk design
#'
#' Provides wrappers for the julia package 'gswdesign', which is a
#' computationally efficient implementation of the Gram-Schmidt walk design.
#'
#' See \code{\link{sample_gsw}} for the main function.
#'
#' See the package's website for more information:
#' \url{https://github.com/xxx/xxx}.
#'
#' Bug reports and suggestions are greatly appreciated. They are best reported
#' here: \url{https://github.com/xxx/xxx/issues}.
#'
#' @references
#'    Harshaw, Christopher and Fredrik Sävje and Daniel Spielman and Peng Zhang (2019),
#'    \sQuote{Balancing covariates in randomized experiments using the Gram-Schmidt walk}, arXiv 1911.03071.
#'    \url{https://arxiv.org/abs/1911.03071}
#'
#' @docType package
#' @name gswdesign-package
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please remember to initialize 'gswdesign' with 'gswdesign_setup()' before calling its functions.")
}

.onUnload <- function (libpath) {
  .gsw_intenv <- new.env(parent = emptyenv())
}

#' Initial setup for the gswdesign package.
#'
#' \code{gswdesign_setup} does the initial setup for the gswdesign package.
#' It should be run before calling any of the functions in the package.
#'
#' @param ... arguments passed to \code{\link[JuliaCall]{julia_setup}}.
#'
#' @examples
#' \dontrun{
#' gswdesign_setup()
#' }
#'
#' @export
gswdesign_setup <- function(...) {
  .gsw_intenv$julia <- JuliaCall::julia_setup(...)
  .gsw_intenv$julia$install_package_if_needed("LinearAlgebra")
  .gsw_intenv$julia$install_package_if_needed("Random")
  .gsw_intenv$julia$library("LinearAlgebra")
  .gsw_intenv$julia$library("Random")
  .gsw_intenv$julia$source(system.file("julia/gsw-design.jl", package = "gswdesign"))
}
