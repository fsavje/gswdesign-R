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

# Internal environment and const
.gsw_intenv <- new.env(parent = emptyenv())
.min_julia_version <- "1.0.5"
.min_GSWDesign_version <- "0.1.0"

#' gswdesign: The Gram-Schmidt walk design
#'
#' Provides wrappers for the Julia package 'GSWDesign', which is a fast
#' implementation of the Gram--Schmidt Walk for balancing covariates in
#' randomized experiments. The Gram--Schmidt Walk design allows experimenters
#' the flexibilty to control the amount of covariate balancing.
#'
#' See \code{\link{sample_gs_walk}} for the main function.
#'
#' See the packages' websites for more information:
#' \url{https://github.com/crharshaw/GSWDesign.jl},
#' \url{https://github.com/fsavje/gswdesign-R}.
#'
#' Bug reports and suggestions are greatly appreciated. They are best reported
#' here: \url{https://github.com/fsavje/gswdesign-R/issues}.
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

  if (utils::compareVersion(.gsw_intenv$julia$VERSION, .min_julia_version) == -1L) {
    .gsw_intenv <- new.env(parent = emptyenv())
    stop("Julia is out of date. Please update and rerun 'gswdesign_setup()'.")
  }

  .gsw_intenv$julia$install_package_if_needed("Random")
  .gsw_intenv$julia$install_package_if_needed("https://github.com/crharshaw/GSWDesign.jl")

  gsw_version <- .gsw_intenv$julia$installed_package("GSWDesign")

  if (gsw_version == "nothing")
    stop("Julia package 'GSWDesign' not found.")

  if (utils::compareVersion(gsw_version, .min_GSWDesign_version) == -1L)
    stop("Julia package 'GSWDesign' is out of date. Please update by running 'gswdesign_update()'.")

  .gsw_intenv$julia$library("Random")
  .gsw_intenv$julia$library("GSWDesign")
}


#' Update GSWDesign package in Julia
#'
#' \code{gswdesign_update} updates the GSWDesign Julia package.
#'
#' @examples
#' \dontrun{
#' gswdesign_setup()
#' gswdesign_update()
#' }
#'
#' @export
gswdesign_update <- function() {
  if(!julia_running())
    stop("Julia is not running. Please call 'gswdesign_setup()'.")

  .gsw_intenv$julia$update_package("https://github.com/crharshaw/GSWDesign.jl")

  gsw_version <- .gsw_intenv$julia$installed_package("GSWDesign")

  if (gsw_version == "nothing")
    stop("Julia package 'GSWDesign' not found.")

  if (utils::compareVersion(gsw_version, .min_GSWDesign_version) == -1L)
    stop("Julia package 'GSWDesign' is out of date. Please update by running 'gswdesign_update()'.")

  .gsw_intenv$julia$library("Random")
  .gsw_intenv$julia$library("GSWDesign")
}

#' Check so Julia is setup and running
#'
#' \code{julia_running} checks so Julia is running and can be called.
#'
#' @examples
#' \dontrun{
#' julia_running()
#' }
#'
julia_running <- function() {
  isTRUE(.gsw_intenv$julia$eval("true"))
}
