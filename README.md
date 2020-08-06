# gswdesign-R

[![Build Status](https://travis-ci.org/fsavje/gswdesign-R.svg?branch=master)](https://travis-ci.org/fsavje/gswdesign-R)

The `gswdesign` R package provides wrappers for the Julia package [GSWDesign](https://github.com/crharshaw/GSWDesign.jl), which is a fast implementation of the Gram-Schmidt Walk for balancing covariates in randomized experiments. The Gram-Schmidt Walk design allows experimenters to control the trade-off between the amount of covariate balance and robustness provided by treatment assignment. Refer to [Harshaw et al (2020) "Balancing covariates in randomized experiments using the Gram-Schmidt Walk"](https://arxiv.org/abs/1911.03071) for an indepth discussion.

## How to install

First, make sure to have Julia version 1.0 or later [installed](https://julialang.org/downloads/) and [added to your path](https://en.wikibooks.org/wiki/Introducing_Julia/Getting_started). The easiest way to do so on macOS is to use [Homebrew](https://brew.sh/) and call:
```{sh}
brew cask install julia
```

Once Julia is installed, you can install `gswdesign` from Github using [devtools](https://github.com/hadley/devtools):
```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fsavje/gswdesign-R")
```
