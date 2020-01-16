# gswdesign-R

[![Build Status](https://travis-ci.org/fsavje/gswdesign-R.svg?branch=master)](https://travis-ci.org/fsavje/gswdesign-R)

The `gswdesign` R package wrappers for the Julia package [GSWDesign](https://github.com/crharshaw/GSWDesign.jl), which is a fast implementation of the Gram--Schmidt Walk for balancing covariates in randomized experiments. The Gram--Schmidt Walk design allows experimenters the flexibilty to control the amount of covariate balancing.


## How to install

First, make sure to have Julia version 1.0 or later [installed](https://julialang.org/downloads/) and [added to your path](https://en.wikibooks.org/wiki/Introducing_Julia/Getting_started). The easiest way to do so on macOS is to use [Homebrew](https://brew.sh/) and call:
```{sh}
brew cask install julia
```

Once Julia is installed, you can install `gswdesign` from Github using [devtools](https://github.com/hadley/devtools):
```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fsavje/distances")
```
