
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mtrailz4r

<!-- badges: start -->

[![R-CMD-check](https://github.com/TeroJii/mtrailz4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TeroJii/mtrailz4r/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/TeroJii/mtrailz4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/TeroJii/mtrailz4r?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![GitHub
Release](https://img.shields.io/github/v/release/TeroJii/mtrailz4r)
<!-- badges: end -->

The goal of mtrailz4r is to help with specific data processing steps…

## TO-DO

- [ ] Consider adding a function for extracting user_properties

## Installation

You can install the development version of mtrailz4r from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github(repo = "TeroJii/mtrailz4r")
```

Use the release tags to install a specific version. For example to
install version 0.0.9:

``` r
# install.packages("remotes")
remotes::install_github(repo = "TeroJii/mtrailz4r@v0.0.9")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mtrailz4r)
## basic example code
mockdata |> 
  mt4r_unnest() |> 
  mt4r_addsessionid() |>
  mt4r_fixtime() |> 
  plot_exertime()
```

<img src="man/figures/README-example-1.png" width="100%" />
