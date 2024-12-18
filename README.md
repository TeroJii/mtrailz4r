
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mtrailz4r

Consider “mtrailz4r” as a working title for now.

<!-- badges: start -->

[![R-CMD-check](https://github.com/TeroJii/mtrailz4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TeroJii/mtrailz4r/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/TeroJii/mtrailz4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/TeroJii/mtrailz4r?branch=main)
<!-- badges: end -->

The goal of mtrailz4r is to …

## TO-DO

- [x] Add a function to unnest json data
  - [x] add tests with test data
- [ ] Consider adding a function for user_properties
- [x] Function for building session_id
  - [x] add tests with test data
- [x] Function for fixing timestamp
  - [x] add tests with test data
- [ ] Other functions for visualization
- [x] Add mock data to the package
- [x] Add a script to prepare mock data
  - [x] check that mock data is ok
  - [ ] add tests with test data
- [ ] Bump up version number
- [x] Add CI/CD to automate R CMD check

## Installation

You can install the development version of mtrailz4r from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TeroJii/mtrailz4r")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mtrailz4r)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
