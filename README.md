
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arkhe <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/arkhe/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/arkhe/actions)
[![codecov](https://codecov.io/gh/tesselle/arkhe/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tesselle/arkhe)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/arkhe/badge/main)](https://www.codefactor.io/repository/github/tesselle/arkhe/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/arkhe)](https://cran.r-project.org/package=arkhe)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/arkhe"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=arkhe"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/arkhe"
alt="CRAN Version" /></a>
<a href="https://cran.r-project.org/web/checks/check_results_arkhe.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/arkhe.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=arkhe"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/arkhe" alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3526659.svg)](https://doi.org/10.5281/zenodo.3526659)
<!-- badges: end -->

## Overview

A dependency-free collection of simple functions for cleaning
rectangular data. This package allows to detect, count and replace
values or discard rows/columns using a predicate function. In addition,
it provides tools to check conditions and return informative error
messages.

## Installation

You can install the released version of **arkhe** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("arkhe")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/arkhe")
```

## Usage

``` r
## Load the package
library(arkhe)

## Create a matrix
X <- matrix(sample(1:10, 25, TRUE), nrow = 5, ncol = 5)

## Add NA
k <- sample(1:25, 3, FALSE)
X[k] <- NA
X
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    4    6    1    2    8
#> [2,]    6    8    2    7   10
#> [3,]   NA    1    2    9    2
#> [4,]   NA    9   NA   10    8
#> [5,]    5    4    3    6    6

## Count missing values in rows
count(X, f = is.na, margin = 1)
#> [1] 0 0 1 2 0
## Count non-missing values in columns
count(X, f = is.na, margin = 2, negate = TRUE)
#> [1] 3 5 4 5 5

## Find row with NA
detect(X, f = is.na, margin = 1)
#> [1] FALSE FALSE  TRUE  TRUE FALSE
## Find column without any NA
detect(X, f = is.na, margin = 2, negate = TRUE, all = TRUE)
#> [1] FALSE  TRUE FALSE  TRUE  TRUE

## Remove row with any NA
discard(X, f = is.na, margin = 1, all = FALSE)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    4    6    1    2    8
#> [2,]    6    8    2    7   10
#> [3,]    5    4    3    6    6
## Remove column with any NA
discard(X, f = is.na, margin = 2, all = FALSE)
#>      [,1] [,2] [,3]
#> [1,]    6    2    8
#> [2,]    8    7   10
#> [3,]    1    9    2
#> [4,]    9   10    8
#> [5,]    4    6    6

## Replace NA with zeros
replace_NA(X, value = 0)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    4    6    1    2    8
#> [2,]    6    8    2    7   10
#> [3,]    0    1    2    9    2
#> [4,]    0    9    0   10    8
#> [5,]    5    4    3    6    6
```

## Contributing

Please note that the **arkhe** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
