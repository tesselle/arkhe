
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arkhe <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/arkhe/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/arkhe/actions)
[![codecov](https://codecov.io/gh/tesselle/arkhe/branch/main/graph/badge.svg?token=3TUSMmF18Q)](https://app.codecov.io/gh/tesselle/arkhe)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/arkhe/badge/main)](https://www.codefactor.io/repository/github/tesselle/arkhe/overview/main)
[![Dependencies](https://tinyverse.netlify.app/badge/arkhe)](https://cran.r-project.org/package=arkhe)

<a href="https://tesselle.r-universe.dev/arkhe"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/arkhe"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=arkhe"
class="pkgdown-release"><img
src="https://www.r-pkg.org/badges/version/arkhe"
alt="CRAN Version" /></a>
<a href="https://cran.r-project.org/web/checks/check_results_arkhe.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/arkhe.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=arkhe"
class="pkgdown-release"><img
src="https://cranlogs.r-pkg.org/badges/arkhe"
alt="CRAN Downloads" /></a>

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

------------------------------------------------------------------------

To cite arkhe in publications use:

Frerebeau N (2024). *arkhe: Tools for Cleaning Rectangular Data*.
Université Bordeaux Montaigne, Pessac, France.
<doi:10.5281/zenodo.3526659> <https://doi.org/10.5281/zenodo.3526659>, R
package version 1.8.0, <https://packages.tesselle.org/arkhe/>.

This package is a part of the tesselle project
<https://www.tesselle.org>.

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
#> [1,]   10   10   NA    7    7
#> [2,]    6    4    7   10    3
#> [3,]    7   10    5    9    6
#> [4,]    7   10    4   NA    2
#> [5,]   NA    9    6    9    7

## Count missing values in rows
count(X, f = is.na, margin = 1)
#> [1] 1 0 0 1 1

## Count non-missing values in columns
count(X, f = is.na, margin = 2, negate = TRUE)
#> [1] 4 5 4 4 5

## Find row with NA
detect(X, f = is.na, margin = 1)
#> [1]  TRUE FALSE FALSE  TRUE  TRUE

## Find column without any NA
detect(X, f = is.na, margin = 2, negate = TRUE, all = TRUE)
#> [1] FALSE  TRUE FALSE FALSE  TRUE

## Remove row with any NA
discard(X, f = is.na, margin = 1, all = FALSE)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    6    4    7   10    3
#> [2,]    7   10    5    9    6

## Remove column with any NA
discard(X, f = is.na, margin = 2, all = FALSE)
#>      [,1] [,2]
#> [1,]   10    7
#> [2,]    4    3
#> [3,]   10    6
#> [4,]   10    2
#> [5,]    9    7

## Replace NA with zeros
replace_NA(X, value = 0)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   10   10    0    7    7
#> [2,]    6    4    7   10    3
#> [3,]    7   10    5    9    6
#> [4,]    7   10    4    0    2
#> [5,]    0    9    6    9    7
```

## Contributing

Please note that the **arkhe** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
