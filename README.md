
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arkhe <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/arkhe/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/arkhe/actions)
[![codecov](https://codecov.io/gh/tesselle/arkhe/branch/main/graph/badge.svg?token=3TUSMmF18Q)](https://app.codecov.io/gh/tesselle/arkhe)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/arkhe/badge/main)](https://www.codefactor.io/repository/github/tesselle/arkhe/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/arkhe)](https://cran.r-project.org/package=arkhe)

<a href="https://tesselle.r-universe.dev/arkhe"
class="pkgdown-devel"><img
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

    To cite arkhe in publications use:

      Frerebeau N (2024). _arkhe: Tools for Cleaning Rectangular Data_.
      Université Bordeaux Montaigne, Pessac, France.
      doi:10.5281/zenodo.3526659 <https://doi.org/10.5281/zenodo.3526659>,
      R package version 1.5.0, <https://packages.tesselle.org/arkhe/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        author = {Nicolas Frerebeau},
        title = {{arkhe: Tools for Cleaning Rectangular Data}},
        year = {2024},
        organization = {Université Bordeaux Montaigne},
        address = {Pessac, France},
        note = {R package version 1.5.0},
        url = {https://packages.tesselle.org/arkhe/},
        doi = {10.5281/zenodo.3526659},
      }

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
#> [1,]    3    1    4    9    6
#> [2,]    8   NA    7    8    6
#> [3,]    5    9    4    4    8
#> [4,]    7   NA   NA    3    4
#> [5,]    8    2    5    3    4

## Count missing values in rows
count(X, f = is.na, margin = 1)
#> [1] 0 1 0 2 0
## Count non-missing values in columns
count(X, f = is.na, margin = 2, negate = TRUE)
#> [1] 5 3 4 5 5

## Find row with NA
detect(X, f = is.na, margin = 1)
#> [1] FALSE  TRUE FALSE  TRUE FALSE
## Find column without any NA
detect(X, f = is.na, margin = 2, negate = TRUE, all = TRUE)
#> [1]  TRUE FALSE FALSE  TRUE  TRUE

## Remove row with any NA
discard(X, f = is.na, margin = 1, all = FALSE)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    3    1    4    9    6
#> [2,]    5    9    4    4    8
#> [3,]    8    2    5    3    4
## Remove column with any NA
discard(X, f = is.na, margin = 2, all = FALSE)
#>      [,1] [,2] [,3]
#> [1,]    3    9    6
#> [2,]    8    8    6
#> [3,]    5    4    8
#> [4,]    7    3    4
#> [5,]    8    3    4

## Replace NA with zeros
replace_NA(X, value = 0)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    3    1    4    9    6
#> [2,]    8    0    7    8    6
#> [3,]    5    9    4    4    8
#> [4,]    7    0    0    3    4
#> [5,]    8    2    5    3    4
```

## Contributing

Please note that the **arkhe** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
