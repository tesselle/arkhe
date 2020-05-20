
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arkhe

<!-- badges: start -->

[![R build
status](https://github.com/nfrerebeau/arkhe/workflows/R-CMD-check/badge.svg)](https://github.com/nfrerebeau/arkhe/actions)
[![codecov](https://codecov.io/gh/nfrerebeau/arkhe/branch/master/graph/badge.svg)](https://codecov.io/gh/nfrerebeau/arkhe)

[![CRAN
Version](http://www.r-pkg.org/badges/version/arkhe)](https://cran.r-project.org/package=arkhe)
[![CRAN
checks](https://cranchecks.info/badges/worst/arkhe)](https://cran.r-project.org/web/checks/check_results_arkhe.html)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/arkhe)](https://cran.r-project.org/package=arkhe)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3526659.svg)](https://doi.org/10.5281/zenodo.3526659)
<!-- badges: end -->

## Overview

A collection of classes that represent archaeological data. This package
provides a set of S4 classes that represent different special types of
matrix (absolute/relative frequency, presence/absence data,
co-occurrence matrix, etc.) upon which package developers can build
subclasses. It also provides a set of generic methods (mutators and
coercion mechanisms) and functions (e.g. predicates). In addition, a few
classes of general interest (e.g. that represent stratigraphic
relationships) are implemented.

## Installation

You can install the released version of **arkhe** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("arkhe")
```

Or install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("nfrerebeau/arkhe")
```

## Usage

``` r
# Load the package
library(arkhe)
```

**arkhe** provides a set of S4 classes that represent different special
types of matrix.

  - Integer matrix:
      - `CountMatrix` represents absolute frequency data,
  - Numeric matrix:
      - `AbundanceMatrix` represents relative frequency data,
      - `OccurrenceMatrix` represents a co-occurrence matrix,
      - `SimilarityMatrix` represents a (dis)similarity matrix,
  - Logical matrix:
      - `IncidenceMatrix` represents presence/absence data,
      - `StratigraphicMatrix` represents stratigraphic relationships.

*It assumes that you keep your data tidy*: each variable (type/taxa)
must be saved in its own column and each observation (assemblage/sample)
must be saved in its own row.

These new classes are of simple use, on the same way as the base
`matrix`:

``` r
# Define a count data matrix
# (data will be rounded to zero decimal places, then coerced with as.integer)
quanti <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10)

# Define a logical matrix
# (data will be coerced with as.logical)
quali <- IncidenceMatrix(data = sample(0:1, 100, TRUE), nrow = 10, ncol = 10)
```

**arkhe** uses coercing mechanisms (with validation methods) for data
type conversions:

``` r
## Create a count matrix
A0 <- matrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10)

## Coerce to absolute frequencies
A1 <- as_count(A0)

## Coerce to relative frequencies
B <- as_abundance(A1)

## Row sums are internally stored before coercing to a frequency matrix
## (use get_totals() to get these values)
## This allows to restore the source data
A2 <- as_count(B)
all(A1 == A2)
#> [1] TRUE

## Coerce to presence/absence
C <- as_incidence(A1)

## Coerce to a co-occurrence matrix
D <- as_occurrence(A1)
```

Many familiar methods and group generic functions are available for all
`*Matrix` classes (such as `length`, `dim`, `rowSums`, `rowMeans`,
`Arith`, `Compare`, `Logic`…). In addition, all functions that call
`as.matrix` or `as.data.frame` first on their main argument should work
(e. g. `apply`).

``` r
rowSums(A1)
#>  row1  row2  row3  row4  row5  row6  row7  row8  row9 row10 
#>    44    57    56    40    53    58    42    61    55    54
apply(X = A1, MARGIN = 1, FUN = sum)
#>  row1  row2  row3  row4  row5  row6  row7  row8  row9 row10 
#>    44    57    56    40    53    58    42    61    55    54
```

Please note that all `*Matrix` classes extend the R base `matrix`, but
the S3 part of the object **does not** store the data. Values are stored
in a specific slot (allowing type checking).

``` r
X <- CountMatrix(data = sample(0:10, 25, TRUE), nrow = 5, ncol = 5)

## Get the S3 object
S3Part(X, strictS3 = TRUE)
#>      col1 col2 col3 col4 col5
#> row1    1    6   11   16   21
#> row2    2    7   12   17   22
#> row3    3    8   13   18   23
#> row4    4    9   14   19   24
#> row5    5   10   15   20   25

## Coerce to an S3 matrix
as.matrix(X)
#>      col1 col2 col3 col4 col5
#> row1    5    3    1    0    5
#> row2    9    2    5    0    7
#> row3    4    7    0    3   10
#> row4    9   10    2    3    3
#> row5    5    9    3    9    9
```

## Contributing

Please note that the **arkhe** project is released with a [Contributor
Code of
Conduct](https://github.com/nfrerebeau/arkhe/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
