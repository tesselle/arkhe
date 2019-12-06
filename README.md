
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codex

<!-- badges: start -->

[![Appveyor build
status](https://ci.appveyor.com/api/projects/status/7xt157mqcny0pht1?svg=true)](https://ci.appveyor.com/project/nfrerebeau/codex)
[![Travis build
Status](https://travis-ci.org/nfrerebeau/codex.svg?branch=master)](https://travis-ci.org/nfrerebeau/codex)
[![codecov](https://codecov.io/gh/nfrerebeau/codex/branch/master/graph/badge.svg)](https://codecov.io/gh/nfrerebeau/codex)

[![CRAN
Version](http://www.r-pkg.org/badges/version/codex)](https://cran.r-project.org/package=codex)
[![CRAN
checks](https://cranchecks.info/badges/worst/codex)](https://cran.r-project.org/web/checks/check_results_codex.html)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/codex)](https://cran.r-project.org/package=codex)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3526659.svg)](https://doi.org/10.5281/zenodo.3526659)
<!-- badges: end -->

## Overview

A collection of classes that represent archaeological data. This package
provides a set of S4 classes that extend the basic matrix data type
(abundance matrix and stratigraphic matrix) upon which package
developers can build subclasses. It also provides a set of generic
methods (mutators and coercion mechanisms) and functions
(e.g. predicates). In addition, a few classes of general interest
(e.g. that represent chronological data) are
implemented.

## Installation

<!--You can install the released version of **codex** from [CRAN](https://CRAN.R-project.org) with:


```r
install.packages("codex")
```

Or-->

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("nfrerebeau/codex")
```

## Usage

``` r
# Load the package
library(codex)
```

``` r
# See the vignette
utils::vignette("codex", package = "codex")
```

**codex** provides a set of S4 classes that extend the basic `matrix`
data type. These new classes represent different special types of
matrix.

  - Numeric matrix:
      - `CountMatrix` represents absolute frequency data,
      - `AbundanceMatrix` represents relative frequency data,
      - `OccurrenceMatrix` represents a co-occurrence matrix,
      - `SimilarityMatrix` represents a (dis)similarity matrix,
  - Logical matrix:
      - `IncidenceMatrix` represents presence/absence data,
      - `StratigraphicMatrix` represents stratigraphic relationships.

*It assumes that you keep your data tidy*: each variable (type/taxa)
must be saved in its own column and each observation (sample/case) must
be saved in its own row.

These new classes are of simple use, on the same way as the base
`matrix`:

``` r
# Define a count data matrix
quanti <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10)

# Define a logical matrix
# Data will be coerced with as.logical()
quali <- IncidenceMatrix(data = sample(0:1, 100, TRUE), nrow = 10, ncol = 10)
```

**codex** uses coercing mechanisms (with validation methods) for data
type conversions:

``` r
## Create a count matrix
A0 <- matrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10)

## Coerce to absolute frequencies
A1 <- as_count(A0)

## Coerce to relative frequencies
B <- as_abundance(A1)

## Row sums are internally stored before coercing to a frequency matrix
## (use totals() to get these values)
## This allows to restore the source data
A2 <- as_count(B)
all(A1 == A2)
#> [1] TRUE

## Coerce to presence/absence
C <- as_incidence(A1)

## Coerce to a co-occurrence matrix
D <- as_occurrence(A1)
```

Represent stratigraphic relationships:

``` r
# Principles of Archaeological Stratigraphy, fig. 12
harris <- read.table(
  header = TRUE,
  text = "lower upper
          2     1
          3     1
          4     1
          5     2
          5     3
          5     4
          6     5
          7     1
          7     6
          8     1
          8     6
          9     7
          9     8"
)

as_stratigraphy(harris)
#> 9 x 9 stratigraphic matrix:
#> (3eff8c95-ed0e-48dd-9936-67ab9e767661)
#>       1     2     3     4     5     6     7     8     9
#> 1 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 2  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 3  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 4  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 5 FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
#> 6 FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
#> 7  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
#> 8  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
#> 9 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE
```

## Contributing

Please note that the **codex** project is released with a [Contributor
Code of
Conduct](https://github.com/nfrerebeau/codex/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
