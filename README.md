
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

# See the vignette
utils::vignette("codex", package = "codex")
```

## Contributing

Please note that the **codex** project is released with a [Contributor
Code of
Conduct](https://github.com/nfrerebeau/codex/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
