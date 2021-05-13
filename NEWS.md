# arkhe 0.3.1

## Bugfixes & changes
* Fix warning "data length differs from size of matrix" in examples (R-devel).

# arkhe 0.3.0
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4675724.svg)](https://doi.org/10.5281/zenodo.4675724)

## New classes and methods
* Add `replace_NA()` to replace missing values.
* Add `remove_NA()`, `remove_zero()` and `remove_empty()` to remove missing values, zeros and empty rows/columns in a `matrix`.
* Add `get_samples()`, `set_samples()`, `get_groups()` and `set_groups()` to extract/replace `samples` and `groups` slots.

## Bugfixes & changes
* Change `OccurrenceMatrix` inheritance (from `NumericMatrix` to `IntegerMatrix`).
* Rename `AbundanceMatrix` (ambiguous) to `CompositionMatrix`.
* Deprecate `as_abundance()`.
* Remove `SimilarityMatrix` class.
* `as_long()` gained a new `reverse` argument.

# arkhe 0.2.2
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3724463.svg)](https://doi.org/10.5281/zenodo.3724463)

## Bugfixes & changes
* CRAN package check error has been fixed (random error with **testthat**).
* CRAN package check warnings have been fixed (remove **nomnoml** from suggested packages).

# arkhe 0.2.1
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3668868.svg)](https://doi.org/10.5281/zenodo.3668868)

## Bugfixes & changes
* CRAN package check warnings have been fixed (documented arguments not in \usage' in the r-devel checks).

# arkhe 0.2.0
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3583089.svg)](https://doi.org/10.5281/zenodo.3583089)

* Initial version on CRAN.

# arkhe 0.1.0
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3526660.svg)](https://doi.org/10.5281/zenodo.3526660)

* Beta release.
