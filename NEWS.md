# arkhe 1.0.0
## New classes and methods
* Add `append_rownames()` to convert row names to an explicit column.
* Add `assert_positive()`, `assert_negative()`, `assert_odd()`, `assert_even()`, `assert_constant()`, `assert_decreasing()`, `assert_increasing()`, `assert_lower()` and `assert_greater()` to check `numeric` vectors.
* Add `bootstrap()` for bootstrap estimation.
* Add `compact()` to remove empty columns/rows in an array-like object.
* Add `discard()` and `keep()` remove/keep rows/columns in an array-like object using a predicate function.
* Add `to_long()` to transform a `matrix` to a long `data.frame`.

## Bugfixes & changes
* Export all predicate functions.
* Deprecate `as_long()`, `assert_numeric()`, `assert_trend()`, `assert_relation()`, `assert_matrix()` and `remove_empty()`.

## Breaking changes
* Remove all `*Matrix` classes and methods.
* Rename `compact()` to `discard()`.

# arkhe 0.5.0
## New classes and methods
* Add `assign_rownames()` and `assign_colnames()` to make a specific row/column the column/row names of a `data.frame`.
* Add `assert_count()` to validate count data (absolute frequencies/integer).
* Add `count()` to count values by rows/columns according to a given predicate.
* Add `detect()` to find rows/columns in an array-like object according to a given predicate.
* Add `compact()` to remove rows/columns in an array-like object according to a given predicate.
* Add `jackknife()` for jackknife estimation.
* Add `confidence()` to compute confidence interval for the mean.

# arkhe 0.4.0
## New classes and methods
* Add `replace_Inf()` to replace infinite values in a `matrix`-like object.
* Add `replace_zero()` to replace zero in a `matrix`-like object.
* Add `remove_Inf()` to remove infinite values in a `matrix`-like object.
* Add `assert_*()` and `validate()` to validate objects.
* Add `get_dates()`, `set_dates()<-` to extract/replace `dates` slot.
* Add `get_terminus()`, `set_terminus()<-`, `get_tpq()`, `set_tpq()<-`, `get_taq()`, `set_taq()<-` to extract/replace `tpq` and `taq` slots.
* Add `summary()` for `AbundanceMatrix` objects.

## Internals
* `AbundanceMatrix` class gained a new slot to store the sample sizes (`totals`).
* `AbundanceMatrix` class gained two new slots to store chronological information (`dates`, `tpq`, `taq`).
* Add `AbundanceSummary` class to store summary of an `AbundanceMatrix` object.

# arkhe 0.3.1
## Bugfixes & changes
* Fix warning "data length differs from size of matrix" in examples (R-devel).

# arkhe 0.3.0
## New classes and methods
* Add `replace_NA()` to replace missing values.
* Add `remove_NA()`, `remove_zero()` and `remove_empty()` to remove missing values, zeros and empty rows/columns in a `matrix`.
* Add `get_samples()`, `set_samples()<-`, `get_groups()` and `set_groups()<-` to extract/replace `samples` and `groups` slots.

## Bugfixes & changes
* Change `OccurrenceMatrix` inheritance (from `NumericMatrix` to `IntegerMatrix`).
* Rename `AbundanceMatrix` (ambiguous) to `CompositionMatrix`.
* Deprecate `as_abundance()`.
* Remove `SimilarityMatrix` class.
* `as_long()` gained a new `reverse` argument.

# arkhe 0.2.2
## Bugfixes & changes
* CRAN package check error has been fixed (random error with **testthat**).
* CRAN package check warnings have been fixed (remove **nomnoml** from suggested packages).

# arkhe 0.2.1
## Bugfixes & changes
* CRAN package check warnings have been fixed ("documented arguments not in \usage" in the r-devel checks).

# arkhe 0.2.0
* Initial version on CRAN.

# arkhe 0.1.0
* Beta release.
