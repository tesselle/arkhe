# arkhe 1.8.0
## New classes and methods
* Add `append_column()` to add a named vector as a column in a `data.frame`.
* Add `assert_nrow()` and `assert_ncol()` to check the number of rows/columns.

## Enhancements
* Translate into French.
* `seek_rows()` and `seek_columns()` gained a new `names` argument.
* `assert_type()` gained new `allow_empty` and `allow_null` arguments.

# arkhe 1.7.0
## New classes and methods
* Add `scale_midpoint` to rescale a continuous vector to have specified minimum, midpoint and maximum.

## Internals
* Reexport `%||%` from base on newer versions of R to avoid conflict messages.

## Breaking changes
* Move `palette_*()` functions to **khroma**.

# arkhe 1.6.0
## New classes and methods
* Add `describe()` to quickly describe a `matrix`-like object.
* Add `sparsity()` to computes data sparsity (proportion of zeros).

# arkhe 1.5.0
## New classes and methods
* Add `palette_color_continuous()` and `palette_color_discrete()` for color mapping.
* Add `palette_shape()` and `palette_size()` for symbol shape and size mapping.
* Add `clean_whitespace()` to remove leading/trailing Whitespace.
* Add `seek_*()` and `get_*()` to find and get rows/columns by names in a `data.frame`.

## Enhancements
* `compact()` and `remove_*()` gained a new `verbose` argument to report extra information on progress.

## Bugfixes & changes
* Rename `needs()` to `assert_package()`.

# arkhe 1.4.0
## New classes and methods
* Add `%||%` to replace `NULL` with a default value.
* Add `%+%` to concatenate character vectors.
* Add `scale_range` to rescale a continuous vector to have specified minimum and maximum.

## Bugfixes & changes
* Remove deprecated methods.
* Remove `na.rm` argument of numeric predicates (so that the length of the output equals the length of the input).

# arkhe 1.3.0
## New classes and methods
* Add `remove_constant()` to remove constant columns.
* Add `remove_empty()` to remove empty strings in a `matrix`-like object.
* Add `replace_empty()` to replace empty strings in a `matrix`-like object.

## Bugfixes & changes
* Remove deprecated methods.
* Deprecate `to_long()` and `wide_to_long()`.

## Internals
* Use **tinytest** instead of **testthat**.

# arkhe 1.2.0
## New classes and methods
* Add `math_lcm()` and `math_gcd()` to compute the lowest common multiple and the greatest common divisor.
* Add `interval_hdr()` and `interval_credible()` to compute the credible intervals.

## Bugfixes & changes
* `jackknife()` gained a new argument to apply a function on the leave-one-out values (`f`).

## Internals
* Add `with_seed()` to evaluate an expression with a temporarily seed.

# arkhe 1.1.0
## New classes and methods
* Add `needs()` to check for the availability of a package.

## Bugfixes & changes
* Remove deprecated methods.
* Remove unused argument in `assert_unique()`.

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
