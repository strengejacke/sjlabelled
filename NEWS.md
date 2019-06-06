# sjlabelled 1.1.0

## New functions

* `remove_label()`, to remove variable labels (and preserve value labels).

## Changes to functions

`convert_case()` gets a `verbose`-argument to toggle warnings and messages on or off.

# sjlabelled 1.0.17

## General

* Reduce package dependencies.
* New package-vignette on quasiquotation.

## New functions

* Re-implement `set_na()`, to define (labelled) `NA`-values in a vector.

## Changes to functions

* `as_label()` gets a `keep.labels`-argument. With this, users can easily convert vector to factors and vice versa, preserving label-attributes.

## Bug fixes

* Fixed bug with argument `use.labels` in `as_numeric()`.

# sjlabelled 1.0.16

## General

* Started adding test-units.
* Minor code revisions to avoid errors during CRAN check for current devel-versions of R.

## New functions

* `val_labels()` as counterpart to `var_labels()`, to set value labels, with support for quasi-quotation (see Examples).

## Changes to functions

* `var_labels()` now supports quasi-quotation (see Examples).

# sjlabelled 1.0.15

## General

* Update code to the new class-attribute `haven_labelled` from the **haven**-package.

## Bug fixes

* Fix issue in `get_term_labels()` that returned wrong object names for factors where factor levels did start with "1".

# sjlabelled 1.0.14

## General

* Reduce package dependencies.

## Bug fixes

* Fix bug in `var_labels()`, where non-existing columns may lead to wrong labelling.

# sjlabelled 1.0.13

## General

* Removed defuncted functions.

## Changes to functions

* `copy_labels()` now also copy labels even if columns in subsetted and original data frame do not completely match.
* Arguments `include.non.labelled` and `include.values` in `get_labels()` are renamed to shorter versions `non.labelled` and `values`. `include.non.labelled` and `include.values` will become softly deprecated.
* The `read_*()`-functions get a `verbose`-argument, to show or hide the progressbar when imported datasets are converted.

## Bug fixes

* Due to changes in the _broom_ and _lmerTest_ packages, tidiers did no longer work for `lmerModLmerTest` objects.

# sjlabelled 1.0.12

## General

* `get_dv_labels()` and `get_term_labels()` now support _clmm_-objects (package **ordinal**) and _stanmvreg_-objects (package **rstanarm**).
* `read_spss()` gets a `enc`-argument for character encoding, which is now supported since haven 1.1.2.
* `get_term_labels()` now returns `NULL` for unsupported models, instead of giving an error.
* `get_dv_labels()` now returns a default string for unsupported models, instead of giving an error.

# sjlabelled 1.0.11

## General

* `as_labelled()` now corrects inconsistent types between labels and variable values.

## Changes to functions

* `get_dv_labels()` gets a `multi.resp`-argument to return each label of a multivariate response model (only for _brmsfit_ objects).
* `get_label()` now also returns name-attribute for empty labels if `x` was a data.frame.

## Bug fixes

* `write_*()`-functions should now properly set labels for negative values.

# sjlabelled 1.0.9

## General

* Deprecated `set_note()` and `get_note()`, because there is already an R base function for this purpose: `comment()`.
* Improved performance of functions, at the cost of removing support for the _foreign_ package. _sjlabelled_ now only supports labelled data from package _haven_.

## Changes to functions

* `get_term_labels()` gets a `prefix`-argument to prefix the returned labels of categorical variable either with the related variable name or label.

## Bug fixes

* Fix issues with retrieving incorrect labels from `get_term_labels()` for models that used unlabelled data in combination with other contrasts than the default option.
* `get_dv_labels()` no longer returns `"NULL"` for multivariate-response-models fitted with _brms_.

# sjlabelled 1.0.8

## General

* Removed `lbl_df()`, because printing tibbles now depends on pkg _pillar_ and was revised substantially, so maintainace of `lbl_df()` is too extensive.

# sjlabelled 1.0.7

## General

* Cross references from `dplyr::select_helpers` were updated to `tidyselect::select_helpers`.
* Replace deprecated arguments in `convert_case()` from call to package *snakecase*

# sjlabelled 1.0.6

## Changes to functions

* `get_dv_labels()` and `get_term_labels()` now support `clm`-objects from package *ordinal*,  `polr`-objects from package *MASS* and `Zelig-relogit`-objects from package *Zelig*.
* `get_dv_labels()` and `get_term_labels()` get a `...`-argument to pass down further arguments to `snakecase::to_any_case()`.
* `convert_case()` is now exported, for usage in other packages as well.
* Remove `protect`-argument from internal case conversion (affects `get_term_labels()` and `get_dv_labels()`), in preparation for forthcoming *snakecase*-package update.

# sjlabelled 1.0.5

## General

* Remove unnecessary imports.
* Revised `lbl_df()` due to changes in the internals of `tibble::trunc_mat()`.

## New functions

* `as_factor()` to convert labelled vectors into factors, preserving labels.

## Changes to functions

* `get_dv_labels()` now supports `brmsfit`-objects from package `brms`.

# sjlabelled 1.0.4

## Changes to functions

* `get_term_labels()` now includes variable names for factors with numeric factor levels only (and not only return the numeric level as term label).

## Bug fixes

* Fixed bug for `as_label()`, when `x` was a character vector and argument `drop.levels` was `TRUE`.
* Fixed issue for *lme* and *gls* objects in `get_term_labels()` and `get_dv_labels()`.

# sjlabelled 1.0.3

## General

*  Changed package imports, so `sjlabelled` no longer requires R version 3.3.3 or higher.

# sjlabelled 1.0.2

## General

* Minor fix to avoid warning when using `as_numeric()`.

## Changes to functions

* `get_label()`, `get_term_labels()` and `get_dv_labels()` get a `case`-argument, to convert labels into any case, using the [snakecase](https://cran.r-project.org/package=snakecase)-package.

# sjlabelled 1.0.1

## General

* Removed function 'var_rename()', which is in pkg 'sjmisc'.

## New functions

* `get_term_labels()` and `get_dv_labels()` to retrieve term labels from regression models.

## Changes to functions

* `as_numeric()` gets a `use.labels`-argument to use value labels as new values if these are numeric.

# sjlabelled 1.0.0

## General

* Initial release. All labelled data utility functions from package *sjmisc* have been moved to this package, which is now dedicated to tools for working with labelled data.