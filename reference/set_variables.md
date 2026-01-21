# Helper function to set optional variables and subset data

A utility function to subset specimen data by optional specimen
biometric categories, and to set these variables for further use in
CPUE, abundance, and biomass calculations within the
[`crabpack::calc_cpue()`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/calc_cpue.md)
and
[`crabpack::calc_bioabund()`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/calc_bioabund.md)
functions.

## Usage

``` r
set_variables(
  crab_data = NULL,
  species = NULL,
  sex = NULL,
  size_min = NULL,
  size_max = NULL,
  crab_category = NULL,
  female_maturity = c("morphometric", "cutline")[1],
  shell_condition = NULL,
  egg_condition = NULL,
  clutch_size = NULL,
  bin_1mm = FALSE
)
```

## Arguments

- crab_data:

  Object created from
  [`crabpack::get_specimen_data()`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/get_specimen_data.md).

- species:

  Character string. One of
  `c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR")`.

- sex:

  Character string. One or both of `c("male", "female")`. Optional,
  `"male"` or `"female"` only will provide estimates for the selected
  sex, specifying both `"male"` and `"female"` will provide estimates
  for each of the selected sexes.

- size_min:

  Numeric. Optional, desired lower range of crab size (inclusive).

- size_max:

  Numeric. Optional, desired upper range of crab size (inclusive).

- crab_category:

  Character string. One or many of
  `c("mature_male", "large_male", "legal_male", "preferred_male", "immature_male", "small_male", "sublegal_male", "mature_female", "immature_female", "all_categories")`.
  Optional, specifying this parameter will provide estimates for each of
  the selected categories; `"all_categories"` will provide estimates for
  each relevant category for the given species. If using a female
  category, maturity will be based on morphometric maturity (default
  `female_maturity = "morphometric"`). Set `female_maturity = "cutline"`
  if you want to define female maturity based on ADF&G size cutlines.
  See
  [`crabpack::set_crab_category()`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/set_crab_category.md)
  documentation for description of species-specific category
  combinations.

- female_maturity:

  Character string. One of `c("morphometric", "cutline")`. Defaults to
  `"morphometric"` maturity for female crab. Morphometric maturity
  biomass and abundance estimates are not available for male crab at
  this time.

- shell_condition:

  Character string. One or many of
  `c("soft_molting", "new_hardshell", "oldshell", "very_oldshell", "all_categories")`.
  Optional, specifying this parameter will provide estimates for each of
  the selected shell conditions; `"all_categories"` will provide
  estimates for each available shell condition category.

- egg_condition:

  Character string. One or many of
  `c("none", "uneyed", "eyed", "dead", "empty_cases", "hatching", "all_categories")`.
  Optional, specifying this parameter will provide estimates for each of
  the selected egg conditions; `"all_categories"` will provide estimates
  for each available egg condition category. Note that specifying
  `egg_condition` will return only female specimens in the final output.

- clutch_size:

  Character string. One or many of
  `c("immature", "mature_barren", "trace", "quarter", "half", "three_quarter", "full", "all_categories")`.
  Optional, specifying this parameter will provide estimates for each of
  the selected clutch sizes; `"all_categories"` will provide estimates
  for each available clutch size category. Note that specifying
  `clutch_size` will return only female specimens in the final output.

- bin_1mm:

  Boolean T/F. If `TRUE`, estimates will be provided for each 1mm bin
  within the size range specified in `size_min` and/or `size_max`, or
  for the full range of observed sizes in the data. Defaults to `FALSE`.

## Value

A named list of subsetted specimen data by biometrics of interest as
well as variables to be carried forward for CPUE, abundance, and biomass
calculations across the selected biometrics.
