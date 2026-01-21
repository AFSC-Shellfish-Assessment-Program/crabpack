# Calculate index of stratum-, district-, or region-level abundance and biomass

This function calculates indices of abundance and biomass (mt and lbs)
at the stratum-, district-, or region-level for a given crab species.
Optional arguments also allow these indices to be calculated for subsets
of crab by biometric categories such as size, sex, maturity, and shell
condition.

## Usage

``` r
calc_bioabund(
  crab_data = NULL,
  species = NULL,
  region = c("EBS", "NBS")[1],
  district = c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT", "UNSTRAT", "E166", "W166",
    "166TO173")[1],
  years = NULL,
  sex = NULL,
  size_min = NULL,
  size_max = NULL,
  crab_category = NULL,
  female_maturity = c("morphometric", "cutline")[1],
  shell_condition = NULL,
  egg_condition = NULL,
  clutch_size = NULL,
  bin_1mm = FALSE,
  spatial_level = c("stratum", "district", "region")[2],
  replace_retow = TRUE
)
```

## Arguments

- crab_data:

  Object created from
  [`crabpack::get_specimen_data()`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/get_specimen_data.md).

- species:

  Character string. One of
  `c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR")`.

- region:

  Character string describing the region of interest. One of
  `c("EBS", "NBS"`). Defaults to `"EBS"` for Eastern Bering Sea.

- district:

  Character string. One or many of
  `c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173")`.
  Defaults to `"ALL"` districts within the selected region if not
  specified.

- years:

  Numeric or integer vector of years. Default output is all available
  years in data.

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

- spatial_level:

  Character string. One of `c("stratum", "district", "region")`.
  Describes the spatial resolution of biomass and abundance output,
  aggregated to the stratum, district (default), or regional scale.

- replace_retow:

  Boolean T/F. If `TRUE`, replace female Bristol Bay red king crab
  observations with resampled data (haul type = 17) in years when a
  Bristol Bay retow took place. Defaults to `TRUE`, please use care when
  interpreting BBRKC outputs if using `FALSE`.

## Value

A data frame with area swept-expanded estimates of stratum-, district-,
or region-level abundance, biomass (mt), and biomass (lbs) by year.

|                    |                                                                                                                                                        |
|--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Field**          | **Description**                                                                                                                                        |
| ABUNDANCE          | Estimated crab abundance using an area-swept expansion.                                                                                                |
| ABUNDANCE_CI       | Estimated crab abundance 95% confidence interval.                                                                                                      |
| ABUNDANCE_CV       | Estimated crab abundance coefficient of variation.                                                                                                     |
| BIOMASS_LBS        | Estimated crab biomass in pounds.                                                                                                                      |
| BIOMASS_LBS_CI     | Estimated crab biomass (pounds) 95% confidence interval.                                                                                               |
| BIOMASS_LBS_CV     | Estimated crab biomass (pounds) coefficient of variation.                                                                                              |
| BIOMASS_MT         | Estimated crab biomass in metric tons.                                                                                                                 |
| BIOMASS_MT_CI      | Estimated crab biomass (metric tons) 95% confidence interval.                                                                                          |
| BIOMASS_MT_CV      | Estimated crab biomass (metric tons) coefficient of variation.                                                                                         |
| CATEGORY           | Shorthand name of crab sex-size-maturity category.                                                                                                     |
| CLUTCH_TEXT        | Text describing amount of eggs, if any, in female crabs.                                                                                               |
| DISTRICT           | Name of crab district, shorthand.                                                                                                                      |
| EGG_CONDITION_TEXT | Text describing egg condition category in female crabs.                                                                                                |
| REGION             | Survey region. EBS = Eastern Bering Sea, NBS = Northern Bering Sea.                                                                                    |
| SEX_TEXT           | Text describing sex of crab.                                                                                                                           |
| SHELL_TEXT         | Text describing carapace condition of crab.                                                                                                            |
| SIZE_1MM           | Measurement of crab in 1 millimeter bins. This number is carapace length for king crab and hair crab species, and carapace width for Chionoecetes spp. |
| SPECIES            | Name of species, shorthand.                                                                                                                            |
| TOTAL_AREA         | Total area of stratum, in square nautical miles.                                                                                                       |
| YEAR               | Year the observation (survey) was collected.                                                                                                           |
