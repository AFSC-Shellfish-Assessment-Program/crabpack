# Calculate haul-level catch per unit effort

This function calculates and zero-fills weight (mt, lbs) and numerical
catch per unit effort (area swept, nm2).

## Usage

``` r
calc_cpue(
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
  replace_retow = TRUE,
  output = c("cpue", "bioabund")[1]
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
  `"morphometric"` maturity for female crab. Morphometric maturity CPUE
  estimates are not available for male crab at this time.

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

- replace_retow:

  Boolean T/F. If `TRUE`, replace female Bristol Bay red king crab
  observations with resampled data (haul type = 17) in years when a
  Bristol Bay retow took place. Defaults to `TRUE`, please use care when
  interpreting BBRKC outputs if using `FALSE`.

- output:

  Character string. One of `c("cpue", "bioabund")`. Default output is
  `"cpue"`; `"bioabund"` is used for internal package purposes only.

## Value

A data frame with station-level crab counts, and area swept-expanded
estimates of weight (mt, lbs) and numerical CPUE by year.

|                    |                                                                                                                                                                                     |
|--------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Field**          | **Description**                                                                                                                                                                     |
| CATEGORY           | Shorthand name of crab sex-size-maturity category.                                                                                                                                  |
| CLUTCH_TEXT        | Text describing amount of eggs, if any, in female crabs.                                                                                                                            |
| COUNT              | Number of crab caught at station.                                                                                                                                                   |
| CPUE               | Numerical catch per unit effort (area swept by the net, in square nautical miles).                                                                                                  |
| CPUE_LBS           | Catch weight (lbs) per unit effort (area swept by the net, in square nautical miles).                                                                                               |
| CPUE_MT            | Catch weight (metric tons) per unit effort (area swept by the net, in square nautical miles).                                                                                       |
| DISTRICT           | Name of crab district, shorthand.                                                                                                                                                   |
| EGG_CONDITION_TEXT | Text describing egg condition category in female crabs.                                                                                                                             |
| LATITUDE           | The latitude (decimal degrees) at the midpoint of the haul for a given station.                                                                                                     |
| LONGITUDE          | The longitude (decimal degrees) at the midpoint of the haul for a given station.                                                                                                    |
| REGION             | Survey region. EBS = Eastern Bering Sea, NBS = Northern Bering Sea.                                                                                                                 |
| SEX_TEXT           | Text describing sex of crab.                                                                                                                                                        |
| SHELL_TEXT         | Text describing carapace condition of crab.                                                                                                                                         |
| SIZE_1MM           | Measurement of crab in 1 millimeter bins. This number is carapace length for king crab and hair crab species, and carapace width for Chionoecetes spp.                              |
| SPECIES            | Name of species, shorthand.                                                                                                                                                         |
| STATION_ID         | Alphanumeric character that identifies a survey trawl location based. Stations are typically on a 20x20 nm grid, with Pribilof and St. Matthew corner stations included in the EBS. |
| STRATUM            | Name of stratum within crab district, shorthand.                                                                                                                                    |
| TOTAL_AREA         | Total area of stratum, in square nautical miles.                                                                                                                                    |
| YEAR               | Year the observation (survey) was collected.                                                                                                                                        |
