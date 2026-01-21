# Assign size-sex-maturity categories based on species- and district-specific size definitions

A utility function to assign standard size-sex-maturity categories to
crab specimen data. Standard categories for Red King Crab and Blue King
Crab are `immature_male`, `mature_male`, `legal_male`,
`immature_female`, and `mature_female`. Categories for Tanner Crab, Snow
Crab, and Hybrid Crab are `small_male`, `large_male`, `legal_male`,
`preferred_male`, `immature_female`, and `mature_female`. Categories for
Hair Crab are `sublegal_male`, `legal_male`, and `female`.

## Usage

``` r
set_crab_category(
  crab_data = NULL,
  species = NULL,
  region = c("EBS", "NBS")[1],
  district = NULL,
  crab_category = NULL,
  female_maturity = c("morphometric", "cutline")[1]
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

- crab_category:

  Character string. One or many of
  `c("mature_male", "large_male", "legal_male", "preferred_male", "immature_male", "small_male", "sublegal_male", "mature_female", "immature_female", "all_categories")`.
  Optional, specifying this parameter will provide estimates for each of
  the selected categories; `"all_categories"` will provide estimates for
  each relevant category for the given species. If using a female
  category, maturity will be based on morphometric maturity (default
  `female_maturity = "morphometric"`). Set `female_maturity = "cutline"`
  if you want to define female maturity based on ADF&G size cutlines.

- female_maturity:

  Character string. One of `c("morphometric", "cutline")`. Defaults to
  `"morphometric"` maturity for female crab. Morphometric maturity
  biomass and abundance estimates are not available for male crab at
  this time.

## Value

A data frame with crab specimen data and an additional "CATEGORY" column
with stock-specific maturity designations.
