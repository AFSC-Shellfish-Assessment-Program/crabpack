# Pull proportion morphometrically mature Chionoecetes spp. males by size

A time series of model estimated proportion of morphometrically mature
male Tanner Crab and Snow Crab by 10 millimeter size bin for a given
region or district, based on chela height measurements. Yearly model
parameter estimates of 50% probability of maturity at size are also
provided.

## Usage

``` r
get_male_maturity(
  species = NULL,
  region = c("EBS", "NBS")[1],
  district = NULL,
  channel = NULL
)
```

## Arguments

- species:

  Character string. One of `c("TANNER", "SNOW")`.

- region:

  Character string describing the region of interest. One of
  `c("EBS", "NBS"`). Defaults to `"EBS"` for Eastern Bering Sea.

- district:

  Character string. One or many of `c("ALL", "E166", "W166")`. Defaults
  to `"ALL"`; `"E166"` and `"W166"` are used for Tanner Crab only.

- channel:

  Character string or Oracle connection. Defaults to an API connection,
  (`channel = "API"`), allowing for public data access. To use an Oracle
  database connection, set `channel` to an object created via
  [`crabpack::get_connected()`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/get_connected.md)
  or
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
  Local AFSC Kodiak users can also set `channel = "KOD"` to access data
  on the network drives (requires VPN connection). This option will pull
  all available years and districts for the given species and region.

## Value

A named list containing the proportion of male Chionoecetes spp. crab
that are morphometrically mature in a given 10mm size bin, and yearly
model parameter estimates for 50% probability of maturity at size for
the species, region, and district of interest.
