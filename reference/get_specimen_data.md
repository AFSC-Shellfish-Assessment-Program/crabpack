# Pull AFSC Shellfish Assessment Program Bering Sea survey data

Pulls haul, specimen, and stratum grouping information for the region,
districts, years, and species of interest from the CRABBASE schema in
the AKFIN Oracle database.

## Usage

``` r
get_specimen_data(
  species = NULL,
  region = c("EBS", "NBS")[1],
  district = NULL,
  years = NULL,
  channel = NULL
)
```

## Arguments

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

A named list containing specimen, haul, stratum, area, and size group
information for the region, districts, years, and species of interest.
