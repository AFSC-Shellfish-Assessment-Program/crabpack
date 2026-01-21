# Define DBI odbc connection to Oracle

Creates the `odbc` connection to Oracle needed to pull SQL queries from
AKFIN database. Also support users who use the R package `keyring` to
store usernames and passwords.

## Usage

``` r
get_connected(db = "AKFIN", check_access = TRUE)
```

## Arguments

- db:

  String. A registered data source name, in this case `"AKFIN"` by
  default. This argument is passed to the `drv` argument in
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- check_access:

  Boolean. If `TRUE` (by default), checks whether you have the specific
  tables in AKFIN used in the `crabpack` package. Outputs an error if
  the user does not have access to these tables with a message of the
  point of contact information for access.

## Value

Channel of class "Oracle". See `?DBI::dbConnect()` for more detail.
