# Example Workflow

## Load package

``` r
library(crabpack)
```

  

## Data connections

### Connect via API

Public data access, no credentials needed.

``` r
# Specify AKFIN API 
  channel <- "API"
```

### Connect via Oracle database

Username and password required.

``` r
# Connect to Oracle via AKFIN database (default)
  channel <- crabpack::get_connected()

# Connect to Oracle via AFSC database (NOAA AFSC users only)
  channel <- crabpack::get_connected(db = "AFSC")
```

### Connect via Kodiak AFSC network drives

VPN connection required.

``` r
# Set local Kodiak server access (Kodiak Fisheries Research Center users only)
  channel <- "KOD"
```

  

## Pull specimen data

``` r
species <- "RKC"
specimen_data <- crabpack::get_specimen_data(species = species,
                                             region = "EBS",
                                             years = c(1975:2024),
                                             channel = channel)
```

  

## Calculate time series of district-level biomass and abundance

See
[`?crabpack::calc_bioabund`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/calc_bioabund.md)
for details on arguments  
***Note:** for each example, please verify that you have the appropriate
species data loaded*

**Example 1:** Mature female Bristol Bay Red King Crab using ADF&G
cutlines

``` r
matfem_bbrkc <- crabpack::calc_bioabund(crab_data = specimen_data,
                                        species = "RKC",
                                        region = "EBS",
                                        district = "BB",
                                        crab_category = "mature_female",
                                        female_maturity = "cutline")
```

**Example 2:** Male St. Matthew district Blue King Crab between 105 and
199mm

``` r
smbkc_105TO119 <- crabpack::calc_bioabund(crab_data = specimen_data,
                                          species = "BKC",
                                          region = "EBS",
                                          district = "STMATT",
                                          sex = "male",
                                          size_min = 105,
                                          size_max = 119)
```

**Example 3:** Legal-sized male Tanner Crab for both the E166 and W166
districts

``` r
legal_tanner <- crabpack::calc_bioabund(crab_data = specimen_data,
                                        species = "TANNER",
                                        region = "EBS",
                                        district = "ALL",
                                        crab_category = "legal_male")
```

**Example 4:** Large, new hardshell male EBS Snow Crab by 1mm bins for
2000-2024

``` r
lg_sc2_snow <- crabpack::calc_bioabund(crab_data = specimen_data,
                                       species = "SNOW",
                                       region = "EBS",
                                       years = c(2000, 2024),
                                       sex = "male",
                                       size_min = 95,
                                       shell_condition = "new_hardshell",
                                       bin_1mm = TRUE)
```

  

## Calculate time series of station-level CPUE

See
[`?crabpack::calc_cpue`](https://afsc-shellfish-assessment-program.github.io/crabpack/reference/calc_cpue.md)
for details on arguments

**Example 1:** 2024 EBS Red King Crab by size-sex-maturity category

``` r
cpue <- crabpack::calc_cpue(crab_data = specimen_data,
                            species = "RKC",
                            region = "EBS",
                            district = "ALL",
                            years = 2024,
                            crab_category = "all_categories")
```

  

## Pull data on *Chionoecetes* spp. male morphometric maturity estimation

``` r
male_maturity_data <- crabpack::get_male_maturity(species = "TANNER",
                                                  region = "EBS",
                                                  district = "E166",
                                                  channel = channel)

  # Unpack df: proportion of morphometrically mature male crabs by size 
    proportion_mature <- male_maturity_data$male_mat_ratio
  
  # Unpack df: model parameter estimates of 50% probability of maturity at size
    matmodel_params <- male_maturity_data$model_parameters
```

  
