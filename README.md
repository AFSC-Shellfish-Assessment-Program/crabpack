# crabpack

## Welcome 

This R package generates the standard design-based indices of catch per unit 
effort, abundance, and biomass from NOAA-NMFS-AFSC-RACE-SAP Eastern Bering Sea Shelf 
(from 1975) and Northern Bering Sea Shelf (from 2010) bottom trawl survey data.

## Installation Instructions

In addition to the default packages in R (base, stats, utils), the functions in 
crabpack use functions from _XX_ additional packages:

- RODBC: Enables ODBC (Open Database Connectivity) interface the AKFIN Oracle
  database where the survey data are housed
- getPass: Allows for masking of Oracle user credentials (usernames, passwords) 
  when accessing RODBC
- keyring: Allows the option for users to store their passwords 
- lifecycle: Aids developers and users with shared conventions, documentation 
  badges, deprecation warnings as the package develops over time
- data.table: Speeds up data.frame manipulations like reading, writing, 
  aggregations, joins, ordering, sorting
- tidyr:
- dplyr:
- purrr::map2
- gapindex::stitch_entries
- magrittr %>%

Please make sure these _XX_ packages are installed before installing crabpack.

```
devtools::install_github("AFSC-Shellfish-Assessment-Program/crabpack")
```

<!--
## Collaborators
The gapindex R package is a product of two AFSC-RACE-GAP working groups 
regarding GAP data processes and index computation. Many thanks to those who 
participated in those working groups:

**Data Processes Working Group**|**Index Computation Working Group**|**Supervisors**
:-----:|:-----:|:-----:
Alexandra Dowlin (AlexandraDowlin-NOAA)|Zack Oyafuso (zoyafuso-NOAA)*|Stan Kotwicki (StanKotwicki-NOAA)
Emily Markowitz (EmilyMarkowitz-NOAA)|Margaret Siple (MargaretSiple-NOAA)|Duane Stevenson (Duane-Stevenson-NOAA)
Liz Dawson (liz-dawson-NOAA)|Rebecca Haehn (RebeccaHaehn-NOAA)|Ned Laman (Ned-Laman-NOAA)
Sarah Friedman (SarahFriedman-NOAA)|Lukas DeFilippo (Lukas-DeFilippo-NOAA)|Susanne McDermott (smcdermo) 
Christopher Anderson (ChrisAnderson-NOAA)|Paul von Szalay (vszalay)| 
Nancy Roberson (NancyRoberson)|Thaddaeus Buser (ThaddaeusBuser-NOAA)| 
 |*package maintainer| 

## Legacy
Here is an non-exhaustive list of people who provided the foundation for many 
of the functions in this package:

AI-GOA: Michael Martin, Peter Munro, Ned Laman

Bering Sea: REM, Jason Conner, Jerry Hoff, Rebecca Haehn 

Many of the index calculations are from Wakabayashi et al. (1985):

Wakabayashi, K., R. G. Bakkala, and M. S. Alton. 1985. Methods of the 
     U.S.-Japan demersal trawl surveys, p. 7-29. In R. G. Bakkala and K. 
     Wakabayashi (editors), Results of cooperative U.S.-Japan groundfish 
     investigations in the Bering Sea during May-August 1979. Int. North Pac. 
     Fish. Comm. Bull. 44.
-->

## Organization Acronymns
NOAA: National Oceanic and Atmospheric Administration

NMFS: National Marine Fisheries Service

AFSC: Alaska Fisheries Science Center

RACE: Resource Assessment and Conservation Engineering Division

SAP: Shellfish Assessment Program

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), 
or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes 
responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by 
all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, 
or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal 
and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the 
United States Government.
