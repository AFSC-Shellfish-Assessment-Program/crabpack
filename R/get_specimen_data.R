### JOIN HAUL, STRATUM, and SPECIMEN DATA ---------------------------

## function to join stratum/area info with haul and specimen data
# -- need to ID pos-catch stations for each species...
# -- need to filter out HT17 for everyone but RKC
# -- return just one DF with haul/stratum/specimen all joined together

## INPUTS: data_haul, data_specimen, species and/or stock, years....
## dependencies: tidyverse, lookups....specify that you need to have them loaded? Or give script on loading so naming works within function?
## -- can we just make sure they're loaded into environment (with the right names) and it'll be fine?

# # Set file paths
# path <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Data/")
# out_dir <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Test/Outputs/"
#
# # Load data, set function inputs for testing
# data_haul <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Test/Data/haul_table.csv")
# data_specimen <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Test/Data/specimen_table.csv")
# species <- "Red King Crab"
# district <- "EBS" # if(missing), district default EBS?
# years <- 2024


#' @description
#'
#' @inheritParams calc_bioabund
#' @param channel connection to Oracle created via crabPack::get_connected() or RODBC::odbcConnect().
#'                'channel = "local"' is to be used for internal development purposes only.
#'
#' @return a named list containing survey, cruise, haul, catch, size, specimen,
#'         stratum, subarea, and stratum_groups information for the years and species of interest.
#'
#' @export get_specimen_data
#'

get_specimen_data <- function(species = NULL,
                     region = c("EBS", "NBS")[1],
                     district = NULL,
                     years = NULL,
                     channel = NULL){


  if (is.null(x = channel)){
    channel <- crabPack::get_connected()
  }

  ## Not totally sure what this part is doing, TBD....
  for (itable in c("AVAIL_SPP", "CATCH", "CRUISE", "HAUL", "INPUT_SPP",
                   "SIZE", "SPECIMEN", "STRATUM", "STRATUM_GROUPS", "SURVEY",
                   "SURVEY_DESIGN", "SUBAREA", "USER_TAXONOMIC_INFO",
                   "UNAVAIL_SPP", "USER_INPUT_SPP")) {

    ## check if temporary table exists and if so...
    if (nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                                 FROM user_tables
                                                 WHERE TABLE_NAME = 'CRABBASE_TEMPORARY_",
                                                 itable, "_QUERY'"))) != 0)
      ## ...drop the table
      RODBC::sqlQuery(channel = channel,
                      query = paste0("DROP TABLE ", "CRABBASE_TEMPORARY_",
                                     itable, "_QUERY"))
  }





  ## pull specimen and haul tables
  ## Pull relevant lookups/stratum tables


## THIS WILL ALREADY BE DONE
  ## LOOK @ JON's SQL sccripts for making CrabHaul files to help with species-specific filtering??
  # ## Filter to species of interest, remove clutch 999 crabs
  # data_crab <- data_specimen %>%
  #              # join species names
  #              left_join(., district_stratum_lookup %>%
  #                          select(SPECIES_CODE, SPECIES_NAME) %>%
  #                          distinct()) %>%
  #              # make NA clutch into 999 for filtering
  #              mutate(CLUTCH_SIZE = ifelse(SEX == 2 & is.na(CLUTCH_SIZE), 999, CLUTCH_SIZE)) %>%
  #              dplyr::filter(# filter females with clutch code 999 or NA
  #                !(SEX == 2 & CLUTCH_SIZE %in% c(999)),
  #                # filter to species
  #                SPECIES_NAME == species) %>%
  #              # reassign clutch size 7 -> 6
  #              dplyr::mutate(CLUTCH_SIZE = ifelse(CLUTCH_SIZE == 7, 6, CLUTCH_SIZE))


  ## Define districts, stock stations, stratum areas -------------------------
  # Pull strata by stock specified. If no stock/district specified, default EBS for the species
  if(missing(district)){
    strata <- district_stratum_lookup %>%
              dplyr::filter(SPECIES_NAME == species,
                            !DISTRICT_CODE %in% c("NBS", "NS")) %>%
              pull(STRATUM_CODE)
  } else{
    strata <- district_stratum_lookup %>%
              dplyr::filter(SPECIES_NAME == species) %>%
              pull(STRATUM_CODE)
  }


  # Pull stock stations from strata tables using stock districts and haul info; assign stratum and area
  # Assign DESIGN_ID to data based off year
  data_haul <- data_haul %>%
               left_join(., stratum_year_design %>%
                              select(-YEAR_BLOCK_ID) %>%
                              distinct())

  # Add stratum name to each station for the species
  stock_stations <- data_haul %>%
                    dplyr::filter(SURVEY_YEAR %in% years) %>%
                    rename(STATION_ID = GIS_STATION) %>%
                    # add correct stratum names to stations based on design_ID for the given year
                    left_join(., stratum_stations_lookup %>%
                                dplyr::filter(SPECIES_NAME == species,
                                              STRATUM_CODE %in% strata) %>%
                                select(-c('SPECIES_CODE', 'STOCK', 'DISTRICT_NAME', 'STRATUM_NAME'))) %>%
                    left_join(., stratum_nstations %>%
                                dplyr::filter(SPECIES_NAME == species,
                                              STRATUM_CODE %in% strata) %>%
                                # get relevant years for each area
                                left_join(., stratum_year_design, relationship = "many-to-many") %>%
                                dplyr::select(STRATUM_CODE, TOTAL_AREA, SURVEY_YEAR)) %>%
                    select(HAULJOIN, SURVEY_YEAR, STATION_ID, HAUL_TYPE, AREA_SWEPT, MID_LATITUDE,
                           MID_LONGITUDE, DISTRICT_CODE, STRATUM_CODE, TOTAL_AREA) %>%
                    rename(LATITUDE = MID_LATITUDE,
                           LONGITUDE = MID_LONGITUDE)

  # Remove HT 17 if not RKC
  if(species != "RKC"){
    stock_stations <- stock_stations %>%
                      filter(!HAUL_TYPE == 17)
  }


  ## Join to haul data -------------------------------------------------------
  # Add specimen data to relevant hauls for the selected districts/strata
  data_crab2 <- stock_stations %>%
                left_join(., data_crab)

  # If district is "Northern Unstratified" or "BKC Unstratified", add TOTAL_AREA
  # based on the number of positive catch stations in a given year
  if(TRUE %in% (c("NORTH", "UNSTRAT") %in% strata)){
    n_pos_catch <- data_crab2 %>%
                   filter(!is.na(SEX),
                          STRATUM_CODE %in% c("NORTH", "UNSTRAT")) %>%
                   select(HAULJOIN, SURVEY_YEAR, STATION_ID, HAUL_TYPE, DISTRICT_CODE, STRATUM_CODE) %>%
                   distinct() %>%
                   group_by(SURVEY_YEAR, STRATUM_CODE) %>%
                   # ID number positive-catch stations for each year
                   summarise(N_POS_STATION = n(),
                             # multiply n_stations by 401 to get TOTAL_AREA
                             TOTAL_AREA_POS = N_POS_STATION*401) %>%
                   select(-N_POS_STATION)

    # Add positive catch station stratum area to haul and specimen info
    data_crab2 <- data_crab2 %>%
                  group_by(SURVEY_YEAR, STRATUM_CODE) %>%
                  left_join(., n_pos_catch) %>%
                  mutate(TOTAL_AREA = ifelse(TRUE %in% (c("NORTH", "UNSTRAT") %in% STRATUM_CODE),
                                             TOTAL_AREA_POS, TOTAL_AREA)) %>%
                  select(-TOTAL_AREA_POS) %>%
                  ungroup
  }
  ## end goal is the correct stratum name and total area joined to haul and specimen data --> one df
  ## -- works for EBS RKC so far

  return(data_crab2)
}
