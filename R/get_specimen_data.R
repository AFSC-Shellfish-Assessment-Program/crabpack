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

#' Pull AFSC Shellfish Assessment Program Bering Sea survey data
#'
#' @description TBD
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


  ## Set up channel if channel = NULL
  if (is.null(x = channel)){
    channel <- crabPack::get_connected()
  }


  ## Clear schema of temporary tables created in this function if present
  for (itable in c("HAUL", "SPECIMEN", "DISTRICT_STRATUM", "STRATUM_STATIONS",
                   "STRATUM_AREA", "STRATUM_YEAR_DESIGN", "SIZEGROUPS")) {
    ### VECTOR OF NECESSARY TABLE NAMES FOR PULL/JOIN GOES HERE ^^

    ## check if temporary table exists and if so...
    if (nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                                 FROM user_tables
                                                 WHERE TABLE_NAME = 'AKFIN_TEMPORARY_",
                                                 itable, "_QUERY'"))) != 0)
      ## ...drop the table
      RODBC::sqlQuery(channel = channel,
                      query = paste0("DROP TABLE ", "AKFIN_TEMPORARY_",
                                     itable, "_QUERY"))
  }


  ## Error messages...
  # - only select 1 species and region
  # something about year availability? like recommended/earliest years for each species?

  #GAPINDEX EG
  # ## Error Query: check that argument survey_set is one the correct options.
  # if (is.null(x = survey_set) |
  #     !all(survey_set %in% c("GOA", "AI", "EBS", "NBS", "BSS"))) {
  #   stop(paste0("argument `survey_set` must contain one or more of these options",
  #               " (case-sensitive): 'GOA', 'AI', 'EBS', 'BSS', or 'NBS'."))
  # }
  #
  # ## Issue a warning when choosing multiple surveys
  # if (length(x = survey_set) > 1)
  #   warning(paste("The gapindex package has only been tested when querying",
  #                 "only one survey region. Use caution when querying",
  #                 "multiple survey regions until further testing has been done.",
  #                 "If you come across an issue, be sure to post it on",
  #                 "github.com/afsc-gap-products/gapindex/issues"))


  # need to concatenate any vectors...year only in our case I think...

  ## Query the haul table. This table....DESCRIPTION
  cat("Pulling haul data...\n")

  haul_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_HAUL_QUERY AS
                     SELECT *
                     FROM CRABBASE.HAUL
                     WHERE YEAR IN", years,
                     " AND REGION = ", region)

  RODBC::sqlQuery(channel = channel, query = haul_sql)

  haul_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                    query = "SELECT * FROM AKFIN_TEMPORARY_HAUL_QUERY"),
                                    key = c("YEAR", "STATION_ID")) # KEY = which columns to sort by
  attributes(x = haul_df)$sql_query <- haul_sql

  # Remove HT 17 if not RKC
  if(species != "RKC"){
    haul_df <- haul_df %>%
               dplyr::filter(!.data$HAUL_TYPE == 17)
  }


  ## Query the specimen data. This table....DESCRIPTION
  cat("Pulling specimen data...\n")

  specimen_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_SPECIMEN_QUERY AS
                         SELECT *
                         FROM CRABBASE.SPECIMEN
                         WHERE SPECIES = ", species)

  RODBC::sqlQuery(channel = channel, query = specimen_sql)

  specimen_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                    query = "SELECT * FROM AKFIN_TEMPORARY_SPECIMEN_QUERY"))
  attributes(x = specimen_df)$sql_query <- specimen_sql

  # # remove clutch 999 crabs --> WILL ALREADY BE DONE IN PROCESSING
  # specimen_df <- specimen_df %>%
  #                              # make NA clutch into 999 for filtering
  #                dplyr::mutate(.data$CLUTCH_SIZE = ifelse(.data$SEX == 2 & is.na(.data$CLUTCH_SIZE), 999, .data$CLUTCH_SIZE),
  #                              # reassign clutch size 7 -> 6
  #                              .data$CLUTCH_SIZE = ifelse(.data$CLUTCH_SIZE == 7, 6, .data$CLUTCH_SIZE)) %>%
  #                dplyr::filter(# filter females with clutch code 999 or NA
  #                              !(.data$SEX == 2 & .data$CLUTCH_SIZE %in% c(999)))


  ## Pull relevant lookups/stratum tables
  cat("Pulling district, stratum, and station data...\n")

  ## Query the district and stratum data. This table....DESCRIPTION
  # district_stratum_lookup <- CRABBASE.DISTRICT_STRATUM
  district_stratum_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_DISTRICT_STRATUM_QUERY AS
                                SELECT *
                                FROM CRABBASE.DISTRICT_STRATUM
                                WHERE SPECIES = ", species,
                                " AND REGION = ", region)

  RODBC::sqlQuery(channel = channel, query = district_stratum_sql)

  district_stratum_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                        query = "SELECT * FROM AKFIN_TEMPORARY_DISTRICT_STRATUM_QUERY"))
  attributes(x = district_stratum_df)$sql_query <- district_stratum_sql



  ## Query the district and stratum data. This table....DESCRIPTION
  # stratum_year_design <- CRABBASE.STRATUM_YEAR
  stratum_year_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_STRATUM_YEAR_QUERY AS
                                SELECT *
                                FROM CRABBASE.STRATUM_YEAR")

  RODBC::sqlQuery(channel = channel, query = stratum_year_sql)

  stratum_year_design <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                                query = "SELECT * FROM AKFIN_TEMPORARY_STRATUM_YEAR_QUERY"))
  attributes(x = stratum_year_design)$sql_query <- stratum_year_sql



  ## Query the district and stratum data. This table....DESCRIPTION
  # stratum_stations <- CRABBASE.STRATUM_STATIONS
  stratum_stations_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_STRATUM_STATIONS_QUERY AS
                                SELECT *
                                FROM CRABBASE.STRATUM_STATIONS
                                WHERE SPECIES = ", species)

  RODBC::sqlQuery(channel = channel, query = stratum_stations_sql)

  stratum_stations_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                                query = "SELECT * FROM AKFIN_TEMPORARY_STRATUM_STATIONS_QUERY"))
  attributes(x = stratum_stations_df)$sql_query <- stratum_stations_sql



  ## Query the district and stratum data. This table....DESCRIPTION
  # stratum_area <- CRABBASE.STRATUM_AREA
  stratum_area_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_STRATUM_AREA_QUERY AS
                                SELECT *
                                FROM CRABBASE.STRATUM_AREA
                                WHERE SPECIES = ", species)

  RODBC::sqlQuery(channel = channel, query = stratum_area_sql)

  stratum_area_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                            query = "SELECT * FROM AKFIN_TEMPORARY_STRATUM_AREA_QUERY"))
  attributes(x = stratum_area_df)$sql_query <- stratum_area_sql


# ## THIS WILL ALREADY BE DONE
#   ## LOOK @ JON's SQL scripts for making CrabHaul files to help with species-specific filtering??
#   data_crab <- data_specimen %>%
#                # join species names
#                dplyr::left_join(., district_stratum_lookup %>%
#                                 dplyr::select(.data$SPECIES_CODE, .data$SPECIES) %>%
#                                 dplyr::distinct())


  ## Define districts, stock stations, stratum areas -------------------------
  # Pull strata by stock specified. If no stock/district specified, default EBS for the species
  # #filter to region --> region never null! default to EBS...
  # if(is.null(region)){
  #
  #
  # }

  #filter to district --> get the strata you need
  if(is.null(district)){
    strata <- district_stratum_df %>%
              dplyr::filter(#.data$SPECIES == species,
                            # .data$REGION == "EBS",
                            .data$DISTRICT == "ALL") %>%
              dplyr::pull(.data$STRATUM)
  } else{
    strata <- district_stratum_df %>%
              dplyr::filter(#.data$SPECIES == species,
                            .data$DISTRICT == district) %>% ## make this %in% to accommodate multiple districts?? leave for now
              dplyr::pull(.data$STRATUM)
  }



  # Pull stock stations from strata tables using stock districts and haul info; assign stratum and area
  # Assign DESIGN_ID to data based off year
  data_haul <- data_haul %>%
               dplyr::left_join(.data$., stratum_year_design %>%
                                     dplyr::select(-.data$YEAR_BLOCK_ID) %>%
                                     dplyr::distinct())

  ## ** I DON'T KNOW IF THIS PORTION CAN BE DONE IN SQL.....
  # Add stratum name to each station for the species
  stock_stations <- data_haul %>%
                    dplyr::filter(.data$SURVEY_YEAR %in% years) %>%
                    dplyr::rename(STATION_ID = .data$GIS_STATION) %>%
                    # add correct stratum names to stations based on design_ID for the given year
                    dplyr::left_join(.data$., stratum_stations_df %>%
                                           dplyr::filter(#.data$SPECIES == species,
                                                         .data$STRATUM %in% strata) %>%
                                           dplyr::select(-c('SPECIES_CODE', 'DISTRICT_NAME', 'STRATUM_NAME'))) %>%
                    dplyr::left_join(.data$., stratum_area_df %>%
                                           dplyr::filter(#.data$SPECIES == species,
                                                         .data$STRATUM %in% strata) %>%
                                           # get relevant years for each area
                                           dplyr::left_join(., stratum_year_design, relationship = "many-to-many") %>%
                                           dplyr::select(.data$STRATUM, .data$TOTAL_AREA, .data$SURVEY_YEAR)) %>%
                    dplyr::select(.data$HAULJOIN, .data$SURVEY_YEAR, .data$STATION_ID, .data$HAUL_TYPE,
                                  .data$AREA_SWEPT, .data$MID_LATITUDE, .data$MID_LONGITUDE, .data$DISTRICT,
                                  .data$STRATUM, .data$TOTAL_AREA) %>%
                    dplyr::rename(LATITUDE = .data$MID_LATITUDE,
                                  LONGITUDE = .data$MID_LONGITUDE)

  # # Remove HT 17 if not RKC --> DONE WHEN PULLING HAUL TABLE now...
  # ## -- maybe do above --> join haul and specimen, subset to species of interest, then move to R for stratum stuff....
  # ## or filter species at least, can join to haul later?
  # if(species != "RKC"){
  #   stock_stations <- stock_stations %>%
  #                     dplyr::filter(!.data$HAUL_TYPE == 17)
  # }


  ## Join to haul data -------------------------------------------------------
  # Add specimen data to relevant hauls for the selected districts/strata
  data_crab2 <- stock_stations %>%
                dplyr::left_join(.data$., specimen_df)

  # If district is "Northern Unstratified" or "BKC Unstratified", add TOTAL_AREA
  # based on the number of positive catch stations in a given year
  if(TRUE %in% (c("NORTH", "UNSTRAT") %in% strata)){
    n_pos_catch <- data_crab2 %>%
                   dplyr::filter(!is.na(.data$SEX),
                                 .data$STRATUM %in% c("NORTH", "UNSTRAT")) %>%
                   dplyr::select(.data$HAULJOIN, .data$SURVEY_YEAR, .data$STATION_ID,
                                 .data$HAUL_TYPE, .data$DISTRICT, .data$STRATUM) %>%
                   dplyr::distinct() %>%
                   dplyr::group_by(.data$SURVEY_YEAR, .data$STRATUM) %>%
                   # ID number positive-catch stations for each year
                   dplyr::summarise(N_POS_STATION = dplyr::n(),
                                    # multiply n_stations by 401 to get TOTAL_AREA
                                    TOTAL_AREA_POS = .data$N_POS_STATION*401) %>%
                   dplyr::select(-.data$N_POS_STATION)

    # Add positive catch station stratum area to haul and specimen info
    data_crab2 <- data_crab2 %>%
                  dplyr::group_by(.data$SURVEY_YEAR, .data$STRATUM) %>%
                  dplyr::left_join(.data$., n_pos_catch) %>%
                  dplyr::mutate(TOTAL_AREA = ifelse(TRUE %in% (c("NORTH", "UNSTRAT") %in% .data$STRATUM),
                                                    .data$TOTAL_AREA_POS, .data$TOTAL_AREA)) %>%
                  dplyr::select(-.data$TOTAL_AREA_POS) %>%
                  dplyr::ungroup()
  }
  ## end goal is the correct stratum name and total area joined to haul and specimen data --> one df
  ## -- works for EBS RKC so far



  ## Pull size groups definition lookup
  cat("Pulling crab size group data...\n")

  ## Query the size group table. This table....DESCRIPTION
  sizegroups_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_SIZEGROUPS_QUERY AS
                           SELECT *
                           FROM CRABBASE.SIZEGROUPS
                           WHERE SPECIES = ", species,
                          " AND REGION = ", region)

  RODBC::sqlQuery(channel = channel, query = sizegroups_sql)

  sizegroups_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                          query = "SELECT * FROM AKFIN_TEMPORARY_SIZEGROUPS_QUERY"))
  attributes(x = sizegroups_df)$sql_query <- sizegroups_sql



  ## Clear temporary tables
  cat("Clearing temporary tables...")
  for (itable in c("HAUL", "SPECIMEN", "DISTRICT_STRATUM", "STRATUM_STATIONS",
                   "STRATUM_AREA", "STRATUM_YEAR_DESIGN", "SIZEGROUPS")) {

    if (nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                   FROM user_tables
                                   WHERE TABLE_NAME = 'AKFIN_TEMPORARY_",
                                                itable, "_QUERY'"))) != 0)
      RODBC::sqlQuery(channel = channel,
                      query = paste0("DROP TABLE ", "AKFIN_TEMPORARY_",
                                     itable, "_QUERY"))
  }

  cat("Finished.\n")



  ## Collate data into a list and return
  # specify all the tables here!! essentially this is 'crab_data', and can unpack the relevant lookups from this list
  return(do.call(what = list,
                 args = list(specimen = data_crab2, # specimen data, with haul and stratum info joined
                             sizegroups = sizegroups_df, # table with size groupings per district/species
                             stock_stations = stock_stations, ## this could maybe replace the lookups below?
                             # and/or could recreate by subsetting the haul/stratum info from the joined specimen data??
                             district_stratum = district_stratum_df, # table with stratum definitions for each district
                             stratum_area = stratum_area_df, # number of stations/area per stratum
                             stratum_stations = stratum_stations_df, # stratum station IDs
                             stratum_year = stratum_year_design # stratum years and design IDs
                             # other lookups for egg/clutch codes?
                             )))
}
