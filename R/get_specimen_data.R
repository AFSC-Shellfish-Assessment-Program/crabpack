#' Pull AFSC Shellfish Assessment Program Bering Sea survey data
#'
#' @description Pulls haul, specimen, and stratum grouping information for the
#'              region, districts, years, and species of interest from the
#'              CRABBASE schema in the AKFIN Oracle database.
#'
#' @inheritParams calc_bioabund
#' @param channel Connection to Oracle created via `crabpack::get_connected()` or `RODBC::odbcConnect()`.
#'
#' @return A named list containing specimen, haul, stratum, area, and size group
#'         information for the region, districts, years, and species of interest.
#'
#' @export
#'


get_specimen_data <- function(species = NULL,
                              region = c("EBS", "NBS")[1],
                              district = NULL,
                              years = NULL,
                              channel = NULL){


  ## Set up channel if channel = NULL
  if(is.null(x = channel)){
    channel <- crabpack::get_connected()
  }



  ## Clear schema of temporary tables created in this function if present
  for(itable in c("HAUL", "SPECIMEN", "DISTRICT_STRATUM", "STRATUM_STATIONS",
                   "STRATUM_AREA", "STRATUM_DESIGN", "SIZEGROUPS")) {

    # check if temporary table exists and if so...
    if(nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                                 FROM user_tables
                                                 WHERE TABLE_NAME = 'AKFIN_TEMPORARY_",
                                                 itable, "_QUERY'"))) != 0)
      # ...drop the table
      RODBC::sqlQuery(channel = channel,
                      query = paste0("DROP TABLE ", "AKFIN_TEMPORARY_",
                                     itable, "_QUERY"))
  }



  ## Error Query: check that the argument `species` is one of the correct options.
  if(TRUE %in% (!species %in% c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR"))){
    stop(paste0("Argument `species` must contain one or more of these options",
                " (case-sensitive): 'RKC', 'BKC', 'TANNER', 'SNOW', 'HYBRID', or 'HAIR'."))

  }

  ## Issue a warning when choosing multiple species
  if(length(x = species) > 1){
    warning(paste("The crabpack package has only been tested when querying",
                  "one species at a time. Use caution when querying",
                  "multiple species until further testing has been done.",
                  "If you come across an issue, please post it on",
                  "github.com/AFSC-Shellfish-Assessment-Program/crabpack/issues"))
  }

  ## Issue a warning when not specifying a region.
  if(missing(x = region)){
    warning(paste0("The default survey region is the Eastern Bering Sea. Please include",
                   " 'NBS' in the argument `region` if you would like data from the",
                   " Northern Bering Sea."))
  }

  ## Issue a warning when choosing multiple regions
  if(length(x = region) > 1){
    warning(paste("The crabpack package has only been tested when querying",
                  "only one survey region. Use caution when querying",
                  "multiple survey regions until further testing has been done.",
                  "If you come across an issue, please post it on",
                  "github.com/AFSC-Shellfish-Assessment-Program/crabpack/issues"))
  }

  ## Error Query: check that the correct districts are selected for the correct species.
  if(!is.null(district)){

    if(TRUE %in% (district %in% c("166TO173", "E166", "W166") & species != "TANNER")){
      stop(paste0("E166, W166, and 166TO173W districts are only available for Tanner Crab.",
                  " Please verify `district` options for the selected species in the",
                  " CRABBASE.DISTRICT_STRATUM table."))
    }

    if(TRUE %in% (district %in% c("BB", "NORTH") & !species %in% c("RKC", "HAIR"))){
      stop(paste0("Bristol Bay and Northern Unstratified districts are only available",
                  " for Red King Crab and Hair Crab. Please verify `district` options",
                  " for the selected species in the CRABBASE.DISTRICT_STRATUM table."))
    }

    if(TRUE %in% (district %in% c("PRIB") & !species %in% c("RKC", "BKC", "HAIR"))){
      stop(paste0("The Pribilof Islands district is only available for Red King Crab,",
                  " Blue King Crab, and Hair Crab. Please verify `district` options",
                  " for the selected species in the CRABBASE.DISTRICT_STRATUM table."))
    }

    if(TRUE %in% (district %in% c("STMATT", "UNSTRAT") & !species %in% c("BKC"))){
      stop(paste0("The St. Matthew and Unstratified districts are only available for",
                  " Blue King Crab. Please verify `district` options for the selected",
                  " species in the CRABBASE.DISTRICT_STRATUM table."))
    }

    if(TRUE %in% (district %in% c("NS") & !species %in% c("RKC"))){
      stop(paste0("The Norton Sound district is only available for Red King Crab.",
                  " Please verify `district` options for the selected species in",
                  " the CRABBASE.DISTRICT_STRATUM table."))
    }
  }



  ## Concatenate species, years, region, district for use in a SQL query
  species_vec <- paste0("(", paste0(sQuote(x = species, q = FALSE), collapse=", "), ")")
  year_vec <- paste0("(", paste0(years, collapse=", "), ")")
  region_vec <- paste0("(", paste0(sQuote(x = region, q = FALSE), collapse=", "), ")")
  # district_vec <- paste0("(", paste0(sQuote(x = district, q = FALSE), collapse=", "), ")")



  ## Query the haul table. This table houses haul information for all hauls included
  ## in the standard Eastern Bering Sea and Northern Bering Sea bottom trawl time series.
  cat("Pulling haul data...\n")

  haul_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_HAUL_QUERY AS
                    SELECT *
                    FROM CRABBASE.HAUL
                    WHERE YEAR IN", year_vec,
                    " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = haul_sql)

  haul_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                    query = "SELECT * FROM AKFIN_TEMPORARY_HAUL_QUERY"),
                                    key = c("YEAR", "STATION_ID")) # KEY = which columns to sort by
  attributes(x = haul_df)$sql_query <- haul_sql

  # Remove HT 17 if not RKC
  if(species != "RKC"){
    haul_df <- haul_df %>%
               dplyr::filter(!HAUL_TYPE == 17)
  }



  ## Query the specimen data. This table contains all specimen data, subsetted for
  ## standard crab bottom trawl stations in the Eastern Bering Sea and Northern Bering Sea.
  cat("Pulling specimen data...\n")

  specimen_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_SPECIMEN_QUERY AS
                         SELECT *
                         FROM CRABBASE.SPECIMEN
                         WHERE SPECIES IN ", species_vec)

  RODBC::sqlQuery(channel = channel, query = specimen_sql)

  specimen_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                    query = "SELECT * FROM AKFIN_TEMPORARY_SPECIMEN_QUERY"))
  attributes(x = specimen_df)$sql_query <- specimen_sql



  ## Pull relevant lookups/stratum tables
  cat("Pulling district, stratum, and station data...\n")

  ## Query the district and stratum data. This table reports which strata are contained
  ## within a given management district or region.
  district_stratum_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_DISTRICT_STRATUM_QUERY AS
                                SELECT *
                                FROM CRABBASE.DISTRICT_STRATUM
                                WHERE SPECIES IN ", species_vec,
                                " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = district_stratum_sql)

  district_stratum_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                                query = "SELECT * FROM AKFIN_TEMPORARY_DISTRICT_STRATUM_QUERY"))
  attributes(x = district_stratum_df)$sql_query <- district_stratum_sql



  ## Query the stratum design data. This table reports the survey years included
  ## in each year block ID and the corresponding stratum design ID.
  stratum_design_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_STRATUM_DESIGN_QUERY AS
                                SELECT *
                                FROM CRABBASE.STRATUM_DESIGN
                              WHERE REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = stratum_design_sql)

  stratum_design <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                           query = "SELECT * FROM AKFIN_TEMPORARY_STRATUM_DESIGN_QUERY"))
  attributes(x = stratum_design)$sql_query <- stratum_design_sql



  ## Query the stratum station data. This table reports the stations that make up
  ## each stratum for each design ID.
  stratum_stations_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_STRATUM_STATIONS_QUERY AS
                                SELECT *
                                FROM CRABBASE.STRATUM_STATIONS
                                WHERE SPECIES = ", species_vec,
                                "AND REGION IN", region_vec)

  RODBC::sqlQuery(channel = channel, query = stratum_stations_sql)

  stratum_stations_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                                query = "SELECT * FROM AKFIN_TEMPORARY_STRATUM_STATIONS_QUERY"))
  attributes(x = stratum_stations_df)$sql_query <- stratum_stations_sql



  ## Query the stratum area data. This table reports stratum total areas for each
  ## species across distinct year block IDs.
  stratum_area_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_STRATUM_AREA_QUERY AS
                                SELECT *
                                FROM CRABBASE.STRATUM_AREA
                                WHERE SPECIES = ", species_vec,
                                "AND REGION IN", region_vec)

  RODBC::sqlQuery(channel = channel, query = stratum_area_sql)

  stratum_area_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                            query = "SELECT * FROM AKFIN_TEMPORARY_STRATUM_AREA_QUERY"))
  attributes(x = stratum_area_df)$sql_query <- stratum_area_sql



  ## Define districts, stock stations, stratum areas
  # Filter to district, pull relevant strata
  if(is.null(district)){
    strata <- district_stratum_df %>%
              dplyr::filter(DISTRICT == "ALL") %>%
              dplyr::pull(STRATUM)
  } else{
    strata <- district_stratum_df %>%
              dplyr::filter(DISTRICT %in% district) %>%
              dplyr::pull(STRATUM)
  }

  # Pull stock stations using stock districts and haul info; assign stratum and area
  # Assign DESIGN_ID to data based on year
  data_haul <- haul_df %>%
               dplyr::left_join(., stratum_design %>%
                                     dplyr::select(-YEAR_BLOCK_ID) %>%
                                     dplyr::distinct())

  # Add stratum name to each station for the species
  stock_stations <- data_haul %>%
                    dplyr::filter(YEAR %in% years) %>%
                    # add correct stratum names to stations based on design_ID for the given year
                    dplyr::left_join(., stratum_stations_df %>%
                                           dplyr::filter(STRATUM %in% strata)) %>%
                    dplyr::left_join(., stratum_area_df %>%
                                           dplyr::filter(STRATUM %in% strata) %>%
                                           # get relevant years for each area
                                           dplyr::left_join(., stratum_design,
                                                            relationship = "many-to-many") %>%
                                           dplyr::select(STRATUM, TOTAL_AREA, YEAR)) %>%
                    dplyr::select(HAULJOIN, REGION, YEAR, STATION_ID, HAUL_TYPE,
                                  AREA_SWEPT, MID_LATITUDE, MID_LONGITUDE, DISTRICT,
                                  STRATUM, TOTAL_AREA) %>%
                    dplyr::rename(LATITUDE = MID_LATITUDE,
                                  LONGITUDE = MID_LONGITUDE)



  ## Join specimen and stratum information to haul data
  # Add specimen data to relevant hauls for the selected districts/strata
  data_crab <- stock_stations %>%
               dplyr::left_join(., specimen_df) %>%
               # assign unstratified districts for RKC/BKC
               {if(species == "RKC") dplyr::mutate(., DISTRICT = dplyr::case_when(is.na(DISTRICT) ~ "NORTH",
                                                                                  TRUE ~ DISTRICT),
                                                      STRATUM = dplyr::case_when(is.na(STRATUM) ~ "NORTH",
                                                                                 TRUE ~ STRATUM)) else .} %>%
               {if(species == "BKC") dplyr::mutate(., DISTRICT = dplyr::case_when(is.na(DISTRICT) ~ "UNSTRAT",
                                                                                  TRUE ~ DISTRICT),
                                                      STRATUM = dplyr::case_when(is.na(STRATUM) ~ "UNSTRAT",
                                                                                 TRUE ~ STRATUM)) else .}


  # If district is "Northern Unstratified" or "BKC Unstratified",
  # add TOTAL_AREA based on the number of positive catch stations in a given year
  if(TRUE %in% (c("NORTH", "UNSTRAT") %in% strata)){
    n_pos_catch <- data_crab %>%
                   dplyr::filter(!is.na(SEX),
                                 STRATUM %in% c("NORTH", "UNSTRAT")) %>%
                   dplyr::select(HAULJOIN, REGION, YEAR, STATION_ID,
                                 HAUL_TYPE, DISTRICT, STRATUM) %>%
                   dplyr::distinct() %>%
                   dplyr::group_by(YEAR, STRATUM) %>%
                   # ID number positive-catch stations for each year
                   dplyr::summarise(N_POS_STATION = dplyr::n(),
                                    # multiply n_stations by 401 to get TOTAL_AREA
                                    TOTAL_AREA_POS = N_POS_STATION*401) %>%
                   dplyr::select(-N_POS_STATION)

    # Add positive catch station stratum area to haul and specimen info
    data_crab <- data_crab %>%
                  dplyr::group_by(YEAR, STRATUM) %>%
                  dplyr::left_join(., n_pos_catch) %>%
                  dplyr::mutate(TOTAL_AREA = ifelse(TRUE %in% (c("NORTH", "UNSTRAT") %in% STRATUM),
                                                    TOTAL_AREA_POS, TOTAL_AREA)) %>%
                  dplyr::select(-TOTAL_AREA_POS) %>%
                  dplyr::ungroup()

    stock_stations <- stock_stations %>%
                      dplyr::group_by(YEAR, STRATUM) %>%
                      dplyr::left_join(., n_pos_catch) %>%
                      dplyr::mutate(TOTAL_AREA = ifelse(TRUE %in% (c("NORTH", "UNSTRAT") %in% STRATUM),
                                                        TOTAL_AREA_POS, TOTAL_AREA)) %>%
                      dplyr::select(-TOTAL_AREA_POS) %>%
                      dplyr::ungroup()
  }
  ## end goal is the correct stratum name and total area joined to haul and specimen data --> one df
  ## -- works for EBS RKC so far

  ## Format haul data for output, filter region, district, years
  data_haul <- data_haul %>%
               dplyr::left_join(., stock_stations %>%
                                   dplyr::select(-c('LATITUDE', 'LONGITUDE'))) %>%
               dplyr::filter(YEAR %in% years,
                             REGION %in% region) %>%
               {if(!is.null(district)) dplyr::filter(., DISTRICT %in% district) else .}

  ## Format specimen data for output, filter region, district, years
  data_crab <- data_crab %>%
               dplyr::filter(YEAR %in% years,
                             REGION %in% region) %>%
               {if(!is.null(district)) dplyr::filter(., DISTRICT %in% district) else .}



  ## Query the size group table. This table reports size definitions for district-
  ## and species-specific size-sex-maturity categories.
  cat("Pulling crab size group data...\n")

  sizegroups_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_SIZEGROUPS_QUERY AS
                           SELECT *
                           FROM CRABBASE.SIZEGROUPS
                           WHERE SPECIES IN ", species_vec,
                          " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = sizegroups_sql)

  sizegroups_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                          query = "SELECT * FROM AKFIN_TEMPORARY_SIZEGROUPS_QUERY"))
  attributes(x = sizegroups_df)$sql_query <- sizegroups_sql



  ## Clear temporary tables
  cat("Clearing temporary tables...")
  for (itable in c("HAUL", "SPECIMEN", "DISTRICT_STRATUM", "STRATUM_STATIONS",
                   "STRATUM_AREA", "STRATUM_DESIGN", "SIZEGROUPS")) {

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
  return(do.call(what = list,
                 args = list(specimen = data_crab, # specimen data, with relevant haul and stratum info joined (includes 0-catch stations)
                             haul = data_haul, # complete haul data, with stratum info joined
                             sizegroups = sizegroups_df # table with size groupings per district/species
                             )))
}
