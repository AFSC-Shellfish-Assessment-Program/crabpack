#' Pull AFSC Shellfish Assessment Program Bering Sea survey data
#'
#' @description Pulls haul, specimen, and stratum grouping information for the
#'              region, districts, years, and species of interest from the
#'              CRABBASE schema in the AKFIN Oracle database.
#'
#' @inheritParams calc_bioabund
#' @param channel Character string or Oracle connection. Defaults to an API connection,
#'                (`channel = "API"`), allowing for public data access. To use an
#'                Oracle database connection, set `channel` to an object created
#'                via `crabpack::get_connected()` or `DBI::dbConnect()`. Local AFSC
#'                Kodiak users can also set `channel = "KOD"` to access data on the
#'                network drives (requires VPN connection). This option will pull all
#'                available years and districts for the given species and region.
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


  ## Set channel to default to API access if channel = NULL
  if(is.null(x = channel)){
    channel <- "API"
  }


  ## Error Query: check that the argument `species` is one of the correct options.
  if(TRUE %in% (!species %in% c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR"))){
    stop(paste0("Argument `species` must contain one or more of these options",
                " (case-sensitive): 'RKC', 'BKC', 'TANNER', 'SNOW', 'HYBRID', or 'HAIR'."))

  }

  ## Error when choosing multiple species
  if(length(x = species) > 1){
    stop(paste0("The crabpack package is designed to only query one species at",
                " a time. Please limit your query to just one species."))
  }

  ## Issue a warning when not specifying a region.
  if(missing(x = region)){
    message(paste0("The default survey region is the Eastern Bering Sea. Please include",
                   " 'NBS' in the argument `region` if you would like data from the",
                   " Northern Bering Sea."))
  }

  ## Issue a warning when choosing multiple regions
  if(length(x = region) > 1){
    warning(paste0("The crabpack package has only been tested when querying",
                   " only one survey region. Use caution when querying",
                   " multiple survey regions until further testing has been done.",
                   " If you come across an issue, please post it on",
                   " github.com/AFSC-Shellfish-Assessment-Program/crabpack/issues"))
  }

  ## Error Query: check that the correct districts are selected for the correct species.
  if(!is.null(district)){

    if(TRUE %in% (district %in% c("166TO173", "E166", "W166") & species != "TANNER")){
      stop(paste0("E166, W166, and 166TO173W districts are only available for Tanner Crab.",
                  " Please verify `district` options for the selected species in the",
                  " CRABBASE.DISTRICT_STRATUM table."))
    }

    if(TRUE %in% (district %in% c("166TO173") & length(district > 1))){
      stop(paste0("The 166TO173W district is a special case area for Tanner Crab, and",
                  " should not be pulled in conjunction with any other districts due to",
                  " spatial overlap with W166. Please set `district` to just `166TO173`",
                  " if you wish to use those data."))
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


  # define year if not specified
  if(missing(years)){
    years <- c(1975:2030) # put dummy end year to accommodate future years?
  }



  if(inherits(channel, "character")){
    # Set special local Kodiak connection option
    if(channel == "KOD"){
      path <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Outputs/"
      specimen_rds <- tryCatch(expr = suppressWarnings(readRDS(paste0(path, species, "_specimen_", region, ".rds"))),
                               error = function(cond) {
                                 stop("Unable to connect. Please check that you are connected to the network (e.g., VPN) and re-try. \n\n")
                                 return(invisible())})

      return(specimen_rds)
    }


    # Set API connection option
    if(channel == "API"){
      api_url <- "https://apex.psmfc.org/akfin/data_marts/crabbase/"

      # set years to filter by
      start_year <- min(years)
      end_year <- max(years)


      ## Query the haul table. This table houses haul information for all hauls included
      ## in the standard Eastern Bering Sea and Northern Bering Sea bottom trawl time series.
      cat("Pulling haul data...\n")
      haul_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "haul?"),
                                                            query = list(region = paste(region, collapse = ","),
                                                                         start_year = start_year,
                                                                         end_year = end_year)),
                                                  type = "text", encoding = "UTF-8")) %>%
                 dplyr::bind_rows() %>%
                 dplyr::arrange(REGION, YEAR, STATION_ID)

      ## Query the specimen data. This table contains all specimen data, subsetted for
      ## standard crab bottom trawl stations in the Eastern Bering Sea and Northern Bering Sea.
      cat("Pulling specimen data...\n")
      specimen_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "specimen?"),
                                                                query = list(species = species)),
                                                      type = "text", encoding = "UTF-8")) %>%
                     dplyr::bind_rows() %>%
                     dplyr::rename_with(toupper)


      ## Pull relevant lookups/stratum tables
      cat("Pulling district, stratum, and station data...\n")

      ## Query the district and stratum data. This table reports which strata are contained
      ## within a given management district or region.
      district_stratum_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "district_stratum?"),
                                                                        query = list(region = paste(region, collapse = ","),
                                                                                     species = species)),
                                                              type = "text", encoding = "UTF-8")) %>%
                             dplyr::bind_rows()

      ## Query the stratum design data. This table reports the survey years included
      ## in each year block ID and the corresponding stratum design ID.
      stratum_design_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "stratum_design?"),
                                                                      query = list(region = paste(region, collapse = ","))),
                                                            type = "text", encoding = "UTF-8")) %>%
                           dplyr::bind_rows()

      ## Query the stratum station data. This table reports the stations that make up
      ## each stratum for each design ID.
      stratum_stations_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "stratum_stations?"),
                                                                        query = list(region = paste(region, collapse=","),
                                                                                     species = species)),
                                                              type = "text", encoding = "UTF-8")) %>%
                             dplyr::bind_rows()

      ## Query the stratum area data. This table reports stratum total areas for each
      ## species across distinct year block IDs.
      stratum_area_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "stratum_area?"),
                                                                    query = list(region = paste(region, collapse = ","),
                                                                                 species = species)),
                                                          type = "text", encoding = "UTF-8")) %>%
                         dplyr::bind_rows()

      ## Query the size group table. This table reports size definitions for district-
      ## and species-specific size-sex-maturity categories.
      cat("Pulling crab size group data...\n")
      sizegroups_df <- jsonlite::fromJSON(httr::content(httr::GET(url = paste0(api_url, "sizegroups?"),
                                                                  query = list(region = paste(region, collapse = ","),
                                                                               species = species)),
                                                        type = "text", encoding = "UTF-8")) %>%
                       dplyr::bind_rows()
    }
  }



  # Pull via Oracle database connection
  if(inherits(channel, "Oracle")){
    ## Concatenate species, years, region, district for use in a SQL query
    species_vec <- paste0("(", paste0(sQuote(x = species, q = FALSE), collapse = ", "), ")")
    year_vec <- paste0("(", paste0(years, collapse = ", "), ")")
    region_vec <- paste0("(", paste0(sQuote(x = region, q = FALSE), collapse = ", "), ")")


    ## Query the haul table. This table houses haul information for all hauls included
    ## in the standard Eastern Bering Sea and Northern Bering Sea bottom trawl time series.
    cat("Pulling haul data...\n")
    haul_df <- data.table::data.table(
                suppressWarnings(
                  DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                statement = paste0("SELECT * FROM CRABBASE.HAUL WHERE YEAR IN ",
                                                                   year_vec, " AND REGION IN ", region_vec)))),
                key = c("REGION", "YEAR", "STATION_ID")) # KEY = which columns to sort by

    ## Query the specimen data. This table contains all specimen data, subsetted for
    ## standard crab bottom trawl stations in the Eastern Bering Sea and Northern Bering Sea.
    cat("Pulling specimen data...\n")
    specimen_df <- data.table::data.table(
                    suppressWarnings(
                      DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                    statement = paste0("SELECT * FROM CRABBASE.SPECIMEN WHERE SPECIES IN ",
                                                                       species_vec)))))

    ## Pull relevant lookups/stratum tables
    cat("Pulling district, stratum, and station data...\n")

    ## Query the district and stratum data. This table reports which strata are contained
    ## within a given management district or region.
    district_stratum_df <- data.table::data.table(
                            suppressWarnings(
                              DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                            statement = paste0("SELECT * FROM CRABBASE.DISTRICT_STRATUM WHERE SPECIES IN ",
                                                                               species_vec, " AND REGION IN ", region_vec)))))

    ## Query the stratum design data. This table reports the survey years included
    ## in each year block ID and the corresponding stratum design ID.
    stratum_design_df <- data.table::data.table(
                          suppressWarnings(
                            DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                          statement = paste0("SELECT * FROM CRABBASE.STRATUM_DESIGN WHERE REGION IN ",
                                                                             region_vec)))))

    ## Query the stratum station data. This table reports the stations that make up
    ## each stratum for each design ID.
    stratum_stations_df <- data.table::data.table(
                            suppressWarnings(
                              DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                            statement = paste0("SELECT * FROM CRABBASE.STRATUM_STATIONS WHERE SPECIES IN ",
                                                                               species_vec, " AND REGION IN ", region_vec)))))

    ## Query the stratum area data. This table reports stratum total areas for each
    ## species across distinct year block IDs.
    stratum_area_df <- data.table::data.table(
                        suppressWarnings(
                          DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                        statement = paste0("SELECT * FROM CRABBASE.STRATUM_AREA WHERE SPECIES IN ",
                                                                           species_vec, " AND REGION IN ", region_vec)))))

    ## Query the size group table. This table reports size definitions for district-
    ## and species-specific size-sex-maturity categories.
    cat("Pulling crab size group data...\n")
    sizegroups_df <- data.table::data.table(
                      suppressWarnings(
                        DBI::dbFetch(DBI::dbSendQuery(conn = channel,
                                                      statement = paste0("SELECT * FROM CRABBASE.SIZEGROUPS WHERE SPECIES IN ",
                                                                         species_vec, " AND REGION IN ", region_vec)))))
  }



  # If pulling data from AKFIN/API, remove "AKFIN_LOAD_DATE" column - messes with joining
  if("AKFIN_LOAD_DATE" %in% names(haul_df)){
    haul_df <- haul_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
    specimen_df <- specimen_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
    district_stratum_df <- district_stratum_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
    stratum_stations_df <- stratum_stations_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
    stratum_design_df <- stratum_design_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
    stratum_area_df <- stratum_area_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
    sizegroups_df <- sizegroups_df %>% dplyr::select(-"AKFIN_LOAD_DATE")
  }


  # Remove HT 17 if not RKC
  if(species != "RKC"){
    haul_df <- haul_df %>%
               dplyr::filter(!HAUL_TYPE == 17)
  }


  ## Define districts, stock stations, stratum areas
  # Filter to district, pull relevant strata
  if(is.null(district)){
    strata <- district_stratum_df %>%
              dplyr::filter(SPECIES == species,
                            DISTRICT == "ALL") %>%
              dplyr::pull(STRATUM)

    # If Tanner and no district specified, remove 166TO173
    if(species == "TANNER"){
      stratum_stations_df <- stratum_stations_df %>%
                             dplyr::filter(SPECIES == species,
                                           !DISTRICT == "166TO173")
    }

  } else{
    strata <- district_stratum_df %>%
              dplyr::filter(SPECIES == species,
                            DISTRICT %in% district) %>%
              dplyr::pull(STRATUM)
  }


  # Pull stock stations using stock districts and haul info; assign stratum and area
  cat("Joining district, stratum, and area information to specimen data...\n")
  # Assign DESIGN_ID to data based on year
  data_haul <- haul_df %>%
               dplyr::left_join(., stratum_design_df %>%
                                  dplyr::select(-YEAR_BLOCK_ID) %>%
                                  dplyr::distinct(),
                                by = c('REGION', 'YEAR'))

  # Add stratum name to each station for the species
  stock_stations <- data_haul %>%
                    dplyr::filter(YEAR %in% years) %>%
                    # add correct stratum names to stations based on design_ID for the given year
                    dplyr::left_join(., stratum_stations_df,
                                     by = c('REGION', 'STATION_ID', 'DESIGN_ID')) %>%
                    dplyr::left_join(., stratum_area_df %>%
                                        dplyr::filter(STRATUM %in% strata) %>%
                                        # get relevant years for each area
                                        dplyr::left_join(., stratum_design_df,
                                                         relationship = "many-to-many",
                                                         by = c('REGION', 'YEAR_BLOCK_ID')) %>%
                                        dplyr::select(STRATUM, TOTAL_AREA, YEAR),
                                     by = c('YEAR', 'STRATUM')) %>%
                    dplyr::select(HAULJOIN, REGION, YEAR, STATION_ID, HAUL_TYPE,
                                  AREA_SWEPT, MID_LATITUDE, MID_LONGITUDE, DISTRICT,
                                  STRATUM, TOTAL_AREA) %>%
                    dplyr::rename(LATITUDE = MID_LATITUDE,
                                  LONGITUDE = MID_LONGITUDE) %>%
                    # assign unstratified districts for RKC/BKC
                    {if(species == "RKC") dplyr::mutate(., DISTRICT = dplyr::case_when(is.na(DISTRICT) ~ "NORTH",
                                                                                       TRUE ~ DISTRICT),
                                                        STRATUM = dplyr::case_when(is.na(STRATUM) ~ "NORTH",
                                                                                   TRUE ~ STRATUM)) else .} %>%
                    {if(species == "BKC") dplyr::mutate(., DISTRICT = dplyr::case_when(is.na(DISTRICT) ~ "UNSTRAT",
                                                                                       TRUE ~ DISTRICT),
                                                        STRATUM = dplyr::case_when(is.na(STRATUM) ~ "UNSTRAT",
                                                                                   TRUE ~ STRATUM)) else .} %>%
                    dplyr::filter(STRATUM %in% strata)



  ## Join specimen and stratum information to haul data
  # Add specimen data to relevant hauls for the selected districts/strata
  data_crab <- stock_stations %>%
               dplyr::left_join(., specimen_df, by = c('HAULJOIN'))



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
                 dplyr::left_join(., n_pos_catch, by = c('YEAR', 'STRATUM')) %>%
                 dplyr::mutate(TOTAL_AREA = ifelse(TRUE %in% (c("NORTH", "UNSTRAT") %in% STRATUM),
                                                   TOTAL_AREA_POS, TOTAL_AREA)) %>%
                 dplyr::select(-TOTAL_AREA_POS) %>%
                 dplyr::ungroup() %>%
                 dplyr::mutate(TOTAL_AREA = ifelse(is.na(TOTAL_AREA), 0, TOTAL_AREA))

    stock_stations <- stock_stations %>%
                      dplyr::group_by(YEAR, STRATUM) %>%
                      dplyr::left_join(., n_pos_catch, by = c('YEAR', 'STRATUM')) %>%
                      dplyr::mutate(TOTAL_AREA = ifelse(TRUE %in% (c("NORTH", "UNSTRAT") %in% STRATUM),
                                                        TOTAL_AREA_POS, TOTAL_AREA)) %>%
                      dplyr::select(-TOTAL_AREA_POS) %>%
                      dplyr::ungroup() %>%
                      dplyr::mutate(TOTAL_AREA = ifelse(is.na(TOTAL_AREA), 0, TOTAL_AREA))
  }


  ## Format haul data for output, filter region, district, years
  data_haul <- data_haul %>%
               dplyr::left_join(., stock_stations %>%
                                  dplyr::select(-c('LATITUDE', 'LONGITUDE')),
                                by = c('HAULJOIN', 'REGION', 'YEAR', 'HAUL_TYPE', 'AREA_SWEPT', 'STATION_ID')) %>%
               dplyr::filter(YEAR %in% years,
                             REGION %in% region,
                             STRATUM %in% strata)

  ## Format specimen data for output, filter region, district, years
  data_crab <- data_crab %>%
               dplyr::filter(YEAR %in% years,
                             REGION %in% region,
                             !SEX == 3,
                             STRATUM %in% strata)



  ## Collate data into a list and return
  return(do.call(what = list,
                 args = list(specimen = data_crab, # specimen data, with relevant haul and stratum info joined (includes 0-catch stations)
                             haul = data_haul, # complete haul data, with stratum info joined
                             sizegroups = sizegroups_df # table with size groupings per district/species
                 )))
}
