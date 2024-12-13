#' Pull proportion morphometrically mature Chionoecetes spp. males by size
#'
#' @description A time series of model estimated proportion of morphometrically mature
#'              male Tanner Crab and Snow Crab by 10 millimeter size bin for a given
#'              region or district, based on chela height measurements. Yearly model
#'              parameter estimates of 50% probability of maturity at size are also
#'              provided.
#'
#' @param species Character string. One of `c("TANNER", "SNOW")`.
#' @param district Character string. One or many of `c("ALL", "E166", "W166")`. Defaults to
#'                 `"ALL"`; `"E166"` and `"W166"` are to be used for Tanner Crab only.
#' @inheritParams get_specimen_data
#'
#' @return A named list containing the proportion of male Chionoecetes spp. crab
#'         that are morphometrically mature in a given 10mm size bin, and yearly
#'         model parameter estimates for 50% probability of maturity at size for
#'         the species, region, and district of interest.
#'
#' @export
#'


get_male_maturity <- function(species = NULL,
                              region = c("EBS", "NBS")[1],
                              district = NULL,
                              channel = NULL){

  ## Set up channel if channel = NULL
  if(is.null(x = channel)){
    channel <- crabpack::get_connected()
  }


  ## Clear schema of temporary tables created in this function if present
  for(itable in c("CHIONOECETES_MAT_RATIO", "CHIONOECETES_MATMODEL_PARAMS")) {

    ## check if temporary table exists and if so...
    if(nrow(x = RODBC::sqlQuery(channel = channel,
                                 query = paste0("SELECT table_name
                                                 FROM user_tables
                                                 WHERE TABLE_NAME = 'AKFIN_TEMPORARY_",
                                                itable, "_QUERY'"))) != 0)
      ## ...drop the table
      RODBC::sqlQuery(channel = channel,
                      query = paste0("DROP TABLE ", "AKFIN_TEMPORARY_",
                                     itable, "_QUERY"))
  }


  ## Error Query: check that the argument `species` is one of the correct options.
  if(TRUE %in% (!species %in% c("TANNER", "SNOW"))){
    stop(paste0("Male chela-based maturity metrics are only available for Tanner",
                " and Snow Crab. Argument `species` must contain one or more of these options",
                " (case-sensitive): 'TANNER' or 'SNOW'."))

  }


  ## Issue a warning when not specifying a region.
  if(missing(x = region)){
    warning(paste0("The default survey region is the Eastern Bering Sea. Please include",
                  " 'NBS' in the argument `region` if you would like data from the",
                  " Northern Bering Sea."))
  }


  ## Error Query: check that the districts E166 or W166 are only included for the correct species.
  if(!is.null(district)){
    if(TRUE %in% (district %in% c("E166", "W166") & species != "TANNER")){
      stop(paste0("E166 and W166 districts are only available for Tanner Crab.",
                  " Please remove the argument `district` if querying Snow Crab."))
    }
  }


  ## Concatenate years, region for use in a SQL query
  species_vec <- paste0("(", paste0(sQuote(x = species, q = FALSE), collapse=", "), ")")
  region_vec <- paste0("(", paste0(sQuote(x = region, q = FALSE), collapse=", "), ")")



  ## Query the Chionoecetes male probability of maturity at size table. This table
  ## contains the proportion mature male Chionoecetes spp. by 10 millimeter size bin
  ## for the Eastern Bering Sea and Northern Bering Sea.
  cat("Pulling Chionoecetes maturity ratio data...\n")

  mat_ratio_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_CHIONOECETES_MAT_RATIO_QUERY AS
                        SELECT *
                        FROM CRABBASE.CHIONOECETES_MAT_RATIO
                        WHERE SPECIES IN ", species_vec,
                      " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = mat_ratio_sql)

  mat_ratio_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                         query = "SELECT * FROM AKFIN_TEMPORARY_CHIONOECETES_MAT_RATIO_QUERY"),
                                         key = c("SPECIES", "REGION", "DISTRICT", "YEAR", "SIZE_BIN")) # KEY = which columns to sort by
  attributes(x = ogives_df)$sql_query <- mat_ratio_sql

  # further filter by district if specified
  if(!is.null(district)){
    mat_ratio_df <- mat_ratio_df %>%
                    dplyr::filter(DISTRICT %in% district)
  }



  ## Query the maturity model parameter table. This table contains the fitted model
  ## parameters for Chionoecetes spp. male 50% probability of maturity at size.
  cat("Pulling Chionoecetes model parameter data...\n")

  params_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_CHIONOECETES_MATMODEL_PARAMS_QUERY AS
                        SELECT *
                        FROM CRABBASE.CHIONOECETES_MATMODEL_PARAMS
                        WHERE SPECIES IN ", species_vec,
                        " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = params_sql)

  params_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                    query = "SELECT * FROM AKFIN_TEMPORARY_CHIONOECETES_MATMODEL_PARAMS_QUERY"),
                                    key = c("SPECIES", "REGION", "DISTRICT", "YEAR")) # KEY = which columns to sort by
  attributes(x = params_df)$sql_query <- params_sql

  # further filter by district if specified
  if(!is.null(district)){
    params_df <- params_df %>%
                 dplyr::filter(DISTRICT %in% district)
  }



  ## Clear temporary tables
  cat("Clearing temporary tables...")
  for(itable in c("CHIONOECETES_MAT_RATIO", "CHIONOECETES_MATMODEL_PARAMS")) {

    if(nrow(x = RODBC::sqlQuery(channel = channel,
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
                 args = list(male_mat_ratio = mat_ratio_df, # Chionoecetes proportion males mature at size
                             model_parameters = params_df))) # Chionoecetes 50% probability of maturity model parameters


}
