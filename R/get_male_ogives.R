#' Pull time series of estimated proportion mature by 10mm bin for Chionoecetes males
#'
#' @description .... time series of Tanner Crab and Snow Crab male ogives...proportion mature
#'              Also yearly parameter estimates....
#'
#' @param species character string. One of c("TANNER", "SNOW").
#' @param district character string. One of c("ALL", "E166", "W166"). Defaults to
#'                 "ALL", "E166" and "W166" are to be used for Tanner Crab only.
#' @inheritParams get_specimen_data
#'
#' @return TBD
#'
#' @export
#'

get_male_ogives <- function(species = NULL,
                            region = c("EBS", "NBS")[1], # not sure if we have ogive calcs for NBS??
                            district = NULL,
                            channel = NULL){

  ## Set up channel if channel = NULL
  if(is.null(x = channel)){
    channel <- crabpack::get_connected()
  }


  ## Clear schema of temporary tables created in this function if present
  for(itable in c("CHIONOECETES_OGIVE_PARAMS", "CHIONOECETES_MAT_RATIO")) {

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
  species_vec <- gapindex::stitch_entries(stitch_what = species)
  region_vec <- gapindex::stitch_entries(stitch_what = region)


  ## Query the Chionoecetes male probability of maturity at size table. This table....DESCRIPTION
  cat("Pulling Chionoecetes maturity ratio data...\n")

  ogives_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_CHIONOECETES_MAT_RATIO_QUERY AS
                        SELECT *
                        FROM CRABBASE.CHIONOECETES_MAT_RATIO
                        WHERE SPECIES IN ", species_vec,
                      " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = ogives_sql)

  ogives_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                      query = "SELECT * FROM AKFIN_TEMPORARY_CHIONOECETES_MAT_RATIO_QUERY"),
                                      key = c("SPECIES", "REGION", "DISTRICT", "YEAR", "SIZE_BIN")) # KEY = which columns to sort by
  attributes(x = ogives_df)$sql_query <- ogives_sql

  # further filter by district if specified
  if(!is.null(district)){
    ogives_df <- ogives_df %>%
                 dplyr::filter(DISTRICT %in% district)
  }



  ## Query the ogives parameter table. This table....DESCRIPTION
  cat("Pulling Chionoecetes model parameter data...\n")

  params_sql <- paste("CREATE TABLE AKFIN_TEMPORARY_CHIONOECETES_OGIVE_PARAMS_QUERY AS
                        SELECT *
                        FROM CRABBASE.CHIONOECETES_OGIVE_PARAMS
                        WHERE SPECIES IN ", species_vec,
                        " AND REGION IN ", region_vec)

  RODBC::sqlQuery(channel = channel, query = params_sql)

  params_df <- data.table::data.table(RODBC::sqlQuery(channel = channel,
                                                    query = "SELECT * FROM AKFIN_TEMPORARY_CHIONOECETES_OGIVE_PARAMS_QUERY"),
                                    key = c("SPECIES", "REGION", "DISTRICT", "YEAR")) # KEY = which columns to sort by
  attributes(x = params_df)$sql_query <- params_sql

  # further filter by district if specified
  if(!is.null(district)){
    params_df <- params_df %>%
                 dplyr::filter(DISTRICT %in% district)
  }



  ## Clear temporary tables
  cat("Clearing temporary tables...")
  for(itable in c("CHIONOECETES_OGIVE_PARAMS", "CHIONOECETES_MAT_RATIO")) {

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
                 args = list(male_ogives = ogives_df, # Chionoecetes proportion males mature at size
                             model_parameters = params_df))) # Chionoecetes ogive model parameters


}
