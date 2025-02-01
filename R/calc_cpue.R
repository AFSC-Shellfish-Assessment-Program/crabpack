#' Calculate haul-level catch per unit effort
#'
#' @description This function calculates and zero-fills weight (mt, lbs) and numerical
#'              catch per unit effort (area swept, nm2).
#'
#' @inheritParams calc_bioabund
#' @param female_maturity Character string. One of `c("morphometric", "cutline")`.
#'                        Defaults to `"morphometric"` maturity for female crab. Morphometric
#'                        maturity CPUE estimates are not available for male crab
#'                        at this time.
#' @param output Character string. One of `c("cpue", "bioabund")`. Default output
#'               is `"cpue"`; `"bioabund"` is used for internal package purposes only.
#'
#' @eval c("@return", "A data frame with station-level crab counts, and area swept-expanded
#'       estimates of weight (mt, lbs) and numerical CPUE by year.",
#'       get_table_metadata("inst/extdata/metadata.csv", select = c("SPECIES", "YEAR",
#'       "REGION", "STATION_ID", "LATITUDE", "LONGITUDE", "DISTRICT", "STRATUM", "TOTAL_AREA",
#'       "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS",
#'       "CATEGORY", "SEX_TEXT", "SHELL_TEXT", "EGG_CONDITION_TEXT", "CLUTCH_TEXT", "SIZE_1MM") ))
#'
#' @export
#'


calc_cpue <- function(crab_data = NULL,
                      species = NULL,
                      region = c("EBS", "NBS")[1],
                      district = c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173")[1],
                      years = NULL,
                      sex = NULL,
                      size_min = NULL,
                      size_max = NULL,
                      crab_category = NULL,
                      female_maturity = c("morphometric", "cutline")[1],
                      shell_condition = NULL,
                      egg_condition = NULL,
                      clutch_size = NULL,
                      bin_1mm = FALSE,
                      replace_retow = TRUE,
                      rm_corners = FALSE,
                      output = c("cpue", "bioabund")[1]){


  ## Check inputs
  if(is.null(x = crab_data)){
    stop(paste0("Must provide argument `crab_data` a named list from",
               " `crabpack::get_specimen_data()`."))
  }


  # Can only handle 1 species at a time
  if(length(species) > 1){
    stop("Argument `species` must be of length = 1.")
  }


  ## ERROR: check species in data == species called in function
  if(unique(crab_data$specimen$SPECIES) != species){
    stop("Argument `species` does not match species in crab_data.")
  }


  ## ERROR: check that district(s) specified are correct for given species
  if(TRUE %in% (species %in% c("RKC", "HAIR")) &
     TRUE %in% (!district %in% c("ALL", "BB", "PRIB", "NORTH")) &
     region == "EBS"){
    stop(paste0("Valid `district` options for EBS ", species, " are:",
                " c('ALL', 'BB', 'PRIB', 'NORTH').",
                " Please set `district` to one or many of these options."))
  }

  if(species == "BKC" &
     TRUE %in% (!district %in% c("ALL", "PRIB", "STMATT", "UNSTRAT")) &
     region == "EBS"){
    stop(paste0("Valid `district` options for EBS ", species, " are:",
                " c('ALL', 'PRIB', 'STMATT', 'UNSTRAT').",
                " Please set `district` to one or many of these options."))
  }

  if(species == "TANNER" & TRUE %in% (!district %in% c("ALL", "E166", "W166", "166TO173")) &
     region == "EBS"){
    stop(paste0("Valid `district` options for EBS ", species, " are:",
                " c('ALL', 'E166', 'W166', '166TO173').",
                " Please set `district` to one or many of these options."))
  }

  if(species %in% c("SNOW", "HYBRID") & TRUE %in% (!district %in% c("ALL")) &
     region == "EBS"){
    stop(paste0("Valid `district` options for EBS ", species, " are:",
                " c('ALL'). Please set `district` to this option."))
  }


  if(TRUE %in% (species %in% c("RKC")) &
     TRUE %in% (!district %in% c("ALL", "NS")) &
     region == "NBS"){
    stop(paste0("Valid `district` options for NBS ", species, " are:",
                " c('ALL', 'NS').",
                " Please set `district` to one or many of these options."))
  }

  if(TRUE %in% (species %in% c("BKC", "TANNER", "SNOW", "HYBRID", "HAIR")) &
     TRUE %in% (!district %in% c("ALL")) &
     region == "NBS"){
    stop(paste0("Valid `district` options for NBS ", species, " is:",
                " c('ALL').",
                " Please set `district` to this option."))
  }

  ## If "ALL" is specified in `district`, just set district to all
  if("ALL" %in% district){
    district <- "ALL"
  }


  ## ERROR: Can't calculate both W166 AND 166TO173 for Tanner, they overlap and things may be wonky. Do separately
  if("166TO173" %in% district & length(district > 1)){
    stop(paste0("The 166TO173W district is a special case area for EBS Tanner Crab, and",
                " should not be calculated in conjunction with any other districts due to",
                " spatial overlap with W166. Please set `district` to just `166TO173`",
                " if you wish to query that district. Data may also have to be pulled using",
                " `crabpack::get_specimen_data` specifically for this species and district."))
  }


  ## ERROR: must specify just one output if length(output > 1)
  if(length(output) > 1){
    stop("Argument `output` must be of length = 1.")
  }


  ## WARNING: specifying 1mm bins across multiple categories may exceed memory limits
  if(!is.null(crab_category)){
    if(bin_1mm == TRUE & TRUE %in% c(length(crab_category) > 2 | "all_categories" %in% crab_category)){

      warning_yn = function(w) {
        cat("Warning message:", '\n', w, '\n')
        cont <- readline('Do you wish to continue? [Y/N] ')
        if(!cont %in% c('Y', 'y')) stop('Aborted by user', call. = FALSE)
      }

      warning_yn(paste0("Calculating across multiple categories by 1mm bin may exceed",
                        " R memory limits, especially if metrics are being calculated",
                        " across the entire region. We suggest calculating by individual",
                        " categories instead, and appending the outputs."))
    }
  }



  ## Set variables, define and filter specimen modifiers
  vars <- crabpack::set_variables(crab_data = crab_data,
                                  species = species,
                                  sex = sex,
                                  size_min = size_min,
                                  size_max = size_max,
                                  crab_category = crab_category,
                                  female_maturity = female_maturity,
                                  shell_condition = shell_condition,
                                  egg_condition = egg_condition,
                                  clutch_size = clutch_size,
                                  bin_1mm = bin_1mm)


  # Pull column names and expand_grid() categories to track throughout calculations
  group_cols <- vars$group_cols
  list2env(vars$expand_combos, .GlobalEnv)


  # define year if not specified
  if(is.null(years)){
    years <- c(1975:2030) # put dummy end year to accommodate future years?
  }


  # Pull specimen dataframe
  specimen_dat <- vars$specimen_data


  # Filter by species, make sure species is the same as the data pulled
  specimen_dat <- specimen_dat %>%
                  dplyr::filter(SPECIES %in% species)
  if(nrow(specimen_dat) == 0){
    stop(paste0("Selected `species` is not present in the specimen data. Verify",
                " that the species specified in `crabpack::get_specimen_data()`",
                " is the same species specified in `crabpack::calc_cpue()`."))
  }


  # Specify stock stations
  stock_stations <- crab_data$haul %>%
                    # Filter years, region, district
                    dplyr::filter(YEAR %in% years,
                                  REGION %in% region,
                                  !is.na(TOTAL_AREA)) %>%
                    {if(!"ALL" %in% district) dplyr::filter(., DISTRICT %in% district) else .} %>%
                    # make dummy HT for retow station tracking, all HT = 3 except 17
                    dplyr::mutate(HT = ifelse(HAUL_TYPE == 17, 17, 3),
                                  LATITUDE = MID_LATITUDE,
                                  LONGITUDE = MID_LONGITUDE) %>%
                    dplyr::select(HAULJOIN, REGION, YEAR, STATION_ID, HT, AREA_SWEPT,
                                  LATITUDE, LONGITUDE, DISTRICT, STRATUM, TOTAL_AREA) %>%
                    dplyr::distinct()

  # If want to remove MTCA corner stations, remove stations and change MTCA stratum to Single
  if(rm_corners == TRUE){
    # remove corners within MTCA strata
    stock_stations <- stock_stations %>%
                      dplyr::filter(!(STRATUM %in% c("PRIB_MTCA", "STMATT_MTCA", "STMATT_MTCA_E") & nchar(STATION_ID) == 6))

    # integrate MTCA stratum into the encompassing stratum, based on species
    if(species == "BKC"){
      stock_stations <- stock_stations %>%
                        dplyr::mutate(STRATUM = case_when(STRATUM == "PRIB_MTCA" ~ "PRIB",
                                                          STRATUM == "STMATT_MTCA" ~ "STMATT",
                                                          TRUE ~ STRATUM))
    }

    if(species %in% c("RKC", "HAIR")){
      # STMATT_MTCA corners --> NORTH for these species, not sure how to remove them
      # without removing other accidental corners in older part of ts. Could manually specify
      # but not sure if anyone wants/needs these data.
      stock_stations <- stock_stations %>%
                        dplyr::mutate(STRATUM = dplyr::case_when(STRATUM == "PRIB_MTCA" ~ "PRIB",
                                                                 TRUE ~ STRATUM))

      warning(paste0("Corner stations within the St. Matthew district are considered",
                     " part of the Northern district for this species and have not",
                     " been removed. Please submit an issue at",
                     " github.com/AFSC-Shellfish-Assessment-Program/crabpack/issues",
                     " if this data modification is needed."))

    }

    if(species == "TANNER"){
      if(district == "166TO173"){
        stock_stations <- stock_stations %>%
                          dplyr::mutate(STRATUM = dplyr::case_when(STRATUM == "PRIB_MTCA" ~ "166TO173",
                                                                   STRATUM == "STMATT_MTCA_E" ~ "166TO173",
                                                                   TRUE ~ STRATUM))
      } else{
        stock_stations <- stock_stations %>%
                          dplyr::mutate(STRATUM = dplyr::case_when(STRATUM == "PRIB_MTCA" ~ "W166",
                                                                   STRATUM == "STMATT_MTCA" ~ "W166",
                                                                   TRUE ~ STRATUM))
      }
    }

    if(species %in% c("SNOW", "HYBRID")){
      stock_stations <- stock_stations %>%
                        dplyr::mutate(STRATUM = dplyr::case_when(STRATUM == "PRIB_MTCA" ~ "EBS_SINGLE",
                                                                 STRATUM == "STMATT_MTCA" ~ "EBS_SINGLE",
                                                                 TRUE ~ STRATUM))
    }
  }

  ## Calculate CPUE by STATION, YEAR, and other biometrics ---------------------
  cpue <- specimen_dat %>%
          # make dummy HT for retow station tracking, all HT = 3 except 17
          dplyr::mutate(HT = ifelse(HAUL_TYPE == 17, 17, 3)) %>%
          # filter hauls in relevant stock stations
          dplyr::filter(HAULJOIN %in% stock_stations$HAULJOIN) %>%
          dplyr::mutate(COUNT = SAMPLING_FACTOR,
                        CPUE = SAMPLING_FACTOR/AREA_SWEPT,
                        CPUE_MT = (SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) / AREA_SWEPT / 1000 / 1000,
                        CPUE_LBS = (SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) * 0.0022046226218488 / AREA_SWEPT) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HT', 'REGION', 'STATION_ID', 'SEX_TEXT', group_cols)))) %>%
          dplyr::summarise(COUNT = sum(COUNT),
                           CPUE = sum(CPUE),
                           CPUE_MT = sum(CPUE_MT),
                           CPUE_LBS = sum(CPUE_LBS))


  ## Join to zero catch stations, summarize ------------------------------------
  station_cpue <- cpue %>%
                  # dplyr::group_by(YEAR) %>%
                  dplyr::right_join(tidyr::expand_grid(SEX_TEXT = sex_combos,
                                                       CATEGORY = category_combos,
                                                       SHELL_TEXT = shell_combos,
                                                       EGG_CONDITION_TEXT = egg_combos,
                                                       CLUTCH_TEXT = clutch_combos,
                                                       SIZE_1MM = bin_combos,
                                                       HT = unique(stock_stations$HT),
                                                       stock_stations %>%
                                                         dplyr::select(YEAR, REGION, STATION_ID,
                                                                       DISTRICT, STRATUM, TOTAL_AREA)),
                                    by = c('YEAR', 'HT', 'REGION', 'STATION_ID', 'SEX_TEXT', group_cols)) %>%
                  tidyr::replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
                  dplyr::ungroup() %>%
                  dplyr::left_join(stock_stations, ., by = c('REGION', 'YEAR', 'STATION_ID', 'HT', 'DISTRICT', 'STRATUM', 'TOTAL_AREA')) %>%
                  dplyr::select(dplyr::all_of(c("YEAR", "HT", "STATION_ID", "SEX_TEXT", group_cols,
                                                "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS",
                                                "REGION", "DISTRICT", "STRATUM", "TOTAL_AREA"))) %>%
                  distinct()


  # add sex_text to group_cols if still needed (ie. specified in function)
  if(!is.null(sex)){
    group_cols <- append(group_cols, "SEX_TEXT")
  }


  # REMOVE NONSENSICAL CRAB CATEGORY COMBOS REMNANT FROM EXPAND_GRID......
  # if we end up making egg_condition and clutch_size not dependent on female only,
  # will probably have to remove those too
  if(!is.null(crab_category)){
    station_cpue <- station_cpue %>%
                    dplyr::filter(!((CATEGORY %in% c("mature_male", "large_male", "immature_male", "small_male",
                                                     "legal_male", "sublegal_male", "preferred_male") & SEX_TEXT == "female") |
                                    (CATEGORY %in% c("mature_female", "immature_female", "female") & SEX_TEXT == "male")))
  }


  # Filter out STATION E-11 in year 2000 for BBRKC males because it wasn't sampled in leg 1
  if(species == "RKC" & region == "EBS" & TRUE %in% (district %in% c("ALL", "BB"))){
    station_cpue <- station_cpue %>%
                    dplyr::filter(!(YEAR == 2000 &
                                    STATION_ID == "E-11" &
                                    SEX_TEXT == "male"))
  } else{
    station_cpue <- station_cpue
  }


  # Replace retow BBRKC --------------------------------------------------------
  if(species == "RKC" & replace_retow != FALSE){
    # Specify retow stations and years for BBRKC
    ## Retow years: 1999 2000 2006 2007 2008 2009 2010 2011 2012 2017 2021
    retows <- stock_stations %>%
              dplyr::filter(HT == 17) %>%
              dplyr::mutate(RETOW = "yes") %>%
              dplyr::select(-c('HAULJOIN', 'AREA_SWEPT', 'LATITUDE', 'LONGITUDE', 'HT'))

    # replace female BBRKC with female data from station with HT 17
    station_cpue <- station_cpue %>%
                    dplyr::left_join(., retows, by = c('YEAR', 'STATION_ID', 'REGION', 'DISTRICT', 'STRATUM', 'TOTAL_AREA')) %>%
                    dplyr::mutate(REMOVE = dplyr::case_when((SEX_TEXT == 'female' & RETOW == "yes" & HT == 3) ~ "remove",
                                                            (SEX_TEXT == 'male' & RETOW == "yes" & HT == 17) ~ "remove",
                                                            TRUE ~ "keep")) %>%
                    dplyr::filter(REMOVE == "keep") %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'STATION_ID', 'HT', group_cols, 'SEX_TEXT', # keeping HT in for output verification
                                                                  'REGION', 'DISTRICT', 'STRATUM', 'TOTAL_AREA')))) %>%
                    dplyr::summarise(COUNT = sum(COUNT),
                                     CPUE = sum(CPUE),
                                     CPUE_MT = sum(CPUE_MT),
                                     CPUE_LBS = sum(CPUE_LBS)) %>%
                    dplyr::ungroup()
  } else{
    # remove all HT 17 data and re-summarise to ignore SEX
    station_cpue <- station_cpue %>%
                    dplyr::filter(HT != 17) %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'STATION_ID', 'HT', group_cols, # keeping HT in for output verification
                                                                  'REGION', 'DISTRICT', 'STRATUM', 'TOTAL_AREA')))) %>%
                    dplyr::summarise(COUNT = sum(COUNT),
                                     CPUE = sum(CPUE),
                                     CPUE_MT = sum(CPUE_MT),
                                     CPUE_LBS = sum(CPUE_LBS))%>%
                    dplyr::ungroup()
  }


  ## CPUE output ---------------------------------------------------------------
  # make final list of groupings to carry over to `calc_bioabund()`
  if(is.null(sex)){
    groups_out <- group_cols[!group_cols %in% c("SEX_TEXT")]
  } else{
    groups_out <- group_cols
  }

  # Format output df
  cpue_out <- station_cpue %>%
              dplyr::left_join(., stock_stations, by = c('YEAR', 'STATION_ID', 'HT', 'REGION', 'DISTRICT', 'STRATUM', 'TOTAL_AREA')) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(SPECIES = species) %>%
              dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'REGION', 'STATION_ID', 'LATITUDE', 'LONGITUDE',
                                            groups_out, 'DISTRICT', 'STRATUM', 'TOTAL_AREA',
                                            "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS")))


  # If 'output' is specified, return appropriate data
  if(!is.null(output)){
    if(output == "cpue"){
      return(cpue = cpue_out)
    }

    if(output == "bioabund"){
      return(list(group_cols = groups_out,
                  cpue = station_cpue))
    }
  }

  # If 'output' isn't specified, default output is 'cpue'
  if(is.null(output)){
    return(cpue = cpue_out)
  }

}
