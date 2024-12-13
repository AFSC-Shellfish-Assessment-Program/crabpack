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
#' @return A data frame with station-level crab counts, and area swept-expanded
#'         estimates of weight (mt, lbs) and numerical CPUE by year.
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
                      output = c("cpue", "bioabund")[1]){


  ## ERROR: must specify just one output if length(output > 1)


  ## Set variables, define and filter specimen modifiers
  vars <- set_variables(crab_data = crab_data,
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

  # Pull specimen dataframe
  specimen_dat <- vars$specimen_data

  # Pull column names and expand_grid() categories to track throughout calculations
  group_cols <- vars$group_cols
  list2env(expand_combos, .GlobalEnv)


  #*SOMETHING IN HERE to make sure species designated is same as the species you pulled data for??**
  ## CHECK SPECIES IN DATA == SPECIES CALLED IN FUNCTION


  # Filter region, district, years
  specimen_dat <- specimen_dat %>%
                  dplyr::filter(REGION %in% region)


  # Specify stock stations
  stock_stations <- specimen_dat %>%
                    dplyr::select(HAULJOIN, YEAR, STATION_ID, HAUL_TYPE, AREA_SWEPT,
                                  LATITUDE, LONGITUDE, DISTRICT, STRATUM, TOTAL_AREA)


  # Specify retow stations for BBRKC, pull by year
  ## Retow years: 1999 2000 2006 2007 2008 2009 2010 2011 2012 2017 2021
  retow_stations <- stock_stations %>%
                    dplyr::filter(HAUL_TYPE == 17) %>%
                    dplyr::select(STATION_ID) %>%
                    dplyr::pull()


  ## Calculate CPUE by STATION, YEAR, and other biometrics ---------------------
  cpue <- specimen_dat %>%
          dplyr::filter(STATION_ID %in% stock_stations$STATION_ID) %>%
          dplyr::mutate(COUNT = SAMPLING_FACTOR, # here's where I could do n_crab vs. total_counts....
                        CPUE = SAMPLING_FACTOR/AREA_SWEPT,
                        CPUE_MT = (SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) / AREA_SWEPT / 1000 / 1000,
                        CPUE_LBS = CPUE_MT*2204.6) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', 'SEX_TEXT', group_cols)))) %>% # don't actually need the 'group_by()' if using base R, that's mostly just to keep those cols in the 'summarise'
          dplyr::summarise(COUNT = sum(COUNT),
                           CPUE = sum(CPUE),
                           CPUE_MT = sum(CPUE_MT),
                           CPUE_LBS = sum(CPUE_LBS))


  # Join to zero catch stations, summarize -------------------------------------
  # If using 1mm bins, include in expand_grid
  # if(!is.null(bin_1mm)){

    # # set maximum size for 1mm bins to maximum size in data if no other maximum size is specified
    # if(is.null(size_max)){
    #   size_max <- max(specimen_dat$SIZE_1MM, na.rm = T)
    # }
    #
    # # set minimum size for 1mm bins to 1mm if no other minimum size is specified
    # if(is.null(size_min)){
    #   size_min <- 1
    # }

    # # If no crab, exclude size bin (get weird NA things)
    # if(nrow(cpue) == 0){
    #   station_haul_cpue <- cpue %>%
    #     right_join(expand_grid(SEX_TEXT = sex_combos,
    #                            CATEGORY = category_combos,
    #                            SHELL_TEXT = shell_combos,
    #                            EGG_CONDITION_TEXT = egg_combos,
    #                            CLUTCH_TEXT = clutch_combos,
    #                            # BIN_1MM = 1:max(cpue$BIN_1MM, na.rm = T),
    #                            HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
    #                            stock_stations %>%
    #                              dplyr::rename(GIS_STATION = STATION_ID) %>%
    #                              select(GIS_STATION, STRATUM, TOTAL_AREA) %>%
    #                              add_column(YEAR = years))) %>%
    #     replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
    #     dplyr::select(all_of(c("YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols[!group_cols == "BIN_1MM"],
    #                            "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA"))) %>%
    #     dplyr::mutate(BIN_1MM = 100)
    # } else{
    # # if(!is.null(max_bin)){
    # #   # if cpue != 0 and there's a max size bin specified, set max to that
    # #   station_haul_cpue <- cpue %>%
    # #     right_join(expand_grid(SEX_TEXT = sex_combos,
    # #                            CATEGORY = category_combos,
    # #                            SHELL_TEXT = shell_combos,
    # #                            EGG_CONDITION_TEXT = egg_combos,
    # #                            CLUTCH_TEXT = clutch_combos,
    # #                            BIN_1MM = 1:max_bin,
    # #                            HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
    # #                            stock_stations %>%
    # #                              dplyr::rename(GIS_STATION = STATION_ID) %>%
    # #                              select(GIS_STATION, STRATUM, TOTAL_AREA) %>%
    # #                              add_column(YEAR = years))) %>%
    # #     replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
    # #     dplyr::select(all_of(c("YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols,
    # #                            "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA")))
    # # }
    #   # else {
    #   # if cpue != 0 and no max size bin, set max to whatever max in data is
    station_cpue <- cpue %>%
                    dplyr::right_join(tidyr::expand_grid(SEX_TEXT = sex_combos,
                                                         CATEGORY = category_combos,
                                                         SHELL_TEXT = shell_combos,
                                                         EGG_CONDITION_TEXT = egg_combos,
                                                         CLUTCH_TEXT = clutch_combos,
                                                         SIZE_1MM = bin_combos,
                                                         HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                                                         stock_stations %>%
                                                           dplyr::select(STATION_ID, STRATUM, TOTAL_AREA) %>%
                                                           # tibble::add_column(YEAR = years) %>%
                                                           mutate(YEAR = years))) %>%
                    tidyr::replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
                    dplyr::select(dplyr::all_of(c("YEAR", "HAUL_TYPE", "STATION_ID", "SEX_TEXT", group_cols,
                                                  "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS", "STRATUM", "TOTAL_AREA")))
    # # }
    # }
  # } else{
  #   # if no 1mm bin, don't include that in the expand_grid
  #   station_cpue <- cpue %>%
  #     dplyr::right_join(tidyr::expand_grid(SEX_TEXT = sex_combos,
  #                                          CATEGORY = category_combos, # 816
  #                                          SHELL_TEXT = shell_combos,
  #                                          EGG_CONDITION_TEXT = egg_combos,
  #                                          CLUTCH_TEXT = clutch_combos,
  #                                          HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
  #                                          stock_stations %>%
  #                                            dplyr::select(STATION_ID, STRATUM, TOTAL_AREA) %>%
  #                                            tibble::add_column(YEAR = years) %>%
  #                                            dplyr::distinct())) %>%
  #     tidyr::replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
  #     dplyr::select(dplyr::all_of(c("YEAR", "HAUL_TYPE", "STATION_ID", "SEX_TEXT", group_cols,
  #                                   "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS", "STRATUM", "TOTAL_AREA")))
  # }


  # add sex_text to group_cols if still needed (ie. specified in function)
  if(!is.null(sex)){
    group_cols <- append(group_cols, "SEX_TEXT")
  }

  # REMOVE NONSENSICAL CRAB CATEGORY COMBOS REMNANT FROM EXPAND_GRID......
  # if we end up making egg_condition and clutch_size not dependent on female only,
  # will probably have to remove those too
  if(!is.null(crab_category)){
    station_cpue <- station_cpue %>%
      filter(!((CATEGORY %in% c("mature_male", "immature_male", "legal_male",
                                      "sublegal_male", "preferred_male") & SEX_TEXT == "female") |
                 (CATEGORY %in% c("mature_female", "immature_female", "female") & SEX_TEXT == "male")))
  }


  ## MAKE THIS AN NA???? NEED TO INCORPORATE CHANGE EVEN IF NOT CATEGORY....
  #Filtering out STATION E-11 in year 2000 for BBRKC males because it wasn't sampled in leg 1
  if(species == "RKC" & district == "BB"){
    station_cpue <- station_cpue %>%
      dplyr::filter(!(YEAR == 2000 &
                        STATION_ID == "E-11" &
                        SEX_TEXT == "male"))
  } else{
    station_cpue <- station_cpue
  }


  # Replace retow BBRKC --------------------------------------------------------
  if(species == "RKC" & replace_retow != FALSE){
    # replace female BBRKC with female data from station with HT 17
    station_cpue <- station_cpue %>%
                    dplyr::group_by(YEAR, STATION_ID, SEX_TEXT) %>%
                    tidyr::nest() %>%
                    # Females: replacing original stations with resampled stations in retow yrs for BBRKC females
                    ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
                    dplyr::mutate(data = purrr::map2(data, SEX_TEXT, function(data, sex) {
                      if(17 %in% data$HAUL_TYPE & district == "BB" & SEX_TEXT == "female" & STATION_ID %in% retow_stations)
                      {data %>% dplyr::filter(HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(HAUL_TYPE != 17)}
                      return(x)
                    })) %>%
                    tidyr::unnest(cols = c(data)) %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
                    dplyr::summarise(COUNT = sum(COUNT),
                                     CPUE = sum(CPUE),
                                     CPUE_MT = sum(CPUE_MT),
                                     CPUE_LBS = sum(CPUE_LBS))
  } else{
    # remove all HT 17 data and re-summarise to ignore SEX
    station_cpue <- station_cpue %>%
                    dplyr::filter(HAUL_TYPE != 17) %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
                    dplyr::summarise(COUNT = sum(COUNT),
                                     CPUE = sum(CPUE),
                                     CPUE_MT = sum(CPUE_MT),
                                     CPUE_LBS = sum(CPUE_LBS))
  } # else{
  #   # replace female BBRKC with female data from station with HT 17
  #   station_haul_cpue <- station_haul_cpue %>%
  #                        dplyr::group_by(YEAR, STATION_ID, SEX_TEXT) %>%
  #                        tidyr::nest() %>%
  #                        #Females: replacing original stations with resampled stations in retow yrs for BBRKC females
  #                        ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
  #                        dplyr::mutate(data = purrr::map2(data, SEX_TEXT, function(data, sex) {
  #                          if(17 %in% data$HAUL_TYPE & district == "BB" & SEX_TEXT == "female" & STATION_ID %in% retow_stations)
  #                          {data %>% dplyr::filter(HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(HAUL_TYPE != 17)}
  #                          return(x)
  #                        })) %>%
  #                        tidyr::unnest(cols = c(data)) %>%
  #                        dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
  #                        dplyr::summarise(COUNT = sum(COUNT),
  #                                         CPUE = sum(CPUE),
  #                                         CPUE_MT = sum(CPUE_MT),
  #                                         CPUE_LBS = sum(CPUE_LBS))
  # }

  ## CPUE output ---------------------------------------------------------------
  # If 'output' is specified, return appropriate data
  if(!is.null(output)){
    if(output == "cpue"){

      cpue_out <- station_cpue %>%
                  dplyr::left_join(., stock_stations) %>%
                  dplyr::ungroup() %>%
                  dplyr::mutate(SPECIES = species) %>%
                  dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'STATION_ID', 'LATITUDE', 'LONGITUDE',
                                                groups_out, 'STRATUM', 'TOTAL_AREA',
                                                "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS")))
      return(list(cpue = cpue_out))
    }

    if(output == "bioabund"){
      # make final list of groupings to carry over to `calc_bioabund()`
      if(is.null(sex)){
        groups_out <- group_cols[!group_cols %in% c("SEX_TEXT")]
      } else{
        groups_out <- group_cols
      }

      return(list(cpue = station_cpue,
                  group_cols = groups_out))
    }
  }

  # If 'output' isn't specified, default output is 'cpue'
  if(is.null(output)){
    cpue_out <- station_cpue %>%
      dplyr::left_join(., stock_stations) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(SPECIES = species) %>%
      dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'STATION_ID', 'LATITUDE', 'LONGITUDE',
                                    groups_out, 'STRATUM', 'TOTAL_AREA',
                                    "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS")))
    return(list(cpue = cpue_out))
  }

}
