#' Calculate index of station-, district-, or region-level abundance and biomass
#'
#' @description This function calculates indices of abundance and biomass (MT and LBS)
#'              at the station-, district-, or region-level for a given crab species.
#'              Optional arguments also allow these indices to be calculated for subsets of
#'              crab by biometric categories such as size, sex, maturity, and shell condition.
#'
#' @param crab_data object created from `crabPack::get_data()`.
#' @param species character string. One of c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR").
#' @param region character string describing the region of interest. One of
#'               c("EBS", "NBS"). Defaults to "EBS" for Eastern Bering Sea.
#' @param district character string. One of c("ALL", "BB", "NORTH", "NS",
#'                 "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173").
#'                 Defaults to "ALL" districts within the selected region if not specified.
#' @param years numeric or integer vector of years.
#' @param sex character string. One or both of c("male", "female"). Optional,
#'            "male" or "female" only will provide estimates for the selected sex,
#'            specifying both "male" and "female" will provide estimates for
#'            each of the selected sexes.
#' @param size_min integer. Optional, desired lower range of crab sizes (inclusive).
#' @param size_max integer. Optional, desired upper range of crab sizes (inclusive).
#' @param crab_category character string. One of c("legal_male", "preferred_male", "mature_male",
#'                      "immature_male", "mature_female", "immature_female", "all").
#'                      Optional, specifying this parameter will provide estimates for
#'                      each of the selected categories; "all" will provide estimates for each
#'                      relevant category for the given species. If using a female category,
#'                      maturity will be based on morphological maturity (default "morphological"
#'                      for the optional 'female_maturity' parameter). Set 'female_maturity'
#'                      to "cutline" if you want to define female maturity based on ADF&G size cutlines.
#' @param female_maturity character string. One of c("morphological", "cutline").
#'                        Defaults to "morphological" maturity for female crab. Morphological
#'                        maturity designation for male crab are not available at this time.
#' @param shell_condition character string. One or many of c("soft molting",
#'                        "new hardshell", "oldshell", "very oldshell", "all_categories").
#'                        Optional, specifying this parameter will provide estimates
#'                        for each of the selected shell conditions; "all_categories" will
#'                        provide estimates for each available shell condition category.
#' @param egg_condition character string. One or many of c("none", "uneyed", "eyed",
#'                       "dead", "empty cases", "hatching", "all_categories"). Optional,
#'                       specifying this parameter will provide estimates for each of the
#'                       selected egg conditions; "all_categories" will provide estimates
#'                       for each available egg condition category.
#' @param clutch_size character string. One or many of c("immature", "mature barren",
#'                     "trace", "quarter", "half", "three quarter", "full", "all_categories").
#'                     Optional, specifying this parameter will provide estimates for each of
#'                     the selected clutch sizes; "all_categories" will provide estimates for
#'                     each available clutch size category.
#' @param bin_1mm boolean T/F. If TRUE, estimates will be provided for each 1mm bin
#'                within the size range specified in 'size_min' and/or 'size_max',
#'                or for the full range of observed sizes in the data. Defaults to FALSE.
#' @param spatial_level character string. One of c("station", "district", "region").
#'                      Describes the spatial resolution of biomass and abundance output,
#'                      either kept at the haul level, or aggregated to the district (default)
#'                      or regional scale.
#' @param replace_retow boolean T/F. Replace female Bristol Bay red king crab observations
#'                      with resampled data (haul type "17") in years when a Bristol Bay
#'                      retow took place? Defaults to TRUE, please use care when interpreting
#'                      BBRKC outputs if using FALSE.
#'
#' @return A data frame with area swept-expanded estimates of species- or district-level abundance,
#'         biomass (mt), and biomass (lbs) by year.
#'
#' @export

calc_bioabund <- function(crab_data = NULL,
                          species = NULL,
                          region = c("EBS", "NBS")[1],
                          district = c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT",
                                       "UNSTRAT", "E166", "W166", "166TO173")[1],
                          years = NULL,
                          sex = NULL,
                          size_min = NULL,
                          size_max = NULL,
                          crab_category = NULL,
                          female_maturity = c("morphological", "cutline")[1],
                          shell_condition = NULL,
                          egg_condition = NULL,
                          clutch_size = NULL,
                          bin_1mm = FALSE,
                          spatial_level = c("station", "district", "region")[2],
                          #output = c("abundance", "biomass_mt", "biomass_lbs")[1],
                          replace_retow = TRUE){




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
                  dplyr::filter(.data$REGION %in% region)


  # Specify stock stations
  stock_stations <- specimen_dat %>%
                    dplyr::select(HAULJOIN, YEAR, STATION_ID, HAUL_TYPE, AREA_SWEPT,
                                  LATITUDE, LONGITUDE, DISTRICT, STRATUM, TOTAL_AREA)


  # Specify retow stations for BBRKC, pull by year
  ## Retow years: 1999 2000 2006 2007 2008 2009 2010 2011 2012 2017 2021
  retow_stations <- stock_stations %>%
                    dplyr::filter(.data$HAUL_TYPE == 17) %>%
                    dplyr::select(.data$STATION_ID) %>%
                    dplyr::pull()


  ## Calculate CPUE by STATION, YEAR, and other biometrics ---------------------
  cpue <- data_crab2 %>%
          dplyr::filter(.data$STATION_ID %in% stock_stations$STATION_ID) %>%
          dplyr::mutate(COUNT = .data$SAMPLING_FACTOR, # here's where I could do n_crab vs. total_counts....
                        CPUE = .data$SAMPLING_FACTOR/.data$AREA_SWEPT,
                        CPUE_MT = (.data$SAMPLING_FACTOR * .data$CALCULATED_WEIGHT_1MM) / .data$AREA_SWEPT / 1000 / 1000,
                        CPUE_LBS = .data$CPUE_MT*2204.6) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', 'SEX_TEXT', group_cols)))) %>% # don't actually need the 'group_by()' if using base R, that's mostly just to keep those cols in the 'summarise'
          dplyr::summarise(COUNT = sum(.data$COUNT),
                           CPUE = sum(.data$CPUE),
                           CPUE_MT = sum(.data$CPUE_MT),
                           CPUE_LBS = sum(.data$CPUE_LBS))


  # # Expand_grid definitions: conditionally specify variables based on  ----
  # # Conditionally specifying sex
  # sex_combos <- c("male", "female")
  #
  # if(!is.null(sex)){
  #   if(TRUE %in% (!sex %in% c(TRUE, "all"))){
  #     sex_combos <- unique(data_crab2$SEX_TEXT)
  #   }
  # }
  #
  #
  # # Conditionally specifying maturity/sex combos for each stock
  # if(is.null(mat_sex)){
  #   mat_sex_combos <- NA
  # }
  # if(!is.null(mat_sex)){
  #   if(TRUE %in% (mat_sex %in% c("all", TRUE))){
  #     if(stock %in% stock_lookup$STOCK[stock_lookup$SPECIES %in% c("rkc", "bkc", "hybrid")]){
  #       mat_sex_combos <- c("Mature Male", "Immature Male",
  #                           "Mature Female", "Immature Female",
  #                           "Legal Male", "Pre-recruit Male")
  #     }
  #
  #     if(stock %in% stock_lookup$STOCK[stock_lookup$SPECIES %in% c("bairdi", "opilio")]){
  #       mat_sex_combos <- c("Mature Male", "Immature Male",
  #                           "Mature Female", "Immature Female",
  #                           "Legal Male", "Industry Preferred")
  #     }
  #
  #     if(stock %in% stock_lookup$STOCK[stock_lookup$SPECIES == "ei"]){
  #       mat_sex_combos <- c("Sublegal Male", "Legal Male", "Female")
  #     }
  #   }
  #   if(TRUE %in% (!mat_sex %in% c("all", TRUE))){
  #     mat_sex_combos <- mat_sex
  #   }
  # }
  #
  #
  # #Conditionally specifying shell conditions
  # if(is.null(shell_condition)){
  #   shell_combos <- NA
  # }
  # if(!is.null(shell_condition)){
  #   if(TRUE %in% (shell_condition == "all")){
  #     shell_combos <- c("soft molting", "new hardshell", "oldshell", "very oldshell")
  #   }
  #   if(TRUE %in% (!shell_condition %in% c("all"))){
  #     if(is.numeric(shell_condition)){
  #       # shell_combos <- shell_condition_lookup$SHELL_TEXT[shell_condition_lookup$SHELL_CONDITION %in% shell_condition]
  #     } else{
  #       shell_combos <- shell_condition
  #     }
  #   }
  # }
  #
  #
  # #Conditionally specifying egg conditions
  # if(is.null(egg_condition)){
  #   egg_combos <- NA
  # }
  # if(!is.null(egg_condition)){
  #   if(TRUE %in% (egg_condition == "all")){
  #     egg_combos <- c("none", "uneyed", "eyed", "dead",
  #                     "empty cases", "hatching", "unknown")
  #   }
  #   if(TRUE %in% (!egg_condition %in% c("all", NULL))){
  #     if(is.numeric(egg_condition)){
  #       # egg_combos <- egg_condition_lookup$EGG_CONDITION_TEXT[egg_condition_lookup$EGG_CONDITION %in% egg_condition]
  #     } else{
  #       egg_combos <- egg_condition
  #     }
  #   }
  # }
  #
  #
  # #Conditionally specifying clutch sizes
  # if(is.null(clutch_size)){
  #   clutch_combos <- NA
  # }
  # if(!is.null(clutch_size)){
  #   if(TRUE %in% (clutch_size == "all")){
  #     clutch_combos <- c("immature", "mature barren", "trace", "quarter",
  #                        "half", "three quarter", "full", "Unknown")
  #   }
  #   if(TRUE %in% (!clutch_size %in% c("all", NULL))){
  #     if(is.numeric(clutch_size)){
  #       # clutch_combos <- clutch_size_lookup$CLUTCH_TEXT[clutch_size_lookup$CLUTCH_SIZE %in% clutch_size]
  #     } else{
  #       clutch_combos <- clutch_size
  #     }
  #   }
  # }


  # Join to zero catch stations, summarize -------------------------------------
  # First, if using 1mm bins, include in expand_grid
  if(!is.null(bin_1mm)){

    # set a max size for 1mm bins if none specified
    if(is.null(size_max)){
      size_max <- max(data_crab2$SIZE_1MM, na.rm = T)
    }

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
                                                         SIZE_1MM = 1:size_max,
                                                         HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                                                         stock_stations %>%
                                                           dplyr::select(.data$STATION_ID, .data$STRATUM, .data$TOTAL_AREA) %>%
                                                           tibble::add_column(YEAR = years))) %>%
                    tidyr::replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
                    dplyr::select(dplyr::all_of(c("YEAR", "HAUL_TYPE", "STATION_ID", "SEX_TEXT", group_cols,
                                                  "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS", "STRATUM", "TOTAL_AREA")))
    # # }
    # }
  } else{
    # if no 1mm bin, don't include that in the expand_grid
    station_cpue <- cpue %>%
                    dplyr::right_join(tidyr::expand_grid(SEX_TEXT = sex_combos,
                                           CATEGORY = category_combos, # 816
                                           SHELL_TEXT = shell_combos,
                                           EGG_CONDITION_TEXT = egg_combos,
                                           CLUTCH_TEXT = clutch_combos,
                                           HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                                           stock_stations %>%
                                             # dplyr::rename(GIS_STATION = STATION_ID) %>%
                                             dplyr::select(.data$STATION_ID, .data$STRATUM, .data$TOTAL_AREA) %>%
                                             tibble::add_column(YEAR = years) %>%
                                             dplyr::distinct())) %>%
                    tidyr::replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
                    dplyr::select(dplyr::all_of(c("YEAR", "HAUL_TYPE", "STATION_ID", "SEX_TEXT", group_cols,
                                                  "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS", "STRATUM", "TOTAL_AREA")))
  }


  # add sex_text to group_cols if still needed (ie. specified in function)
  if(!is.null(sex)){
    group_cols <- append(group_cols, "SEX_TEXT")
  }

  # REMOVE NONSENSICAL CRAB CATEGORY COMBOS REMNANT FROM EXPAND_GRID......
  # if we end up making egg_condition and clutch_size not dependent on female only,
  # will probably have to remove those too
  if(!is.null(crab_category)){
    station_cpue <- station_cpue %>%
                    filter(!((.data$CATEGORY %in% c("mature_male", "immature_male", "legal_male",
                                                    "sublegal_male", "preferred_male") & .data$SEX_TEXT == "female") |
                               (.data$CATEGORY %in% c("mature_female", "immature_female", "female") & .data$SEX_TEXT == "male")))
  }


  ## MAKE THIS AN NA???? NEED TO INCORPORATE CHANGE EVEN IF NOT CATEGORY....
  #Filtering out STATION E-11 in year 2000 for BBRKC males because it wasn't sampled in leg 1
  if(species == "RKC" & district == "BB"){
    station_cpue <- station_cpue %>%
                    dplyr::filter(!(.data$YEAR == 2000 &
                                      .data$STATION_ID == "E-11" &
                                      .data$SEX_TEXT == "male"))
  } else{
    station_cpue <- station_cpue
  }


  # Replace retow BBRKC ------------------------
  # if(!is.null(replace_retow)){
  #   if(replace_retow == TRUE){
  #     # replace female BBRKC with female data from station with HT 17
  #     station_haul_cpue <- station_haul_cpue %>%
  #       dplyr::group_by(.data$YEAR, .data$STATION_ID, .data$SEX_TEXT) %>%
  #       tidyr::nest() %>%
  #       #Females: replacing original stations with resampled stations in retow yrs for BBRKC females
  #       ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
  #       dplyr::mutate(data = purrr::map2(.data$data, .data$SEX_TEXT, function(data, sex) {
  #         if(17 %in% data$HAUL_TYPE & district == "BB" & .data$SEX_TEXT == "female" & .data$STATION_ID %in% retow_stations)
  #         {data %>% dplyr::filter(.data$HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(.data$HAUL_TYPE != 17)}
  #         return(x)
  #       })) %>%
  #       tidyr::unnest(cols = c(.data$data)) %>%
  #       dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
  #       dplyr::summarise(COUNT = sum(.data$COUNT),
  #                        CPUE = sum(.data$CPUE),
  #                        CPUE_MT = sum(.data$CPUE_MT),
  #                        CPUE_LBS = sum(.data$CPUE_LBS))
  #   }
  #
  #   if(replace_retow == FALSE){
  #     # remove all HT 17 data and re-summarise to ignore SEX
  #     station_haul_cpue <- station_haul_cpue %>%
  #       dplyr::filter(.data$HAUL_TYPE != 17) %>%
  #       dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
  #       dplyr::summarise(COUNT = sum(.data$COUNT),
  #                        CPUE = sum(.data$CPUE),
  #                        CPUE_MT = sum(.data$CPUE_MT),
  #                        CPUE_LBS = sum(.data$CPUE_LBS)) #%>%
  #   }
  # }

  if(species == "RKC" & replace_retow != FALSE){
      # replace female BBRKC with female data from station with HT 17
      station_cpue <- station_cpue %>%
                           dplyr::group_by(.data$YEAR, .data$STATION_ID, .data$SEX_TEXT) %>%
                           tidyr::nest() %>%
                           # Females: replacing original stations with resampled stations in retow yrs for BBRKC females
                           ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
                           dplyr::mutate(data = purrr::map2(.data$data, .data$SEX_TEXT, function(data, sex) {
                             if(17 %in% data$HAUL_TYPE & district == "BB" & .data$SEX_TEXT == "female" & .data$STATION_ID %in% retow_stations)
                             {data %>% dplyr::filter(.data$HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(.data$HAUL_TYPE != 17)}
                             return(x)
                           })) %>%
                           tidyr::unnest(cols = c(.data$data)) %>%
                           dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
                           dplyr::summarise(COUNT = sum(.data$COUNT),
                                            CPUE = sum(.data$CPUE),
                                            CPUE_MT = sum(.data$CPUE_MT),
                                            CPUE_LBS = sum(.data$CPUE_LBS))
    } else{
    # remove all HT 17 data and re-summarise to ignore SEX
    station_cpue <- station_cpue %>%
                         dplyr::filter(.data$HAUL_TYPE != 17) %>%
                         dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
                         dplyr::summarise(COUNT = sum(.data$COUNT),
                                          CPUE = sum(.data$CPUE),
                                          CPUE_MT = sum(.data$CPUE_MT),
                                          CPUE_LBS = sum(.data$CPUE_LBS))
    } # else{
      #   # replace female BBRKC with female data from station with HT 17
      #   station_haul_cpue <- station_haul_cpue %>%
      #                        dplyr::group_by(.data$YEAR, .data$STATION_ID, .data$SEX_TEXT) %>%
      #                        tidyr::nest() %>%
      #                        #Females: replacing original stations with resampled stations in retow yrs for BBRKC females
      #                        ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
      #                        dplyr::mutate(data = purrr::map2(.data$data, .data$SEX_TEXT, function(data, sex) {
      #                          if(17 %in% data$HAUL_TYPE & district == "BB" & .data$SEX_TEXT == "female" & .data$STATION_ID %in% retow_stations)
      #                          {data %>% dplyr::filter(.data$HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(.data$HAUL_TYPE != 17)}
      #                          return(x)
      #                        })) %>%
      #                        tidyr::unnest(cols = c(.data$data)) %>%
      #                        dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM', 'TOTAL_AREA')))) %>%
      #                        dplyr::summarise(COUNT = sum(.data$COUNT),
      #                                         CPUE = sum(.data$CPUE),
      #                                         CPUE_MT = sum(.data$CPUE_MT),
      #                                         CPUE_LBS = sum(.data$CPUE_LBS))
      # }


  # OUTPUTS -----------


  ## CPUE output ---------------------------------------------------------------
  # If 'output' is specified, return appropriate data
  if(!is.null(output)){
    if(output == "cpue"){

      cpue_out <- station_cpue %>%
                  dplyr::left_join(.data$., stock_stations) %>%
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
                dplyr::left_join(.data$., stock_stations) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(SPECIES = species) %>%
                dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'STATION_ID', 'LATITUDE', 'LONGITUDE',
                                              groups_out, 'STRATUM', 'TOTAL_AREA',
                                              "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS")))
    return(list(cpue = cpue_out))
  }



  # CALL CALC_CPUE FUNCTION (everything above this is actually that function....) ------
  # ## set inputs from calc_bioabund....
  # crab_data_bioabund <- crab_data
  # species_bioabund <- species
  # region_bioabund <- region
  # district_bioabund <- district
  # years_bioabund <- years
  # sex_bioabund <- sex
  # size_min_bioabund <- size_min
  # size_max_bioabund <- size_max
  # crab_category_bioabund <- crab_category
  # female_maturity_bioabund <- female_maturity
  # shell_condition_bioabund <- shell_condition
  # egg_condition_bioabund <- egg_condition
  # clutch_size_bioabund <- clutch_size
  # bin_1mm_bioabund <- bin_1mm
  # spatial_level_bioabund <- spatial_level
  # replace_retow_bioabund <- replace_retow

  # call calc_cpue()
  station_cpue <- calc_cpue(crab_data = crab_data,
                            species = species,
                            region = region,
                            district = district,
                            years = years,
                            sex = sex,
                            size_min = size_min,
                            size_max = size_max,
                            crab_category = crab_category,
                            female_maturity = female_maturity,
                            shell_condition = shell_condition,
                            egg_condition = egg_condition,
                            clutch_size = clutch_size,
                            bin_1mm = bin_1mm,
                            replace_retow = replace_retow,
                            output = c("bioabund"))

  # LOOP OVER YEARS??
  # DO I NEED TO CALL set_variables() again here?? need 'groups_out'

  ## Calculate abundance and biomass -------------
  #Sum across haul, scale abundance, biomass, and variance to strata, then sum across strata and calc CIs
  bio_abund_df <- station_cpue %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'STATION_ID', 'STRATUM', groups_out, 'TOTAL_AREA')))) %>%
                  dplyr::summarise(COUNT = sum(.data$COUNT), CPUE = sum(.data$CPUE), CPUE_KG = sum(.data$CPUE_KG)) %>%
                  #Scale to abundance by strata
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'STRATUM', groups_out)))) %>%
                  dplyr::reframe(AREA = .data$TOTAL_AREA,
                                 MEAN_CPUE = mean(.data$CPUE),
                                 N_CPUE = dplyr::n(),
                                 VAR_CPUE = (stats::var(.data$CPUE)*(.data$AREA^2))/.data$N_CPUE,
                                 MEAN_CPUE_MT = mean(.data$CPUE_MT),
                                 N_CPUE_MT = dplyr::n(),
                                 VAR_CPUE_MT = (stats::var(.data$CPUE_MT)*(.data$AREA^2))/.data$N_CPUE_MT,
                                 MEAN_CPUE_LBS = mean(.data$CPUE_LBS),
                                 N_CPUE_LBS = dplyr::n(),
                                 VAR_CPUE_LBS = (stats::var(.data$CPUE_LBS)*(.data$AREA^2))/.data$N_CPUE_LBS,
                                 ABUNDANCE = (.data$MEAN_CPUE * .data$AREA),
                                 BIOMASS_MT = (.data$MEAN_CPUE_MT * .data$AREA),
                                 BIOMASS_LBS = (.data$MEAN_CPUE_LBS * .data$AREA),
                                 N_STATIONS = length(unique(.data$STATION_ID))) %>%
                  dplyr::distinct() %>%
                  #Sum across strata
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', groups_out)))) %>%
                  dplyr::reframe(#AREA = sum(.data$AREA),
                                 MEAN_CPUE = sum(.data$MEAN_CPUE),
                                 SD_CPUE = sqrt(sum(.data$VAR_CPUE)),
                                 N_CPUE = sum(.data$N_CPUE),
                                 MEAN_CPUE_MT = sum(.data$MEAN_CPUE_MT),
                                 SD_CPUE_MT = sqrt(sum(.data$VAR_CPUE_MT)),
                                 N_CPUE_MT = sum(.data$N_CPUE_MT),
                                 MEAN_CPUE_LBS = sum(.data$MEAN_CPUE_LBS),
                                 SD_CPUE_LBS = sqrt(sum(.data$VAR_CPUE_LBS)),
                                 N_CPUE_LBS = sum(.data$N_CPUE_LBS),
                                 ABUNDANCE = sum(.data$ABUNDANCE),
                                 ABUNDANCE_CV = (.data$SD_CPUE/.data$ABUNDANCE),
                                 ABUNDANCE_CI = 1.96*(.data$SD_CPUE),
                                 BIOMASS_MT = sum(.data$BIOMASS_MT),
                                 BIOMASS_MT_CV = (.data$SD_CPUE_MT/.data$BIOMASS_MT),
                                 BIOMASS_MT_CI = (1.96*.data$SD_CPUE_MT),
                                 BIOMASS_LBS = sum(.data$BIOMASS_LBS),
                                 BIOMASS_LBS_CV = (.data$SD_CPUE_LBS/.data$BIOMASS_LBS),
                                 BIOMASS_LBS_CI = 1.96*(.data$SD_CPUE_LBS),
                                 N_STATIONS = sum(.data$N_STATIONS)) %>%
                  dplyr::mutate(N_STATIONS = ifelse((.data$YEAR == 2000
                                                     & district == "BB"), 135, .data$N_STATIONS)) %>%
                  dplyr::ungroup()# %>%
                # complete.cases()

                ## format output better!!
                # bio_abund_out <- bio_abund_out_EBS %>%
                #   mutate(SPECIES_CODE = 69323,
                #          SPECIES_NAME = "Blue King Crab",
                #          DISTRICT_CODE = "STMATT",
                #          SEX = "MALE",
                #          SIZE_GROUP = paste(SEX, SIZE_CLASS_MM, sep = "_"),
                #          SHELL_CONDITION = "",
                #          MATURITY = "") %>%
                #   select(YEAR, SPECIES_CODE, SPECIES_NAME, DISTRICT_CODE, SIZE_GROUP, MATURITY, SEX, SIZE_CLASS_MM, SHELL_CONDITION,
                #          BIOMASS_LBS, BIOMASS_LBS_CV, BIOMASS_LBS_CI, ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI, BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI)
                #
                # bio_abund_out2 <- bio_abund_out_EBS2 %>%
                #   mutate(SPECIES_CODE = 69323,
                #          SPECIES_NAME = "Blue King Crab",
                #          DISTRICT_CODE = "STMATT",
                #          SEX = "MALE",
                #          SIZE_GROUP = paste(SEX, SIZE_CLASS_MM, sep = "_"),
                #          SHELL_CONDITION = "",
                #          MATURITY = "",
                #          SIZE_CATEGORY = "SIZE_GROUP") %>%
                #   select(SPECIES_CODE, SPECIES_NAME, DISTRICT_CODE, YEAR, SIZE_CATEGORY, SIZE_GROUP, MATURITY, SEX, SIZE_CLASS_MM, SHELL_CONDITION,
                #          ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI, BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI, BIOMASS_LBS, BIOMASS_LBS_CV, BIOMASS_LBS_CI)


                return(list(bio_abund_df))
}
