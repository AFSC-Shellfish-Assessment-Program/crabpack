#' Calculate index of station-, district-, or region-level abundance and biomass
#'
#' @description This function calculates indices of abundance and biomass (MT and LBS)
#'              at the station-, district-, or region-level for a given crab species.
#'              Optional arguments also allow these indices to be calculated for subsets of
#'              crab by biometric categories such as size, sex, maturity, and shell condition.
#'
#' @param crab_data object created from `crabPack::get_data()`.
#' @param species character string. One of c("Red King Crab", "Blue King Crab",
#'                "Tanner Crab", "Snow Crab", "Hybrid Tanner Crab", "Horsehair Crab").
#'                c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR").
#' @param region character string describing the region of interest. One of
#'               c("EBS", "NBS"). Defaults to "EBS" for Eastern Bering Sea.
#' @param district character string. One of c("ALL", "BB", "NORTH", "NS",
#'                 "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173").
#'                 Defaults to "ALL" districts within the selected region if not specified.
#' @param years numeric or integer vector of years.
#'
#' @param sex character string. One or both of c("male", "female"). Optional,
#'            "male" or "female" only will provide estimates for the selected sex,
#'            specifying both "male" and "female" will provide estimates for
#'            each of the selected sexes.
#' @param female_maturity character string. One of c("morphological", "cutline").
#'                        Defaults to "morphological" maturity for female crab. Morphological
#'                        maturity designation for male crab are not available at this time.
#'
#' @param shell_condition character string. One or many of c("soft molting",
#'                        "new hardshell", "oldshell", "very oldshell", "all").
#'                        Optional, specifying this parameter will provide estimates
#'                        for each of the selected shell conditions; "all" will provide
#'                        estimates for each available shell condition category.
#' @param egg_condition: character string. One or many of c("none", "uneyed", "eyed",
#'                       "dead", "empty cases", "hatching", "all"). Optional, specifying
#'                       this parameter will provide estimates for each of the selected
#'                       egg conditions; "all" will provide estimates for each available
#'                       egg condition category.
#' @param clutch_size: character string. One or many of c("immature", "mature barren",
#'                     "trace", "quarter", "half", "three quarter", "full"). Optional,
#'                     specifying this parameter will provide estimates for each of the
#'                     selected clutch sizes; "all" will provide estimates for each
#'                     available clutch size category.
#'
#' @param size_min integer. Optional, desired lower range of crab sizes (inclusive).
#' @param size_max integer. Optional, desired upper range of crab sizes (inclusive).
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

calc_bioabund <- function(data_crab = NULL,
                          species = NULL,
                          region = c("EBS", "NBS")[1],
                          district = c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173")[1],
                          years = NULL,
                          sex = NULL,
                          female_maturity = c("morphological", "cutline")[1],
                          shell_condition = NULL,
                          egg_condition = NULL,
                          clutch_size = NULL,
                          size_min = NULL,
                          size_max = NULL,
                          bin_1mm = FALSE,
                          spatial_level = c("station", "district", "region")[2],
                          #output = c("abundance", "biomass_mt", "biomass_lbs")[1],
                          replace_retow = TRUE){



  #*SOMETHING IN HERE to make sure species designated is same as the species you pulled data for??**



  # Define districts, stock stations, stratum areas -------
  # pull districts by stock specified
  ## FILTER DISTRICT? SHOULD ALREADY BE SPECIFIED NOW....but maybe provide option if want to call full species data then do one district at a time...
  dist <- dist_stock_lookup %>%
    dplyr::filter(STOCK == stock) %>%
    pull(DISTRICT) ## need to change this to stratum, and stock --> district? Tying to keep naming consistent....

  # ## WHY CAN'T WE JUST ADD group_by SURVEY_YEAR??
  # for(i in 1:length(years)){
  # }

  # Specify retow stations for BBRKC, pull by year
  ## Retow years: 1999 2000 2006 2007 2008 2009 2010 2011 2012 2017 2021
  retow_stations <- data_crab2 %>%
    dplyr::filter(HAUL_TYPE == 17) %>%
    dplyr::select(STATION_ID) %>%
    pull()



  ## Add biometrics definitions and filtering ----
  # define set of columns to 'group_by()' based on whether or not there is "shellcond", "size", or other modifiers defined in the function
  group_cols <- c()


  ###SEX: Define/filter if specified in function ----
  # First, always include initially to carry through for BBRKC resample filtering
  data_crab2 <- data_crab2 %>%
    mutate(SEX_TEXT = case_when(SEX == 1 ~ "male",
                                SEX == 2 ~ "female"))

  # Filter sex if only want one
  if(!missing(sex)){
    if(!sex %in% c("all", TRUE))
      data_crab2 <- data_crab2 %>%
        dplyr::filter(SEX_TEXT %in% sex)
  }


  ###MATURITY: Define/filter if specified in function ----
  if(!missing(mat_sex)){
    # assign maturity
    data_crab2 <- get_maturity(crab_dat = data_crab2, stock = stock) %>%
      filter(!is.na(MAT_SEX))

    # filter by specific category
    if(!mat_sex %in% c("all", TRUE)){
      data_crab2 <- data_crab2 %>% dplyr::filter(MAT_SEX %in% mat_sex)
    }

    group_cols <- append(group_cols, "MAT_SEX")
  }


  ###SHELL CONDITION: Define/filter if specified in function ----
  if(!missing(shell_condition)){
    data_crab2 <- data_crab2 %>%
      mutate(SHELL_TEXT = case_when(SHELL_CONDITION %in% 0:1 ~ "soft molting",
                                    SHELL_CONDITION == 2 ~ "new hardshell",
                                    SHELL_CONDITION == 3 ~ "oldshell",
                                    SHELL_CONDITION %in% 4:5 ~ "very oldshell"))

    # filter by specific category
    if(TRUE %in% (shell_condition %in% c(0:5))){
      data_crab2 <- data_crab2 %>% dplyr::filter(SHELL_CONDITION %in% shell_condition)
    }

    if(TRUE %in% (shell_condition %in% c("soft molting", "new hardshell", "oldshell", "very oldshell"))){
      data_crab2 <- data_crab2 %>% dplyr::filter(SHELL_TEXT %in% shell_condition)
    }

    group_cols <- append(group_cols, "SHELL_TEXT")
  }


  ###EGG CONDITION: Define/filter if specified in function ----
  if(!missing(egg_condition)){
    data_crab2 <- data_crab2 %>%
      dplyr::filter(SEX == 2) %>% #HAUL_TYPE != 17,
      mutate(EGG_CONDITION_TEXT = case_when(EGG_CONDITION == 0 ~ "none",
                                            EGG_CONDITION == 1 ~ "uneyed",
                                            EGG_CONDITION == 2 ~ "eyed",
                                            EGG_CONDITION == 3 ~ "dead",
                                            EGG_CONDITION == 4 ~ "empty cases",
                                            EGG_CONDITION == 5 ~ "hatching",
                                            TRUE ~ "unknown"))

    if(TRUE %in% (egg_condition %in% c(0:5))){
      data_crab2 <- data_crab2 %>% dplyr::filter(EGG_CONDITION %in% egg_condition)
    }

    if(TRUE %in% (egg_condition %in% c("none", "uneyed", "eyed", "dead",
                                       "empty cases", "hatching", "unknown"))){
      data_crab2 <- data_crab2 %>% dplyr::filter(EGG_CONDITION_TEXT %in% egg_condition)
    }

    group_cols <- append(group_cols, "EGG_CONDITION_TEXT")
  }


  ###CLUTCH SIZE: Define/filter if specified in function ----
  if(!missing(clutch_size)){
    data_crab2 <- data_crab2 %>%
      dplyr::filter(SEX == 2) %>% #HAUL_TYPE != 17,
      mutate(CLUTCH_TEXT = case_when(CLUTCH_SIZE == 0 ~ "immature",
                                     CLUTCH_SIZE == 1 ~ "mature barren",
                                     CLUTCH_SIZE == 2 ~ "trace",
                                     CLUTCH_SIZE == 3 ~ "quarter",
                                     CLUTCH_SIZE == 4 ~ "half",
                                     CLUTCH_SIZE == 5 ~ "three quarter",
                                     CLUTCH_SIZE == 6 ~ "full",
                                     ((CLUTCH_SIZE == 999) | is.na(CLUTCH_SIZE) == TRUE) ~ "unknown"))

    if(TRUE %in% (clutch_size %in% c(0:6))){
      data_crab2 <- data_crab2 %>% dplyr::filter(CLUTCH_SIZE %in% clutch_size)
    }

    if(TRUE %in% (clutch_size %in% c("immature", "mature Barren", "trace", "quarter",
                                     "half", "three quarter", "full", "unknown"))){
      data_crab2 <- data_crab2 %>% dplyr::filter(CLUTCH_TEXT %in% clutch_size)
    }

    group_cols <- append(group_cols, "CLUTCH_TEXT")
  }


  ###1MM BINS: Define/filter if specified in function ----
  if(!missing(size_bin)){
    data_crab2 <- data_crab2 %>% dplyr::mutate(SIZE_BIN = floor(get(SIZE_DEF)))

    group_cols <- append(group_cols, "SIZE_BIN")
  }


  ###SIZE RANGE: Filter if specified in function ----
  if(!missing(size_range)){
    data_crab2 <- data_crab2 %>%
      dplyr::filter(get(SIZE_DEF) >= min(size_range) &
                      get(SIZE_DEF) <= max(size_range))
  }


  #Calculate CPUE by GIS STATION, year, and maturity, shell condition, etc. ----
  cpue <- data_crab2 %>%
          dplyr::filter(GIS_STATION %in% stock_stations$STATION_ID) %>%
          mutate(COUNT = SAMPLING_FACTOR, # here's where I could do n_crab vs. total_counts....
                 CPUE = SAMPLING_FACTOR/AREA_SWEPT,
                 CPUE_KG = (SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) / AREA_SWEPT / 1000) %>%
          group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'GIS_STATION', 'SEX_TEXT', group_cols)))) %>%
          dplyr::summarise(COUNT = sum(COUNT),
                           CPUE = sum(CPUE),
                           CPUE_KG = sum(CPUE_KG)) #%>%
        # filter(is.na(MAT_SEX) == FALSE)


  # Expand_grid definitions ----
  #Conditionally specifying sex
  sex_combos <- c("male", "female")

  if(!missing(sex)){
    if(TRUE %in% (!sex %in% c(TRUE, "all"))){
      sex_combos <- unique(data_crab2$SEX_TEXT)
    }
  }


  #Conditionally specifying maturity/sex combos for each stock
  if(missing(mat_sex)){
    mat_sex_combos <- NA
  }
  if(!missing(mat_sex)){
    if(TRUE %in% (mat_sex %in% c("all", TRUE))){
      if(stock %in% stock_lookup$STOCK[stock_lookup$SPECIES %in% c("rkc", "bkc", "hybrid")]){
        mat_sex_combos <- c("Mature Male", "Immature Male",
                            "Mature Female", "Immature Female",
                            "Legal Male", "Pre-recruit Male")
      }

      if(stock %in% stock_lookup$STOCK[stock_lookup$SPECIES %in% c("bairdi", "opilio")]){
        mat_sex_combos <- c("Mature Male", "Immature Male",
                            "Mature Female", "Immature Female",
                            "Legal Male", "Industry Preferred")
      }

      if(stock %in% stock_lookup$STOCK[stock_lookup$SPECIES == "ei"]){
        mat_sex_combos <- c("Sublegal Male", "Legal Male", "Female")
      }
    }
    if(TRUE %in% (!mat_sex %in% c("all", TRUE))){
      mat_sex_combos <- mat_sex
    }
  }


  #Conditionally specifying shell conditions
  if(missing(shell_condition)){
    shell_combos <- NA
  }
  if(!missing(shell_condition)){
    if(TRUE %in% (shell_condition == "all")){
      shell_combos <- c("soft molting", "new hardshell", "oldshell", "very oldshell")
    }
    if(TRUE %in% (!shell_condition %in% c("all"))){
      if(is.numeric(shell_condition)){
        shell_combos <- shell_condition_lookup$SHELL_TEXT[shell_condition_lookup$SHELL_CONDITION %in% shell_condition]
      } else{
        shell_combos <- shell_condition
      }
    }
  }


  #Conditionally specifying egg conditions
  if(missing(egg_condition)){
    egg_combos <- NA
  }
  if(!missing(egg_condition)){
    if(TRUE %in% (egg_condition == "all")){
      egg_combos <- c("none", "uneyed", "eyed", "dead",
                      "empty cases", "hatching", "unknown")
    }
    if(TRUE %in% (!egg_condition %in% c("all", NULL))){
      if(is.numeric(egg_condition)){
        egg_combos <- egg_condition_lookup$EGG_CONDITION_TEXT[egg_condition_lookup$EGG_CONDITION %in% egg_condition]
      } else{
        egg_combos <- egg_condition
      }
    }
  }


  #Conditionally specifying clutch sizes
  if(missing(clutch_size)){
    clutch_combos <- NA
  }
  if(!missing(clutch_size)){
    if(TRUE %in% (clutch_size == "all")){
      clutch_combos <- c("immature", "mature barren", "trace", "quarter",
                         "half", "three quarter", "full", "Unknown")
    }
    if(TRUE %in% (!clutch_size %in% c("all", NULL))){
      if(is.numeric(clutch_size)){
        clutch_combos <- clutch_size_lookup$CLUTCH_TEXT[clutch_size_lookup$CLUTCH_SIZE %in% clutch_size]
      } else{
        clutch_combos <- clutch_size
      }
    }
  }


  # Join to zero catch stations, summarize ----
  # First, if using 1mm bins, include in expand_grid
  if(!missing(size_bin)){
    # If no crab, exclude size bin (get weird NA things)
    if(nrow(cpue) == 0){
      station_haul_cpue <- cpue %>%
        right_join(expand_grid(SEX_TEXT = sex_combos,
                               MAT_SEX = mat_sex_combos,
                               SHELL_TEXT = shell_combos,
                               EGG_CONDITION_TEXT = egg_combos,
                               CLUTCH_TEXT = clutch_combos,
                               # SIZE_BIN = 1:max(cpue$SIZE_BIN, na.rm = T),
                               HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                               stock_stations %>%
                                 dplyr::rename(GIS_STATION = STATION_ID) %>%
                                 select(GIS_STATION, STRATUM_NAME, TOTAL_AREA) %>%
                                 add_column(SURVEY_YEAR = years))) %>%
        replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
        dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols[!group_cols == "SIZE_BIN"],
                               "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA"))) %>%
        dplyr::mutate(SIZE_BIN = 100)
    } else{ if(!missing(max_bin)){
      # if cpue != 0 and there's a max size bin specified, set max to that
      station_haul_cpue <- cpue %>%
        right_join(expand_grid(SEX_TEXT = sex_combos,
                               MAT_SEX = mat_sex_combos,
                               SHELL_TEXT = shell_combos,
                               EGG_CONDITION_TEXT = egg_combos,
                               CLUTCH_TEXT = clutch_combos,
                               SIZE_BIN = 1:max_bin,
                               HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                               stock_stations %>%
                                 dplyr::rename(GIS_STATION = STATION_ID) %>%
                                 select(GIS_STATION, STRATUM_NAME, TOTAL_AREA) %>%
                                 add_column(SURVEY_YEAR = years))) %>%
        replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
        dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols,
                               "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA")))
    } else {
      # if cpue != 0 and no max size bin, set max to whatever max in data is
      station_haul_cpue <- cpue %>%
        right_join(expand_grid(SEX_TEXT = sex_combos,
                               MAT_SEX = mat_sex_combos,
                               SHELL_TEXT = shell_combos,
                               EGG_CONDITION_TEXT = egg_combos,
                               CLUTCH_TEXT = clutch_combos,
                               SIZE_BIN = 1:max(cpue$SIZE_BIN, na.rm = T),
                               HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                               stock_stations %>%
                                 dplyr::rename(GIS_STATION = STATION_ID) %>%
                                 select(GIS_STATION, STRATUM_NAME, TOTAL_AREA) %>%
                                 add_column(SURVEY_YEAR = years))) %>%
        replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
        dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols,
                               "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA")))
    }
    }
  } else{
    # if no 1mm bin, don't include that in the expand_grid
    station_haul_cpue <- cpue %>%
      right_join(expand_grid(SEX_TEXT = sex_combos,
                             MAT_SEX = mat_sex_combos, # 816
                             SHELL_TEXT = shell_combos,
                             EGG_CONDITION_TEXT = egg_combos,
                             CLUTCH_TEXT = clutch_combos,
                             HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                             stock_stations %>%
                               dplyr::rename(GIS_STATION = STATION_ID) %>%
                               select(GIS_STATION, STRATUM_NAME, TOTAL_AREA) %>%
                               add_column(SURVEY_YEAR = years) %>%
                               distinct())) %>%
      replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
      dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols,
                             "COUNT", "CPUE", "CPUE_KG", "STRATUM_NAME", "TOTAL_AREA")))
  }

  # add sex_text to group_cols if still needed (ie. specified in function)
  if(!missing(sex)){
    group_cols <- append(group_cols, "SEX_TEXT")
  }

  # REMOVE NONSENSICAL MATSEX/SEX COMBOS REMNANT FROM EXPAND_GRID......
  if(!missing(mat_sex)){
    station_haul_cpue <- station_haul_cpue %>%
      filter(!((MAT_SEX %in% c("Mature Male", "Immature Male", "Legal Male", "Sublegal Male",
                               "Pre-recruit Male", "Industry Preferred") & SEX_TEXT == "female") |
                 (MAT_SEX %in% c("Mature Female", "Immature Female", "Female") & SEX_TEXT == "male")))
  }


  ## MAKE THIS AN NA???? NEED TO INCORPORATE CHANGE EVEN IF NOT MAT_SEX....
  #Filtering out GIS_STATION E-11 in year 2000 for BBRKC males because it wasn't sampled in leg 1
  if(stock == "BBRKC"){
    station_haul_cpue <- station_haul_cpue %>%
      dplyr::filter(!(SURVEY_YEAR == 2000 &
                        GIS_STATION == "E-11" &
                        SEX_TEXT == "male"))
  } else{
    station_haul_cpue <- station_haul_cpue
  }


  # Replace retow BBRKC ------------------------
  if(!missing(replace_retow)){
    if(replace_retow == TRUE){
      # replace female BBRKC with female data from station with HT 17
      station_haul_cpue <- station_haul_cpue %>%
        group_by(SURVEY_YEAR, GIS_STATION, SEX_TEXT) %>%
        nest() %>%
        #Females: replacing original stations with resampled stations in retow yrs for BBRKC females
        ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
        mutate(data = purrr::map2(data, SEX_TEXT, function(data, sex) {
          if(17 %in% data$HAUL_TYPE & stock == "BBRKC" & SEX_TEXT == "female" & GIS_STATION %in% retow_stations)
          {data %>% dplyr::filter(HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(HAUL_TYPE != 17)}
          return(x)
        })) %>%
        unnest(cols = c(data)) %>%
        group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'GIS_STATION', group_cols, 'STRATUM_NAME', 'TOTAL_AREA')))) %>%
        dplyr::summarise(COUNT = sum(COUNT),
                         CPUE = sum(CPUE),
                         CPUE_KG = sum(CPUE_KG))
    }

    if(replace_retow == FALSE){
      # remove all HT 17 data and re-summarise to ignore SEX
      station_haul_cpue <- station_haul_cpue %>%
        dplyr::filter(HAUL_TYPE != 17) %>%
        group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'GIS_STATION', group_cols, 'STRATUM_NAME', 'TOTAL_AREA')))) %>%
        dplyr::summarise(COUNT = sum(COUNT),
                         CPUE = sum(CPUE),
                         CPUE_KG = sum(CPUE_KG)) #%>%
    }
  }

  if(missing(replace_retow)){
    # remove all HT 17 data and re-summarise to ignore SEX
    station_haul_cpue <- station_haul_cpue %>%
      dplyr::filter(HAUL_TYPE != 17) %>%
      group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'GIS_STATION', group_cols, 'STRATUM_NAME', 'TOTAL_AREA')))) %>%
      dplyr::summarise(COUNT = sum(COUNT),
                       CPUE = sum(CPUE),
                       CPUE_KG = sum(CPUE_KG)) #%>%
  }


  # OUTPUTS -----------
  if(missing(sex)){
    groups_out <- group_cols[!group_cols %in% c("SEX_TEXT")]
  } else{
    groups_out <- group_cols
  }

  ## CPUE out -----
  # if specified, return CPUE df rather than biomass or abundance (which is default)
  if(!missing(output)){
    if(output == "cpue"){

      cpue_out <- station_haul_cpue %>%
        left_join(., stock_stations %>%
                    rename(GIS_STATION = STATION_ID)) %>%
        ungroup() %>%
        dplyr::select(all_of(c('SURVEY_YEAR', 'GIS_STATION', 'LATITUDE', 'LONGITUDE', groups_out, 'STRATUM_NAME', 'TOTAL_AREA',
                               "COUNT", "CPUE", "CPUE_KG")))
      return(list(cpue_out))
    }
  }

  if(missing(output)){
    output <- "bioabund"
  }

  ## Calculate abundance and biomass -------------
  #Sum across haul, scale abundance, biomass, and variance to strata, then sum across strata and calc CIs
  if(output == "bioabund"){
    bio_abund_df <- station_haul_cpue %>%
      group_by(across(all_of(c('SURVEY_YEAR', 'GIS_STATION', 'STRATUM_NAME', groups_out, 'TOTAL_AREA')))) %>%
      dplyr::summarise(COUNT = sum(COUNT), CPUE = sum(CPUE), CPUE_KG = sum(CPUE_KG)) %>%
      #Scale to abundance by strata
      group_by(across(all_of(c('SURVEY_YEAR', 'STRATUM_NAME', groups_out)))) %>%
      dplyr::reframe(AREA = TOTAL_AREA,
                     MEAN_CPUE = mean(CPUE),
                     N_CPUE = n(),
                     VAR_CPUE = (var(CPUE)*(AREA^2))/N_CPUE,
                     MEAN_CPUE_KG = mean(CPUE_KG),
                     N_CPUE_KG = n(),
                     VAR_CPUE_KG = (var(CPUE_KG)*(AREA^2))/N_CPUE_KG,
                     ABUNDANCE = (MEAN_CPUE * AREA),
                     BIOMASS = (MEAN_CPUE_KG * AREA),
                     N_STATIONS = length(unique(GIS_STATION))) %>%
      distinct() %>%
      #Sum across strata
      group_by(across(all_of(c('SURVEY_YEAR', groups_out)))) %>%
      dplyr::reframe(AREA = sum(AREA),
                     MEAN_CPUE = sum(MEAN_CPUE),
                     SD_CPUE = sqrt(sum(VAR_CPUE)),
                     N_CPUE = sum(N_CPUE),
                     MEAN_CPUE_KG = sum(MEAN_CPUE_KG),
                     SD_CPUE_KG = sqrt(sum(VAR_CPUE_KG)),
                     N_CPUE_KG = sum(N_CPUE_KG),
                     ABUNDANCE = sum(ABUNDANCE),
                     ABUNDANCE_CI = 1.96*(SD_CPUE),
                     BIOMASS = sum(BIOMASS),
                     BIOMASS_CI = 1.96*(SD_CPUE_KG),
                     N_STATIONS = sum(N_STATIONS)) %>%
      dplyr::mutate(N_STATIONS = ifelse((SURVEY_YEAR == 2000
                                         & stock == "BBRKC"), 135, N_STATIONS)) %>% # get rid of the "n-1" part? NA in avg
      ungroup()# %>%
    # complete.cases()

    return(list(bio_abund_df))
  }

}
