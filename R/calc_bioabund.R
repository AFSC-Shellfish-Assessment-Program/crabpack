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
#'
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
#'
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



  ## get maturity....

  # set_variables()



  #Calculate CPUE by GIS STATION, year, and maturity, shell condition, etc. ----
  cpue <- data_crab2 %>%
          dplyr::filter(STATION_ID %in% stock_stations$STATION_ID) %>%
          mutate(COUNT = SAMPLING_FACTOR, # here's where I could do n_crab vs. total_counts....
                 CPUE = SAMPLING_FACTOR/AREA_SWEPT,
                 CPUE_MT = (SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) / AREA_SWEPT / 1000 / 1000,
                 CPUE_LBS = CPUE_MT*2204.6) %>%
          group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'STATION_ID', 'SEX_TEXT', group_cols)))) %>% # don't actually need the 'group_by()' if using base R, that's mostly just to keep those cols in the 'summarise'
          dplyr::summarise(COUNT = sum(COUNT),
                           CPUE = sum(CPUE),
                           CPUE_MT = sum(CPUE_MT),
                           CPUE_LBS = sum(CPUE_LBS))


  # Expand_grid definitions ----
  #Conditionally specifying sex
  sex_combos <- c("male", "female")

  if(!is.null(sex)){
    if(TRUE %in% (!sex %in% c(TRUE, "all"))){
      sex_combos <- unique(data_crab2$SEX_TEXT)
    }
  }


  #Conditionally specifying maturity/sex combos for each stock
  if(is.null(mat_sex)){
    mat_sex_combos <- NA
  }
  if(!is.null(mat_sex)){
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
  if(is.null(shell_condition)){
    shell_combos <- NA
  }
  if(!is.null(shell_condition)){
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
  if(is.null(egg_condition)){
    egg_combos <- NA
  }
  if(!is.null(egg_condition)){
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
  if(is.null(clutch_size)){
    clutch_combos <- NA
  }
  if(!is.null(clutch_size)){
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
  if(!is.null(bin_1mm)){

    # set a max size for 1mm bins if none specified
    if(is.null(size_max)){
      size_max <- max(data_crab2$BIN_1MM, na.rm = T)
    }

    # # If no crab, exclude size bin (get weird NA things)
    # if(nrow(cpue) == 0){
    #   station_haul_cpue <- cpue %>%
    #     right_join(expand_grid(SEX_TEXT = sex_combos,
    #                            MAT_SEX = mat_sex_combos,
    #                            SHELL_TEXT = shell_combos,
    #                            EGG_CONDITION_TEXT = egg_combos,
    #                            CLUTCH_TEXT = clutch_combos,
    #                            # BIN_1MM = 1:max(cpue$BIN_1MM, na.rm = T),
    #                            HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
    #                            stock_stations %>%
    #                              dplyr::rename(GIS_STATION = STATION_ID) %>%
    #                              select(GIS_STATION, STRATUM_CODE, TOTAL_AREA) %>%
    #                              add_column(SURVEY_YEAR = years))) %>%
    #     replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
    #     dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols[!group_cols == "BIN_1MM"],
    #                            "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA"))) %>%
    #     dplyr::mutate(BIN_1MM = 100)
    # } else{
    # # if(!is.null(max_bin)){
    # #   # if cpue != 0 and there's a max size bin specified, set max to that
    # #   station_haul_cpue <- cpue %>%
    # #     right_join(expand_grid(SEX_TEXT = sex_combos,
    # #                            MAT_SEX = mat_sex_combos,
    # #                            SHELL_TEXT = shell_combos,
    # #                            EGG_CONDITION_TEXT = egg_combos,
    # #                            CLUTCH_TEXT = clutch_combos,
    # #                            BIN_1MM = 1:max_bin,
    # #                            HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
    # #                            stock_stations %>%
    # #                              dplyr::rename(GIS_STATION = STATION_ID) %>%
    # #                              select(GIS_STATION, STRATUM_CODE, TOTAL_AREA) %>%
    # #                              add_column(SURVEY_YEAR = years))) %>%
    # #     replace_na(list(COUNT = 0, CPUE = 0, CPUE_KG = 0)) %>%
    # #     dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "GIS_STATION", "SEX_TEXT", group_cols,
    # #                            "COUNT", "CPUE", "CPUE_KG", "STRATUM", "TOTAL_AREA")))
    # # }
    #   # else {
    #   # if cpue != 0 and no max size bin, set max to whatever max in data is
      station_haul_cpue <- cpue %>%
        right_join(expand_grid(SEX_TEXT = sex_combos,
                               MAT_SEX = mat_sex_combos,
                               SHELL_TEXT = shell_combos,
                               EGG_CONDITION_TEXT = egg_combos,
                               CLUTCH_TEXT = clutch_combos,
                               BIN_1MM = 1:size_max,
                               HAUL_TYPE = unique(stock_stations$HAUL_TYPE),
                               stock_stations %>%
                                 # dplyr::rename(GIS_STATION = STATION_ID) %>%
                                 select(STATION_ID, STRATUM_CODE, TOTAL_AREA) %>%
                                 add_column(SURVEY_YEAR = years))) %>%
        replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
        dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "STATION_ID", "SEX_TEXT", group_cols,
                               "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS", "STRATUM", "TOTAL_AREA")))
    # # }
    # }
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
                               # dplyr::rename(GIS_STATION = STATION_ID) %>%
                               select(STATION_ID, STRATUM_CODE, TOTAL_AREA) %>%
                               add_column(SURVEY_YEAR = years) %>%
                               distinct())) %>%
      replace_na(list(COUNT = 0, CPUE = 0, CPUE_MT = 0, CPUE_LBS = 0)) %>%
      dplyr::select(all_of(c("SURVEY_YEAR", "HAUL_TYPE", "STATION_ID", "SEX_TEXT", group_cols,
                             "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS", "STRATUM_CODE", "TOTAL_AREA")))
  }


  # add sex_text to group_cols if still needed (ie. specified in function)
  if(!is.null(sex)){
    group_cols <- append(group_cols, "SEX_TEXT")
  }

  # REMOVE NONSENSICAL MATSEX/SEX COMBOS REMNANT FROM EXPAND_GRID......
  if(!is.null(mat_sex)){
    station_haul_cpue <- station_haul_cpue %>%
      filter(!((MAT_SEX %in% c("Mature Male", "Immature Male", "Legal Male", "Sublegal Male",
                               "Pre-recruit Male", "Industry Preferred") & SEX_TEXT == "female") |
                 (MAT_SEX %in% c("Mature Female", "Immature Female", "Female") & SEX_TEXT == "male")))
  }


  ## MAKE THIS AN NA???? NEED TO INCORPORATE CHANGE EVEN IF NOT MAT_SEX....
  #Filtering out STATION E-11 in year 2000 for BBRKC males because it wasn't sampled in leg 1
  if(stock == "BBRKC"){
    station_haul_cpue <- station_haul_cpue %>%
                         dplyr::filter(!(SURVEY_YEAR == 2000 &
                                         STATION_ID == "E-11" &
                                         SEX_TEXT == "male"))
  } else{
    station_haul_cpue <- station_haul_cpue
  }


  # Replace retow BBRKC ------------------------
  if(!is.null(replace_retow)){
    if(replace_retow == TRUE){
      # replace female BBRKC with female data from station with HT 17
      station_haul_cpue <- station_haul_cpue %>%
        group_by(SURVEY_YEAR, STATION_ID, SEX_TEXT) %>%
        nest() %>%
        #Females: replacing original stations with resampled stations in retow yrs for BBRKC females
        ## I DON'T THINK WE NEED 'sex' IN THE FUNCTION?
        mutate(data = purrr::map2(data, SEX_TEXT, function(data, sex) {
          if(17 %in% data$HAUL_TYPE & stock == "BBRKC" & SEX_TEXT == "female" & STATION_ID %in% retow_stations)
          {data %>% dplyr::filter(HAUL_TYPE == 17) -> x} else{x <- data %>% dplyr::filter(HAUL_TYPE != 17)}
          return(x)
        })) %>%
        unnest(cols = c(data)) %>%
        group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM_CODE', 'TOTAL_AREA')))) %>%
        dplyr::summarise(COUNT = sum(COUNT),
                         CPUE = sum(CPUE),
                         CPUE_MT = sum(CPUE_MT),
                         CPUE_LBS = sum(CPUE_LBS))
    }

    if(replace_retow == FALSE){
      # remove all HT 17 data and re-summarise to ignore SEX
      station_haul_cpue <- station_haul_cpue %>%
        dplyr::filter(HAUL_TYPE != 17) %>%
        group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM_CODE', 'TOTAL_AREA')))) %>%
        dplyr::summarise(COUNT = sum(COUNT),
                         CPUE = sum(CPUE),
                         CPUE_MT = sum(CPUE_MT),
                         CPUE_LBS = sum(CPUE_LBS)) #%>%
    }
  }

  if(is.null(replace_retow)){
    # remove all HT 17 data and re-summarise to ignore SEX
    station_haul_cpue <- station_haul_cpue %>%
      dplyr::filter(HAUL_TYPE != 17) %>%
      group_by(across(all_of(c('SURVEY_YEAR', 'HAUL_TYPE', 'STATION_ID', group_cols, 'STRATUM_CODE', 'TOTAL_AREA')))) %>%
      dplyr::summarise(COUNT = sum(COUNT),
                       CPUE = sum(CPUE),
                       CPUE_MT = sum(CPUE_MT),
                       CPUE_LBS = sum(CPUE_LBS)) #%>%
  }


  # OUTPUTS -----------
  if(is.null(sex)){
    groups_out <- group_cols[!group_cols %in% c("SEX_TEXT")]
  } else{
    groups_out <- group_cols
  }

  ## CPUE out -----
  # if specified, return CPUE df rather than biomass or abundance (which is default)
  if(!is.null(output)){
    if(output == "cpue"){

      cpue_out <- station_haul_cpue %>%
        left_join(., stock_stations) %>%
                    # rename(GIS_STATION = STATION_ID)) %>%
        ungroup() %>%
        dplyr::select(all_of(c('SURVEY_YEAR', 'STATION_ID', 'LATITUDE', 'LONGITUDE', groups_out, 'STRATUM_CODE', 'TOTAL_AREA',
                               "COUNT", "CPUE", "CPUE_MT", "CPUE_LBS")))
      return(list(cpue_out))
    }
  }

  if(is.null(output)){
    output <- "bioabund"
  }



  # CALL CALC_CPUE FUNCTION (everything above this is actually that function....) ------
  ## set inputs from calc_bioabund....
  data_crab_bioabund <- data_crab
  species_bioabund <- species
  region_bioabund <- region
  district_bioabund <- district
  years_bioabund <- years
  sex_bioabund <- sex
  size_min_bioabund <- size_min
  size_max_bioabund <- size_max
  crab_category_bioabund <- crab_category
  female_maturity_bioabund <- female_maturity
  shell_condition_bioabund <- shell_condition
  egg_condition_bioabund <- egg_condition
  clutch_size_bioabund <- clutch_size
  bin_1mm_bioabund <- bin_1mm
  spatial_level_bioabund <- spatial_level
  replace_retow_bioabund <- replace_retow

  # call calc_cpue()
  station_haul_cpue <- calc_cpue(data_crab = data_crab_bioabund,
                                 species = species_bioabund,
                                 region = region_bioabund,
                                 district = district_bioabund,
                                 years = years_bioabund,
                                 sex = sex_bioabund,
                                 size_min = size_min_bioabund,
                                 size_max = size_max_bioabund,
                                 crab_category = crab_category_bioabund,
                                 female_maturity = female_maturity_bioabund,
                                 shell_condition = shell_condition_bioabund,
                                 egg_condition = egg_condition_bioabund,
                                 clutch_size = clutch_size_bioabund,
                                 bin_1mm = bin_1mm_bioabund1,
                                 replace_retow = replace_retow_bioabund,
                                 output = c("bioabund"))


  ## Calculate abundance and biomass -------------
  #Sum across haul, scale abundance, biomass, and variance to strata, then sum across strata and calc CIs
  bio_abund_df <- station_haul_cpue %>%
                  group_by(across(all_of(c('SURVEY_YEAR', 'STATION_ID', 'STRATUM_CODE', groups_out, 'TOTAL_AREA')))) %>%
                  dplyr::summarise(COUNT = sum(COUNT), CPUE = sum(CPUE), CPUE_KG = sum(CPUE_KG)) %>%
                  #Scale to abundance by strata
                  group_by(across(all_of(c('SURVEY_YEAR', 'STRATUM_CODE', groups_out)))) %>%
                  dplyr::reframe(AREA = TOTAL_AREA,
                                 MEAN_CPUE = mean(CPUE),
                                 N_CPUE = n(),
                                 VAR_CPUE = (stats::var(CPUE)*(AREA^2))/N_CPUE,
                                 MEAN_CPUE_MT = mean(CPUE_MT),
                                 N_CPUE_MT = n(),
                                 VAR_CPUE_MT = (stats::var(CPUE_MT)*(AREA^2))/N_CPUE_MT,
                                 MEAN_CPUE_LBS = mean(CPUE_LBS),
                                 N_CPUE_LBS = n(),
                                 VAR_CPUE_LBS = (stats::var(CPUE_LBS)*(AREA^2))/N_CPUE_LBS,
                                 ABUNDANCE = (MEAN_CPUE * AREA),
                                 BIOMASS_MT = (MEAN_CPUE_MT * AREA),
                                 BIOMASS_LBS = (MEAN_CPUE_LBS * AREA),
                                 N_STATIONS = length(unique(STATION_ID))) %>%
                  distinct() %>%
                  #Sum across strata
                  group_by(across(all_of(c('SURVEY_YEAR', groups_out)))) %>%
                  dplyr::reframe(#AREA = sum(AREA),
                                 MEAN_CPUE = sum(MEAN_CPUE),
                                 SD_CPUE = sqrt(sum(VAR_CPUE)),
                                 N_CPUE = sum(N_CPUE),
                                 MEAN_CPUE_MT = sum(MEAN_CPUE_MT),
                                 SD_CPUE_MT = sqrt(sum(VAR_CPUE_MT)),
                                 N_CPUE_MT = sum(N_CPUE_MT),
                                 MEAN_CPUE_LBS = sum(MEAN_CPUE_LBS),
                                 SD_CPUE_LBS = sqrt(sum(VAR_CPUE_LBS)),
                                 N_CPUE_LBS = sum(N_CPUE_LBS),
                                 ABUNDANCE = sum(ABUNDANCE),
                                 ABUNDANCE_CV = (SD_CPUE/ABUNDANCE),
                                 ABUNDANCE_CI = 1.96*(SD_CPUE),
                                 BIOMASS_MT = sum(BIOMASS_MT),
                                 BIOMASS_MT_CV = (SD_CPUE_MT/BIOMASS_MT),
                                 BIOMASS_MT_CI = (1.96*SD_CPUE_MT),
                                 BIOMASS_LBS = sum(BIOMASS_LBS),
                                 BIOMASS_LBS_CV = (SD_CPUE_LBS/BIOMASS_LBS),
                                 BIOMASS_LBS_CI = 1.96*(SD_CPUE_LBS),
                                 N_STATIONS = sum(N_STATIONS)) %>%
                  dplyr::mutate(N_STATIONS = ifelse((SURVEY_YEAR == 2000
                                                     & stock == "BBRKC"), 135, N_STATIONS)) %>%
                  ungroup()# %>%
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
                #   select(SURVEY_YEAR, SPECIES_CODE, SPECIES_NAME, DISTRICT_CODE, SIZE_GROUP, MATURITY, SEX, SIZE_CLASS_MM, SHELL_CONDITION,
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
                #   select(SPECIES_CODE, SPECIES_NAME, DISTRICT_CODE, SURVEY_YEAR, SIZE_CATEGORY, SIZE_GROUP, MATURITY, SEX, SIZE_CLASS_MM, SHELL_CONDITION,
                #          ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI, BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI, BIOMASS_LBS, BIOMASS_LBS_CV, BIOMASS_LBS_CI)


                return(list(bio_abund_df))
}
