#' Set optional variables used in `calc_cpue` and `calc_bioabund` and subset data
#'
#' @description A utility function to...
#'
#' @inheritParams calc_bioabund
#'
#' @return ....
#'
#' @export set_variables

set_variables <- function(crab_data = NULL,
                          species = NULL,
                          sex = NULL,
                          size_min = NULL,
                          size_max = NULL,
                          crab_category = NULL,
                          female_maturity = c("morphological", "cutline")[1],
                          shell_condition = NULL,
                          egg_condition = NULL,
                          clutch_size = NULL,
                          bin_1mm = NULL){


  ## Error messages:
  # - NOT MORE THAN 1 SPECIES AT A TIME!
  # - clutch size, egg condition only for females (right now), default subsets
  # - crab category/species combos...eg. mature male for chionoecetes (warning, set to lg_male)
  # - general check inputs to see if allowed options
  # - can't call 'all_categories' and another category -- redundant: egg condition, shell condition, clutch size, category
  ## **SOME SORT OF WARNING if wanting male, can't do morphometric, it's cutline only for this. But see Chionoecetes maturity tables?
  ## Hair --> no female maturity...cutline at least -- I guess could do morphometric? And legal/sublegal == mature/immature? (but not recommended)
  # female maturity - if not specified, warning that using morphological default, error if length > 1


  # Define set of columns to 'group_by()' and define 'expand_grid()' combinations
  # based on whether or not there are optional specimen modifiers defined in function
  group_cols <- c()
  expand_combos <- list()


  ## SEX -----------------------------------------------------------------------
  # Always include initially to carry through for BBRKC resample filtering
  data_crab <- crab_data %>%
               dplyr::mutate(SEX_TEXT = case_when(.data$SEX == 1 ~ "male",
                                                  .data$SEX == 2 ~ "female"))
  sex_combos <- c("male", "female")

  # filter sex if only want one
  if(!is.null(sex)){
    if(!sex %in% c("all_categories", TRUE)){
      data_crab <- data_crab %>%
                   dplyr::filter(.data$SEX_TEXT %in% sex)
      sex_combos <- unique(data_crab$SEX_TEXT)
    }
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(sex_combos = sex_combos))


  ## SIZE_MIN ------------------------------------------------------------------
  if(!is.null(size_min)){
    # filter minimum size
    data_crab <- data_crab %>%
                  dplyr::filter(.data$SIZE_1MM >= size_min)

  }


  ## SIZE_MAX ------------------------------------------------------------------
  if(!is.null(size_max)){
    # filter maximum size
    data_crab <- data_crab %>%
                  dplyr::filter(.data$SIZE_1MM <= size_max)
  }


  ## CRAB CATEGORY -------------------------------------------------------------
  if(is.null(crab_category)){
    category_combos <- NA
  }

  if(!is.null(crab_category)){
    # assign crab CATEGORY
    data_crab <- get_crab_category(crab_dat = data_crab,
                                   species = species,
                                   region = region,
                                   district = district,
                                   crab_category = crab_category,
                                   female_maturity = female_maturity) %>%
                 filter(!is.na(CATEGORY)) # maybe don't need line?

    # assign expand_combos
    if(species %in% c("RKC", "BKC")){
      category_combos <- c("immature_female", "mature_female",
                           "immature_male", "mature_male", "legal_male")
    }

    if(species %in% c("TANNER", "SNOW", "HYBRID")){
      category_combos <- c("immature_female", "mature_female",
                           "small_male", "large_male",
                           "legal_male", "preferred_male")
    }

    if(species == "HAIR"){
      if(female_maturity == "cutline"){
        category_combos <- c("female", "sublegal_male", "legal_male")
      } else{
        category_combos <- c("immature_female", "mature_female",
                             "sublegal_male", "legal_male")
      }
    }

    # filter categories
    if(TRUE %in% (!crab_category %in% c("all_categories"))){
      data_crab <- data_crab %>% dplyr::filter(.data$CATEGORY %in% crab_category)
      category_combos <- crab_category
    }

    # append group columns
    group_cols <- append(group_cols, "CATEGORY")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(category_combos = category_combos))


  ## SHELL CONDITION -----------------------------------------------------------
  if(is.null(shell_condition)){
    shell_combos <- NA
  }

  if(!is.null(shell_condition)){
    # assign SHELL_TEXT
    data_crab <- data_crab %>%
                 dplyr::mutate(SHELL_TEXT = case_when(.data$SHELL_CONDITION %in% 0:1 ~ "soft molting",
                                                      .data$SHELL_CONDITION == 2 ~ "new hardshell",
                                                      .data$SHELL_CONDITION == 3 ~ "oldshell",
                                                      .data$SHELL_CONDITION %in% 4:5 ~ "very oldshell"))

    if(TRUE %in% (shell_condition == "all_categories")){
      shell_combos <- c("soft molting", "new hardshell", "oldshell", "very oldshell")
    }

    # filter categories
    if(TRUE %in% (shell_condition %in% c("soft molting", "new hardshell", "oldshell", "very oldshell"))){
      data_crab <- data_crab %>%
                    dplyr::filter(.data$SHELL_TEXT %in% shell_condition)
      shell_combos <- shell_condition
    }

    # append group columns
    group_cols <- append(group_cols, "SHELL_TEXT")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(shell_combos = shell_combos))


  ## EGG CONDITION -------------------------------------------------------------
  if(is.null(egg_condition)){
    egg_combos <- NA
  }

  if(!is.null(egg_condition)){
    # assign EGG_CONDITION_TEXT
    data_crab <- data_crab %>%
                 dplyr::filter(.data$SEX == 2) %>%
                 dplyr::mutate(EGG_CONDITION_TEXT = case_when(.data$EGG_CONDITION == 0 ~ "none",
                                                              .data$EGG_CONDITION == 1 ~ "uneyed",
                                                              .data$EGG_CONDITION == 2 ~ "eyed",
                                                              .data$EGG_CONDITION == 3 ~ "dead",
                                                              .data$EGG_CONDITION == 4 ~ "empty cases",
                                                              .data$EGG_CONDITION == 5 ~ "hatching"))

    if(TRUE %in% (egg_condition == "all_categories")){
      egg_combos <- c("none", "uneyed", "eyed", "dead", "empty cases", "hatching")
    }

    # filter categories
    if(TRUE %in% (egg_condition %in% c("none", "uneyed", "eyed", "dead", "empty cases", "hatching"))){
      data_crab <- data_crab %>%
                    dplyr::filter(.data$EGG_CONDITION_TEXT %in% egg_condition)
      egg_combos <- egg_condition
    }

    # append group columns
    group_cols <- append(group_cols, "EGG_CONDITION_TEXT")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(egg_combos = egg_combos))


  ## CLUTCH SIZE ---------------------------------------------------------------
  if(is.null(clutch_size)){
    clutch_combos <- NA
  }

  if(!is.null(clutch_size)){
    # assign CLUTCH_TEXT
    data_crab <- data_crab %>%
                  dplyr::filter(.data$SEX == 2) %>%
                  dplyr::mutate(CLUTCH_TEXT = case_when(.data$CLUTCH_SIZE == 0 ~ "immature",
                                                        .data$CLUTCH_SIZE == 1 ~ "mature barren",
                                                        .data$CLUTCH_SIZE == 2 ~ "trace",
                                                        .data$CLUTCH_SIZE == 3 ~ "quarter",
                                                        .data$CLUTCH_SIZE == 4 ~ "half",
                                                        .data$CLUTCH_SIZE == 5 ~ "three quarter",
                                                        .data$CLUTCH_SIZE == 6 ~ "full"))

    if(TRUE %in% (clutch_size == "all_categories")){
      clutch_combos <- c("immature", "mature barren", "trace", "quarter",
                         "half", "three quarter", "full")
    }

    # filter categories
    if(TRUE %in% (clutch_size %in% c("immature", "mature barren", "trace", "quarter",
                                     "half", "three quarter", "full"))){
      data_crab <- data_crab %>%
                   dplyr::filter(.data$CLUTCH_TEXT %in% clutch_size)
      clutch_combos <- clutch_size
    }

    # append group columns
    group_cols <- append(group_cols, "CLUTCH_TEXT")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(clutch_combos = clutch_combos))


  ## 1MM BINS ----
  # might not really need this....could just retain SIZE_1MM into group_cols
  if(!is.null(bin_1mm)){
    # data_crab <- data_crab %>%
    #               dplyr::mutate(BIN_1MM = floor(.data$SIZE_1MM))
    group_cols <- append(group_cols, "SIZE_1MM")
  }


  return(list(specimen_data = data_crab,
              group_cols = group_cols,
              expand_combos = expand_combos)) #, size_max)) #size_max if null is defined in calc_cpue now...
}
