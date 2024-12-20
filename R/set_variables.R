#' Helper function to set optional variables and subset data
#'
#' @description A utility function to subset specimen data by optional specimen
#'              biometric categories, and to set these variables for further use
#'              in CPUE, abundance, and biomass calculations within the `crabpack::calc_cpue()`
#'              and `crabpack::calc_bioabund()` functions.
#'
#' @inheritParams calc_bioabund
#'
#' @return A named list of subsetted specimen data by biometrics of interest
#'         as well as variables to be carried forward for CPUE, abundance, and
#'         biomass calculations across the selected biometrics.
#'
#' @export
#'


set_variables <- function(crab_data = NULL,
                          species = NULL,
                          sex = NULL,
                          size_min = NULL,
                          size_max = NULL,
                          crab_category = NULL,
                          female_maturity = c("morphometric", "cutline")[1],
                          shell_condition = NULL,
                          egg_condition = NULL,
                          clutch_size = NULL,
                          bin_1mm = FALSE){


  ## Error messages:
  # Can only handle 1 species at a time
  if(length(species) > 1){
    stop("Argument `species` must be of length = 1.")
  }

  # clutch size, egg condition only for females (right now), default subsets
  if(!is.null(egg_condition) | !is.null(clutch_size)){
    warning(paste0("`egg_condition` and `clutch_size` are for female crab only.",
                   " Specifying these arguments will return female-only data."))
  }

  # - crab category/species combos
  if(species %in% c("TANNER", "SNOW", "HYBRID")){

    if("immature_male" %in% crab_category){
      crab_category <- replace(crab_category, crab_category == "immature_male", "small_male")
      warning(paste0("Category 'immature_male' is not available for Chionoecetes spp. crab.",
                     " Category has been set to 'small_male' instead."))
    }

    if("mature_male" %in% crab_category){
      crab_category <- replace(crab_category, crab_category == "mature_male", "large_male")
      warning(paste0("Category 'mature_male' is not available for Chionoecetes spp. crab.",
                     " Category has been set to 'large_male' instead."))
    }

    if(crab_category %in% c("sublegal_male")){
      stop(paste("'sublegal_male' is not a valid category for this species."))
    }
  }


  if(species %in% c("RKC", "BKC")){
    if("small_male" %in% crab_category){
      crab_category <- replace(crab_category, crab_category == "small_male", "large_male")
      warning(paste0("Category 'small_male' is not available for king crab species.",
                     " Category has been set to 'large_male' instead."))
    }

    if("large_male" %in% crab_category){
      crab_category <- replace(crab_category, crab_category == "large_male", "mature_male")
      warning(paste0("Category 'large_male' is not available for king crab species.",
                     " Category has been set to 'mature_male' instead."))
    }

    if("sublegal_male" %in% crab_category){
      stop(paste("'sublegal_male' is not a valid category for this species."))
    }

    if("preferred_male" %in% crab_category){
      stop(paste("'preferred_male' is not a valid category for this species."))
    }
  }


  if(species %in% c("HAIR")){
    if(TRUE %in% (crab_category %in% c("small_male", "large_male", "immature_male", "mature_male", "preferred_male"))){
      stop(paste0("One or many specified male category is not valid for this species. Please use",
                  " 'sublegal_male' and/or 'legal_male' only."))
    }
  }



  # Define set of columns to 'group_by()' and define 'expand_grid()' combinations
  # based on whether or not there are optional specimen modifiers defined in function
  group_cols <- c()
  expand_combos <- list()


  ## CRAB CATEGORY -------------------------------------------------------------
  if(is.null(crab_category)){
    # If not using crab_category, pull specimen dataframe, remove 0-catch stations
    data_crab <- crab_data$specimen %>%
                 dplyr::filter(!is.na(SEX))

    category_combos <- NA
  }

  if(!is.null(crab_category)){
    # assign crab CATEGORY
    data_crab <- crabpack::set_crab_category(crab_data = crab_data,
                                             species = species,
                                             region = region,
                                             district = district,
                                             crab_category = crab_category,
                                             female_maturity = female_maturity)

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
      data_crab <- data_crab %>% dplyr::filter(CATEGORY %in% crab_category)
      category_combos <- crab_category
    }

    # append group columns
    group_cols <- append(group_cols, "CATEGORY")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(category_combos = category_combos))


  ## SEX -----------------------------------------------------------------------
  # Always include initially to carry through for BBRKC resample filtering
  data_crab <- data_crab %>%
               dplyr::mutate(SEX_TEXT = case_when(SEX == 1 ~ "male",
                                                  SEX == 2 ~ "female"))
  sex_combos <- c("male", "female")

  # filter sex if only want one
  if(!is.null(sex)){
    if(!sex %in% c("all_categories", TRUE)){
      data_crab <- data_crab %>%
                   dplyr::filter(SEX_TEXT %in% sex)
      sex_combos <- unique(data_crab$SEX_TEXT)
    }
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(sex_combos = sex_combos))


  ## SIZE_MIN ------------------------------------------------------------------
  if(!is.null(size_min)){
    # filter minimum size
    data_crab <- data_crab %>%
                 dplyr::filter(SIZE_1MM >= size_min)

  }


  ## SIZE_MAX ------------------------------------------------------------------
  if(!is.null(size_max)){
    # filter maximum size
    data_crab <- data_crab %>%
                 dplyr::filter(SIZE_1MM <= size_max)
  }


  ## SHELL CONDITION -----------------------------------------------------------
  if(is.null(shell_condition)){
    shell_combos <- NA
  }

  if(!is.null(shell_condition)){
    # assign SHELL_TEXT
    data_crab <- data_crab %>%
                 dplyr::mutate(SHELL_TEXT = case_when(SHELL_CONDITION %in% 0:1 ~ "soft_molting",
                                                      SHELL_CONDITION == 2 ~ "new_hardshell",
                                                      SHELL_CONDITION == 3 ~ "oldshell",
                                                      SHELL_CONDITION %in% 4:5 ~ "very_oldshell"))

    if(TRUE %in% (shell_condition == "all_categories")){
      shell_combos <- c("soft_molting", "new_hardshell", "oldshell", "very_oldshell")
    }

    # filter categories
    if(TRUE %in% (shell_condition %in% c("soft_molting", "new_hardshell", "oldshell", "very_oldshell"))){
      data_crab <- data_crab %>%
                    dplyr::filter(SHELL_TEXT %in% shell_condition)
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
                 dplyr::filter(SEX == 2) %>%
                 dplyr::mutate(EGG_CONDITION_TEXT = case_when(EGG_CONDITION == 0 ~ "none",
                                                              EGG_CONDITION == 1 ~ "uneyed",
                                                              EGG_CONDITION == 2 ~ "eyed",
                                                              EGG_CONDITION == 3 ~ "dead",
                                                              EGG_CONDITION == 4 ~ "empty_cases",
                                                              EGG_CONDITION == 5 ~ "hatching"))

    if(TRUE %in% (egg_condition == "all_categories")){
      egg_combos <- c("none", "uneyed", "eyed", "dead", "empty_cases", "hatching")
    }

    # filter categories
    if(TRUE %in% (egg_condition %in% c("none", "uneyed", "eyed", "dead", "empty_cases", "hatching"))){
      data_crab <- data_crab %>%
                    dplyr::filter(EGG_CONDITION_TEXT %in% egg_condition)
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
                  dplyr::filter(SEX == 2) %>%
                  dplyr::mutate(CLUTCH_TEXT = case_when(CLUTCH_SIZE == 0 ~ "immature",
                                                        CLUTCH_SIZE == 1 ~ "mature_barren",
                                                        CLUTCH_SIZE == 2 ~ "trace",
                                                        CLUTCH_SIZE == 3 ~ "quarter",
                                                        CLUTCH_SIZE == 4 ~ "half",
                                                        CLUTCH_SIZE == 5 ~ "three_quarter",
                                                        CLUTCH_SIZE == 6 ~ "full"))

    if(TRUE %in% (clutch_size == "all_categories")){
      clutch_combos <- c("immature", "mature_barren", "trace", "quarter",
                         "half", "three_quarter", "full")
    }

    # filter categories
    if(TRUE %in% (clutch_size %in% c("immature", "mature_barren", "trace", "quarter",
                                     "half", "three_quarter", "full"))){
      data_crab <- data_crab %>%
                   dplyr::filter(CLUTCH_TEXT %in% clutch_size)
      clutch_combos <- clutch_size
    }

    # append group columns
    group_cols <- append(group_cols, "CLUTCH_TEXT")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(clutch_combos = clutch_combos))


  ## 1MM BINS ------------------------------------------------------------------
  if(bin_1mm == FALSE){
    bin_combos <- NA
  }

  if(bin_1mm == TRUE){
    # set maximum size for 1mm bins to maximum size in data if no other maximum size is specified
    if(is.null(size_max)){
      bin_max <- max(data_crab$SIZE_1MM, na.rm = T)
    } else{
      bin_max <- size_max
    }

    # set minimum size for 1mm bins to 1mm if no other minimum size is specified
    if(is.null(size_min)){
      bin_min <- 1
    } else{
      bin_min <- size_min
    }

    # assign range of bin sizes to expand over
    bin_combos <- bin_min:bin_max

    # append group columns
    group_cols <- append(group_cols, "SIZE_1MM")
  }

  # append expand combinations
  expand_combos <- append(expand_combos, list(bin_combos = bin_combos))


  return(list(group_cols = group_cols,
              expand_combos = expand_combos,
              specimen_data = data_crab))
}
