#' Set optional variables and subset data
#'
#' @description A utility function to...
#'
#' @inheritParams calc_bioabund
#'
#' @return ....
#'
#' @export set_variables

set_variables <- function(data_crab = NULL,
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

  ## Add biometrics definitions and filtering ----
  # define set of columns to 'group_by()' based on whether or not there is "shellcond", "size", or other modifiers defined in the function
  group_cols <- c()


  # SEX: Define/filter if specified in function ----
  # First, always include initially to carry through for BBRKC resample filtering
  data_crab2 <- data_crab %>%
                mutate(SEX_TEXT = case_when(SEX == 1 ~ "male",
                                            SEX == 2 ~ "female"))

  # Filter sex if only want one
  if(!is.null(sex)){
    if(!sex %in% c("all", TRUE))
      data_crab2 <- data_crab2 %>%
                    dplyr::filter(SEX_TEXT %in% sex)
  }


  # SIZE_MIN ----
  if(!is.null(size_min)){
    data_crab2 <- data_crab2 %>%
                  dplyr::filter(SIZE_1MM >= size_min)

  }

  # SIZE_MAX:  ----
  if(!is.null(size_max)){
    data_crab2 <- data_crab2 %>%
                  dplyr::filter(SIZE_1MM <= size_max)
  }

  ## CRAB CATEGORY??
  # ###MATURITY: Define/filter if specified in function ----
  # if(!is.null(mat_sex)){
  #   # assign maturity
  #   ## maybe a note her if want "mature male" for chionoecetes (for example),
  #   ## say "we can't actually know maturity, giving "large male"/"small male" instead...
  #   data_crab2 <- get_maturity(crab_dat = data_crab2, stock = stock) %>%
  #     filter(!is.na(MAT_SEX))
  #
  #   # filter by specific category
  #   if(!mat_sex %in% c("all", TRUE)){
  #     data_crab2 <- data_crab2 %>% dplyr::filter(MAT_SEX %in% mat_sex)
  #   }
  #
  #   group_cols <- append(group_cols, "MAT_SEX")
  # }


  # SHELL CONDITION: Define/filter if specified in function ----
  if(!is.null(shell_condition)){
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


  # EGG CONDITION: Define/filter if specified in function ----
  if(!is.null(egg_condition)){
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


  # CLUTCH SIZE: Define/filter if specified in function ----
  if(!is.null(clutch_size)){
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

    if(TRUE %in% (clutch_size %in% c("immature", "mature barren", "trace", "quarter",
                                     "half", "three quarter", "full", "unknown"))){
      data_crab2 <- data_crab2 %>% dplyr::filter(CLUTCH_TEXT %in% clutch_size)
    }

    group_cols <- append(group_cols, "CLUTCH_TEXT")
  }


  # 1MM BINS: Define/filter if specified in function ----
  if(!is.null(bin_1mm)){
    data_crab2 <- data_crab2 %>% dplyr::mutate(BIN_1MM = floor(SIZE_1MM))
    group_cols <- append(group_cols, "BIN_1MM")
  }



  return(list(data_crab2, group_cols)) #, size_max)) #size_max if null is defined in calc_cpue now...

}
