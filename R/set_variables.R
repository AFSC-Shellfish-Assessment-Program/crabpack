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
                dplyr::mutate(.data$SEX_TEXT = case_when(.data$SEX == 1 ~ "male",
                                                         .data$SEX == 2 ~ "female"))

  # Filter sex if only want one
  if(!is.null(sex)){
    if(!sex %in% c("all", TRUE))
      data_crab2 <- data_crab2 %>%
                    dplyr::filter(.data$SEX_TEXT %in% sex)
  }


  # SIZE_MIN ----
  if(!is.null(size_min)){
    data_crab2 <- data_crab2 %>%
                  dplyr::filter(.data$SIZE_1MM >= size_min)

  }

  # SIZE_MAX:  ----
  if(!is.null(size_max)){
    data_crab2 <- data_crab2 %>%
                  dplyr::filter(.data$SIZE_1MM <= size_max)
  }

  ## CRAB CATEGORY??
  # ###MATURITY: Define/filter if specified in function ----
  # if(!is.null(mat_sex)){
  #   # assign maturity
  #   ## maybe a note her if want "mature male" for chionoecetes (for example),
  #   ## say "we can't actually know maturity, giving "large male"/"small male" instead...
  #   data_crab2 <- get_crab_category(crab_dat = data_crab2, stock = stock) %>%
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
      dplyr::mutate(.data$SHELL_TEXT = case_when(.data$SHELL_CONDITION %in% 0:1 ~ "soft molting",
                                                 .data$SHELL_CONDITION == 2 ~ "new hardshell",
                                                 .data$SHELL_CONDITION == 3 ~ "oldshell",
                                                 .data$SHELL_CONDITION %in% 4:5 ~ "very oldshell"))

    # filter by specific category
    if(TRUE %in% (shell_condition %in% c(0:5))){
      data_crab2 <- data_crab2 %>%
                    dplyr::filter(.data$SHELL_CONDITION %in% shell_condition)
    }

    if(TRUE %in% (shell_condition %in% c("soft molting", "new hardshell", "oldshell", "very oldshell"))){
      data_crab2 <- data_crab2 %>%
                    dplyr::filter(.data$SHELL_TEXT %in% shell_condition)
    }

    group_cols <- append(group_cols, "SHELL_TEXT")
  }


  # EGG CONDITION: Define/filter if specified in function ----
  if(!is.null(egg_condition)){
    data_crab2 <- data_crab2 %>%
      dplyr::filter(SEX == 2) %>% #HAUL_TYPE != 17,
      dplyr::mutate(.data$EGG_CONDITION_TEXT = case_when(.data$EGG_CONDITION == 0 ~ "none",
                                                         .data$EGG_CONDITION == 1 ~ "uneyed",
                                                         .data$EGG_CONDITION == 2 ~ "eyed",
                                                         .data$EGG_CONDITION == 3 ~ "dead",
                                                         .data$EGG_CONDITION == 4 ~ "empty cases",
                                                         .data$EGG_CONDITION == 5 ~ "hatching",
                                                         TRUE ~ "unknown"))

    if(TRUE %in% (egg_condition %in% c(0:5))){
      data_crab2 <- data_crab2 %>% dplyr::filter(.data$EGG_CONDITION %in% egg_condition)
    }

    if(TRUE %in% (egg_condition %in% c("none", "uneyed", "eyed", "dead",
                                       "empty cases", "hatching", "unknown"))){
      data_crab2 <- data_crab2 %>% dplyr::filter(.data$EGG_CONDITION_TEXT %in% egg_condition)
    }

    group_cols <- append(group_cols, "EGG_CONDITION_TEXT")
  }


  # CLUTCH SIZE: Define/filter if specified in function ----
  if(!is.null(clutch_size)){
    data_crab2 <- data_crab2 %>%
      dplyr::filter(.data$SEX == 2) %>% #HAUL_TYPE != 17,
      dplyr::mutate(.data$CLUTCH_TEXT = case_when(.data$CLUTCH_SIZE == 0 ~ "immature",
                                                  .data$CLUTCH_SIZE == 1 ~ "mature barren",
                                                  .data$CLUTCH_SIZE == 2 ~ "trace",
                                                  .data$CLUTCH_SIZE == 3 ~ "quarter",
                                                  .data$CLUTCH_SIZE == 4 ~ "half",
                                                  .data$CLUTCH_SIZE == 5 ~ "three quarter",
                                                  .data$CLUTCH_SIZE == 6 ~ "full",
                                                  ((.data$CLUTCH_SIZE == 999) | is.na(.data$CLUTCH_SIZE) == TRUE) ~ "unknown"))

    if(TRUE %in% (clutch_size %in% c(0:6))){
      data_crab2 <- data_crab2 %>% dplyr::filter(.data$CLUTCH_SIZE %in% clutch_size)
    }

    if(TRUE %in% (clutch_size %in% c("immature", "mature barren", "trace", "quarter",
                                     "half", "three quarter", "full", "unknown"))){
      data_crab2 <- data_crab2 %>% dplyr::filter(.data$CLUTCH_TEXT %in% clutch_size)
    }

    group_cols <- append(group_cols, "CLUTCH_TEXT")
  }


  # 1MM BINS: Define/filter if specified in function ----
  if(!is.null(bin_1mm)){
    data_crab2 <- data_crab2 %>% dplyr::mutate(.data$BIN_1MM = floor(.data$SIZE_1MM))
    group_cols <- append(group_cols, "BIN_1MM")
  }



  return(list(data_crab2, group_cols)) #, size_max)) #size_max if null is defined in calc_cpue now...

}
