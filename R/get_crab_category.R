#' Assign maturity-sex categories based on stock-specific size definitions
#' #' NEEDS MAT_LOOKUP!
#' @description TBD
#'
#' @inheritParams calc_bioabund
#'
#' @return Returns a data frame with crab specimen data and a CATEGORY column with
#'         stock-specific maturity designations
#'
#' @export get_crab_category


get_crab_category <- function(crab_data = NULL,
                              species = NULL,
                              region = c("EBS", "NBS")[1],
                              district = NULL,
                              crab_category = NULL,
                              female_maturity = c("morphological", "cutline")[1]) {

  ## Get crab category size definitions
  sizegroups <- crab_data$sizegroups


  ## Define 'maturity district' to accommodate unstratified BKC
  # BKC below 58.65 deg lat --> PRIB; BKC above 58.65 deg lat --> STMATT
  # all other species, maturity tied to defined districts
  crab_data <- crab_data %>%
               dplyr::mutate(MAT_DISTRICT = .data$DISTRICT,
                             MAT_DISTRICT = dplyr::case_when((.data$DISTRICT == "UNSTRAT" & .data$MID_LATITUDE < 58.65) ~ "PRIB",
                                                             (.data$DISTRICT == "UNSTRAT" & .data$MID_LATITUDE >= 58.65) ~ "STMATT",
                                                             TRUE ~ .data$MAT_DISTRICT))


  ## Assign female maturity -- default to "morphological" ----------------------
  if(female_maturity == "cutline"){
    # look up cutlines for mature and immature females
    if(!species == "HAIR"){
      imm_fem <- sizegroups %>%
                 dplyr::filter(.data$SEX == 2,
                               .data$CATEGORY == "immature_female")
      mat_fem <- sizegroups %>%
                 dplyr::filter(.data$SEX == 2,
                               .data$CATEGORY == "mature_female")

      female_dat <- crab_data %>%
                    dplyr::filter(.data$SEX == 2) %>%
                    dplyr::mutate(CATEGORY = case_when((.data$SIZE >= mat_fem$SIZE_MIN[mat_fem$DISTRICT == .data$MAT_DISTRICT]) ~ "mature_female",
                                                       (.data$SIZE <= imm_fem$SIZE_MAX[imm_fem$DISTRICT == .data$MAT_DISTRICT]) ~ "immature_female"))
      } else{
        # no female maturity cutline for hair crab, just "female"
        female_dat <- crab_data %>%
                      dplyr::filter(.data$SEX == 2) %>%
                      dplyr::mutate(CATEGORY = "female")
      }
  } else{
    # define female maturity by clutch size
    female_dat <- crab_data %>%
                  dplyr::filter(.data$SEX == 2) %>%
                  dplyr::mutate(CATEGORY = case_when(.data$CLUTCH_SIZE >= 1 ~ "mature_female",
                                                     .data$CLUTCH_SIZE == 0 ~ "immature_female"))
  }


  ## Assign male maturity, and legal/industry-preferred sizes if applicable ----
  # Assign immature, mature, legal for King Crab spp.
  if(species %in% c("RKC", "BKC")){
    # mature/immature males
    imm_male <- sizegroups %>%
                dplyr::filter(.data$SEX == 1,
                              .data$CATEGORY == "immature_male")
    mat_male <- sizegroups %>%
                dplyr::filter(.data$SEX == 1,
                              .data$CATEGORY == "mature_male")

    mat_male_dat <- crab_data %>%
                    dplyr::filter(.data$SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((.data$SIZE >= mat_male$SIZE_MIN[mat_male$DISTRICT == .data$MAT_DISTRICT]) ~ "mature_male",
                                                       (.data$SIZE <= imm_male$SIZE_MAX[imm_male$DISTRICT == .data$MAT_DISTRICT]) ~ "immature_male"))

    # legal males
    leg_male <- sizegroups %>%
                dplyr::filter(.data$SEX == 1,
                              .data$CATEGORY == "legal_male")

    leg_male_dat <- crab_data %>%
                    dplyr::filter(.data$SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((.data$SIZE >= leg_male$SIZE_MIN[leg_male$DISTRICT == .data$MAT_DISTRICT]) ~ "legal_male"))

    male_dat <- rbind(mat_male_dat, leg_male_dat)
  }

  # Assign small, large, legal, preferred for Chionoecetes spp.
  if(species %in% c("SNOW", "TANNER", "HYBRID")){
    # large/small males
    small_male <- sizegroups %>%
                  dplyr::filter(.data$SEX == 1,
                                .data$CATEGORY == "small_male")
    large_male <- sizegroups %>%
                  dplyr::filter(.data$SEX == 1,
                                .data$CATEGORY == "large_male")

    mat_male_dat <- crab_data %>%
                    dplyr::filter(.data$SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((.data$SIZE >= large_male$SIZE_MIN[large_male$DISTRICT == .data$MAT_DISTRICT]) ~ "large_male",
                                                       (.data$SIZE <= small_male$SIZE_MAX[small_male$DISTRICT == .data$MAT_DISTRICT]) ~ "small_male"))

    # legal males
    leg_male <- sizegroups %>%
                dplyr::filter(.data$SEX == 1,
                              .data$CATEGORY == "legal_male")

    leg_male_dat <- crab_data %>%
                    dplyr::filter(.data$SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((.data$SIZE >= leg_male$SIZE_MIN[leg_male$DISTRICT == .data$MAT_DISTRICT]) ~ "legal_male"))


    # industry-preferred males
    pref_male <- sizegroups %>%
                 dplyr::filter(.data$SEX == 1,
                               .data$CATEGORY == "preferred_male")

    ind_pref_dat <- crab_data %>%
                    dplyr::filter(.data$SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((.data$SIZE >= pref_male$SIZE_MIN[pref_male$DISTRICT == .data$MAT_DISTRICT]) ~ "preferred_male"))


    male_dat <- rbind(mat_male_dat, leg_male_dat, ind_pref_dat)
  }

  # Assign sublegal, legal for Hair Crab
  if(species == "HAIR"){
    # legal/sublegal males
    sub_male <- sizegroups %>%
                dplyr::filter(.data$SEX == 1,
                              .data$CATEGORY == "sublegal_male")
    leg_male <- sizegroups %>%
                dplyr::filter(.data$SEX == 1,
                              .data$CATEGORY == "legal_male")

    male_dat <- crab_data %>%
                dplyr::filter(.data$SEX == 1) %>%
                dplyr::mutate(CATEGORY = case_when((.data$SIZE >= leg_male$SIZE_MIN[leg_male$DISTRICT == .data$MAT_DISTRICT]) ~ "legal_male",
                                                   (.data$SIZE <= sub_male$SIZE_MAX[sub_male$DISTRICT == .data$MAT_DISTRICT]) ~ "sublegal_male"))
  }


  ## Bind male and female data
  category_dat <- rbind(male_dat, female_dat) %>%
                  dplyr::filter(!is.na(.data$CATEGORY)) %>%
                  dplyr::select(-.data$MAT_DISTRICT)

  return(category_dat)
}
