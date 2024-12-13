#' Assign size-sex-maturity categories based on species- and district-specific size definitions
#'
#' @description A utility function to assign standard size-sex-maturity categories to crab
#'              specimen data. Standard categories for Red King Crab and Blue King Crab are
#'              `immature_male`, `mature_male`, `legal_male`, `immature_female`, and `mature_female`.
#'              Categories for Tanner Crab, Snow Crab, and Hybrid Crab are `small_male`, `large_male`,
#'              `legal_male`, `preferred_male`, `immature_female`, and `mature_female`. Categories
#'              for Hair Crab are `sublegal_male`, `legal_male`, and `female`.
#'
#' @param crab_category Character string. One or many of
#'                      `c("mature_male", "large_male", "legal_male", "preferred_male", "immature_male", "small_male", "sublegal_male", "mature_female", "immature_female", "all_categories")`.
#'                      Optional, specifying this parameter will provide estimates
#'                      for each of the selected categories; `"all_categories"` will
#'                      provide estimates for each relevant category for the given species.
#'                      If using a female category, maturity will be based on morphometric
#'                      maturity (default `female_maturity = "morphometric"`). Set
#'                      `female_maturity = "cutline"` if you want to define female maturity
#'                      based on ADF&G size cutlines.
#'
#' @return A data frame with crab specimen data and an additional "CATEGORY" column
#'         with stock-specific maturity designations.
#'
#' @export
#'


set_crab_category <- function(crab_data = NULL,
                              species = NULL,
                              region = c("EBS", "NBS")[1],
                              district = NULL,
                              crab_category = NULL,
                              female_maturity = c("morphometric", "cutline")[1]) {

  ## Get crab category size definitions
  sizegroups <- crab_data$sizegroups


  ## Define 'maturity district' to accommodate unstratified BKC
  # BKC below 58.65 deg lat --> PRIB; BKC above 58.65 deg lat --> STMATT
  # all other species, maturity tied to defined districts
  crab_data <- crab_data %>%
               dplyr::mutate(MAT_DISTRICT = DISTRICT,
                             MAT_DISTRICT = dplyr::case_when((DISTRICT == "UNSTRAT" & MID_LATITUDE < 58.65) ~ "PRIB",
                                                             (DISTRICT == "UNSTRAT" & MID_LATITUDE >= 58.65) ~ "STMATT",
                                                             TRUE ~ MAT_DISTRICT))


  ## Assign female maturity -- default to "morphometric" ----------------------
  if(female_maturity == "cutline"){
    # look up cutlines for mature and immature females
    if(!species == "HAIR"){
      imm_fem <- sizegroups %>%
                 dplyr::filter(SEX == 2,
                               CATEGORY == "immature_female")
      mat_fem <- sizegroups %>%
                 dplyr::filter(SEX == 2,
                               CATEGORY == "mature_female")

      female_dat <- crab_data %>%
                    dplyr::filter(SEX == 2) %>%
                    dplyr::mutate(CATEGORY = case_when((SIZE >= mat_fem$SIZE_MIN[mat_fem$DISTRICT == MAT_DISTRICT]) ~ "mature_female",
                                                       (SIZE <= imm_fem$SIZE_MAX[imm_fem$DISTRICT == MAT_DISTRICT]) ~ "immature_female"))
      } else{
        # no female maturity cutline for hair crab, just "female"
        female_dat <- crab_data %>%
                      dplyr::filter(SEX == 2) %>%
                      dplyr::mutate(CATEGORY = "female")
      }
  } else{
    # define female maturity by clutch size
    female_dat <- crab_data %>%
                  dplyr::filter(SEX == 2) %>%
                  dplyr::mutate(CATEGORY = case_when(CLUTCH_SIZE >= 1 ~ "mature_female",
                                                     CLUTCH_SIZE == 0 ~ "immature_female"))
  }


  ## Assign male maturity, and legal/industry-preferred sizes if applicable ----
  # Assign immature, mature, legal for King Crab spp.
  if(species %in% c("RKC", "BKC")){
    # mature/immature males
    imm_male <- sizegroups %>%
                dplyr::filter(SEX == 1,
                              CATEGORY == "immature_male")
    mat_male <- sizegroups %>%
                dplyr::filter(SEX == 1,
                              CATEGORY == "mature_male")

    mat_male_dat <- crab_data %>%
                    dplyr::filter(SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((SIZE >= mat_male$SIZE_MIN[mat_male$DISTRICT == MAT_DISTRICT]) ~ "mature_male",
                                                       (SIZE <= imm_male$SIZE_MAX[imm_male$DISTRICT == MAT_DISTRICT]) ~ "immature_male"))

    # legal males
    leg_male <- sizegroups %>%
                dplyr::filter(SEX == 1,
                              CATEGORY == "legal_male")

    leg_male_dat <- crab_data %>%
                    dplyr::filter(SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((SIZE >= leg_male$SIZE_MIN[leg_male$DISTRICT == MAT_DISTRICT]) ~ "legal_male"))

    male_dat <- rbind(mat_male_dat, leg_male_dat)
  }

  # Assign small, large, legal, preferred for Chionoecetes spp.
  if(species %in% c("SNOW", "TANNER", "HYBRID")){
    # large/small males
    small_male <- sizegroups %>%
                  dplyr::filter(SEX == 1,
                                CATEGORY == "small_male")
    large_male <- sizegroups %>%
                  dplyr::filter(SEX == 1,
                                CATEGORY == "large_male")

    mat_male_dat <- crab_data %>%
                    dplyr::filter(SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((SIZE >= large_male$SIZE_MIN[large_male$DISTRICT == MAT_DISTRICT]) ~ "large_male",
                                                       (SIZE <= small_male$SIZE_MAX[small_male$DISTRICT == MAT_DISTRICT]) ~ "small_male"))

    # legal males
    leg_male <- sizegroups %>%
                dplyr::filter(SEX == 1,
                              CATEGORY == "legal_male")

    leg_male_dat <- crab_data %>%
                    dplyr::filter(SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((SIZE >= leg_male$SIZE_MIN[leg_male$DISTRICT == MAT_DISTRICT]) ~ "legal_male"))


    # industry-preferred males
    pref_male <- sizegroups %>%
                 dplyr::filter(SEX == 1,
                               CATEGORY == "preferred_male")

    ind_pref_dat <- crab_data %>%
                    dplyr::filter(SEX == 1) %>%
                    dplyr::mutate(CATEGORY = case_when((SIZE >= pref_male$SIZE_MIN[pref_male$DISTRICT == MAT_DISTRICT]) ~ "preferred_male"))


    male_dat <- rbind(mat_male_dat, leg_male_dat, ind_pref_dat)
  }

  # Assign sublegal, legal for Hair Crab
  if(species == "HAIR"){
    # legal/sublegal males
    sub_male <- sizegroups %>%
                dplyr::filter(SEX == 1,
                              CATEGORY == "sublegal_male")
    leg_male <- sizegroups %>%
                dplyr::filter(SEX == 1,
                              CATEGORY == "legal_male")

    male_dat <- crab_data %>%
                dplyr::filter(SEX == 1) %>%
                dplyr::mutate(CATEGORY = case_when((SIZE >= leg_male$SIZE_MIN[leg_male$DISTRICT == MAT_DISTRICT]) ~ "legal_male",
                                                   (SIZE <= sub_male$SIZE_MAX[sub_male$DISTRICT == MAT_DISTRICT]) ~ "sublegal_male"))
  }


  ## Bind male and female data
  category_dat <- rbind(male_dat, female_dat) %>%
                  dplyr::filter(!is.na(CATEGORY)) %>%
                  dplyr::select(-MAT_DISTRICT)

  return(category_dat)
}
