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
#' @inheritParams calc_bioabund
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

  # Pull specimen dataframe, remove 0-catch stations
  specimen_dat <- crab_data$specimen %>%
                  dplyr::filter(!is.na(SEX))


  ## Define 'maturity district' to accommodate unstratified BKC
  # BKC below 58.65 deg lat --> PRIB; BKC above 58.65 deg lat --> STMATT
  # all other species, maturity tied to defined districts
  specimen_dat <- specimen_dat %>%
                  dplyr::mutate(MAT_DISTRICT = DISTRICT,
                                MAT_DISTRICT = dplyr::case_when((DISTRICT == "UNSTRAT" & LATITUDE < 58.65) ~ "PRIB",
                                                                (DISTRICT == "UNSTRAT" & LATITUDE >= 58.65) ~ "STMATT",
                                                                TRUE ~ MAT_DISTRICT))


  ## Assign female maturity -- default to "morphometric" ----------------------
  if(female_maturity == "cutline"){
    # look up cutlines for mature and immature females
    if(!species == "HAIR"){

      female_dat <- specimen_dat %>%
                    dplyr::filter(SEX == 2) %>%
                    dplyr::left_join(., sizegroups %>%
                                        dplyr::filter(SEX == 2) %>%
                                        dplyr::rename(MAT_DISTRICT = DISTRICT),
                                     relationship = "many-to-many",
                                     by = c('REGION', 'SPECIES_CODE', 'SPECIES', 'SEX', 'MAT_DISTRICT')) %>%
                    dplyr::mutate(RANGE = ifelse(SIZE_1MM >= SIZE_MIN & SIZE_1MM <= SIZE_MAX, 1, NA)) %>%
                    dplyr::filter(!is.na(RANGE)) %>%
                    dplyr::select(-c('RANGE', 'SIZE_MIN', 'SIZE_MAX'))
      } else{
        # no female maturity cutline for hair crab, just "female"
        female_dat <- specimen_dat %>%
                      dplyr::filter(SEX == 2) %>%
                      dplyr::mutate(CATEGORY = "female")
      }
  } else{
    # define female maturity by clutch size
    female_dat <- specimen_dat %>%
                  dplyr::filter(SEX == 2) %>%
                  dplyr::mutate(CATEGORY = dplyr::case_when(CLUTCH_SIZE >= 1 ~ "mature_female",
                                                            CLUTCH_SIZE == 0 ~ "immature_female"))
  }


  ## Assign male maturity, and legal/industry-preferred sizes if applicable ----
  # Assign immature, mature, legal for King Crab spp.
  if(species %in% c("RKC", "BKC")){
    # immature, mature, legal males
    male_dat <- specimen_dat %>%
                dplyr::filter(SEX == 1) %>%
                dplyr::left_join(., sizegroups %>%
                                    dplyr::filter(SEX == 1) %>%
                                    dplyr::rename(MAT_DISTRICT = DISTRICT),
                                 relationship = "many-to-many",
                                 by = c('REGION', 'SPECIES_CODE', 'SPECIES', 'SEX', 'MAT_DISTRICT')) %>%
                dplyr::mutate(RANGE = ifelse(SIZE_1MM >= SIZE_MIN & SIZE_1MM <= SIZE_MAX, 1, NA)) %>%
                dplyr::filter(!is.na(RANGE)) %>%
                dplyr::select(-c('RANGE', 'SIZE_MIN', 'SIZE_MAX'))
  }

  # Assign small, large, legal, preferred for Chionoecetes spp.
  if(species %in% c("SNOW", "TANNER", "HYBRID")){
    # small, large, legal, preferred males
    male_dat <- specimen_dat %>%
                dplyr::filter(SEX == 1) %>%
                dplyr::left_join(., sizegroups %>%
                                    dplyr::filter(SEX == 1) %>%
                                    dplyr::rename(MAT_DISTRICT = DISTRICT),
                                 relationship = "many-to-many",
                                 by = c('REGION', 'SPECIES_CODE', 'SPECIES', 'SEX', 'MAT_DISTRICT')) %>%
                dplyr::mutate(RANGE = ifelse(SIZE_1MM >= SIZE_MIN & SIZE_1MM <= SIZE_MAX, 1, NA)) %>%
                dplyr::filter(!is.na(RANGE)) %>%
                dplyr::select(-c('RANGE', 'SIZE_MIN', 'SIZE_MAX'))
  }

  # Assign sublegal, legal for Hair Crab
  if(species == "HAIR"){
    # legal, sublegal males
    male_dat <- specimen_dat %>%
                dplyr::filter(SEX == 1) %>%
                dplyr::left_join(., sizegroups %>%
                                    dplyr::filter(SEX == 1) %>%
                                    dplyr::rename(MAT_DISTRICT = DISTRICT),
                                 relationship = "many-to-many",
                                 by = c('REGION', 'SPECIES_CODE', 'SPECIES', 'SEX', 'MAT_DISTRICT')) %>%
                dplyr::mutate(RANGE = ifelse(SIZE_1MM >= SIZE_MIN & SIZE_1MM <= SIZE_MAX, 1, NA)) %>%
                dplyr::filter(!is.na(RANGE)) %>%
                dplyr::select(-c('RANGE', 'SIZE_MIN', 'SIZE_MAX'))
    }


  ## Bind male and female data
  category_dat <- rbind(male_dat, female_dat) %>%
                  dplyr::select(-MAT_DISTRICT)

  return(category_dat)
}
