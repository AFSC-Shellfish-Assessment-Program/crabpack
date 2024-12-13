#' Calculate index of station-, district-, or region-level abundance and biomass
#'
#' @description This function calculates indices of abundance and biomass (mt and lbs)
#'              at the station-, district-, or region-level for a given crab species.
#'              Optional arguments also allow these indices to be calculated for subsets of
#'              crab by biometric categories such as size, sex, maturity, and shell condition.
#'
#' @param crab_data object created from `crabpack::get_data()`.
#' @param species character string. One of c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR").
#' @param region character string describing the region of interest. One of
#'               c("EBS", "NBS"). Defaults to "EBS" for Eastern Bering Sea.
#' @param district character string. One or many of c("ALL", "BB", "NORTH", "NS",
#'                 "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173").
#'                 Defaults to "ALL" districts within the selected region if not specified.
#' @param years numeric or integer vector of years.
#' @param sex character string. One or both of c("male", "female"). Optional,
#'            "male" or "female" only will provide estimates for the selected sex,
#'            specifying both "male" and "female" will provide estimates for
#'            each of the selected sexes.
#' @param size_min integer. Optional, desired lower range of crab size (inclusive).
#' @param size_max integer. Optional, desired upper range of crab size (inclusive).
#' @param crab_category character string. One or many of c("legal_male", "preferred_male",
#'                      "mature_male", "immature_male", "mature_female", "immature_female",
#'                      "all_categories"). Optional, specifying this parameter will provide
#'                      estimates for each of the selected categories; "all_categories" will
#'                      provide estimates for each relevant category for the given species.
#'                      If using a female category, maturity will be based on morphometric
#'                      maturity (default "morphometric" for the optional `female_maturity`
#'                      parameter). Set `female_maturity` to "cutline" if you want to define
#'                      female maturity based on ADF&G size cutlines.
#' @param female_maturity character string. One of c("morphometric", "cutline").
#'                        Defaults to "morphometric" maturity for female crab. Morphometric
#'                        maturity biomass and abundance estimates are not available for
#'                        male crab at this time.
#' @param shell_condition character string. One or many of c("soft_molting", "new_hardshell",
#'                        "oldshell", "very_oldshell", "all_categories"). Optional,
#'                        specifying this parameter will provide estimates for each of the
#'                        selected shell conditions; "all_categories" will provide estimates
#'                        for each available shell condition category.
#' @param egg_condition character string. One or many of c("none", "uneyed", "eyed",
#'                      "dead", "empty_cases", "hatching", "all_categories"). Optional,
#'                      specifying this parameter will provide estimates for each of the
#'                      selected egg conditions; "all_categories" will provide estimates
#'                      for each available egg condition category. Note that specifying
#'                      `egg_condition` will return only female specimens in the final output.
#' @param clutch_size character string. One or many of c("immature", "mature_barren",
#'                    "trace", "quarter", "half", "three_quarter", "full", "all_categories").
#'                    Optional, specifying this parameter will provide estimates for each of
#'                    the selected clutch sizes; "all_categories" will provide estimates for
#'                    each available clutch size category. Note that specifying `clutch_size`
#'                    will return only female specimens in the final output.
#' @param bin_1mm boolean T/F. If TRUE, estimates will be provided for each 1mm bin
#'                within the size range specified in `size_min` and/or `size_max`,
#'                or for the full range of observed sizes in the data. Defaults to FALSE.
#' @param spatial_level character string. One of c("station", "district", "region").
#'                      Describes the spatial resolution of biomass and abundance output,
#'                      either kept at the haul level, or aggregated to the district (default)
#'                      or regional scale.
#' @param replace_retow boolean T/F. If TRUE, replace female Bristol Bay red king crab observations
#'                      with resampled data (haul type = 17) in years when a Bristol Bay
#'                      retow took place. Defaults to TRUE, please use care when interpreting
#'                      BBRKC outputs if using FALSE.
#'
#' @return A data frame with area swept-expanded estimates of station-, district-, or region-level abundance,
#'         biomass (mt), and biomass (lbs) by year.
#'
#' @export
#'


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
                          female_maturity = c("morphometric", "cutline")[1],
                          shell_condition = NULL,
                          egg_condition = NULL,
                          clutch_size = NULL,
                          bin_1mm = FALSE,
                          spatial_level = c("station", "district", "region")[2],
                          #output = c("abundance", "biomass_mt", "biomass_lbs")[1],
                          replace_retow = TRUE){

  # call calc_cpue()
  cpue <- calc_cpue(crab_data = crab_data,
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

  station_cpue <- cpue$cpue
  groups_out <- cpue$group_cols

  # LOOP OVER YEARS??

  ## NEED TO CONSIDER SPATIAL LEVEL!!
  # 'if's in calculations/aggregating --> break into steps for outputs....
  ## ERROR if multiple spatial levels selected!!

  ## Calculate abundance and biomass -------------
  #Sum across haul, scale abundance, biomass, and variance to strata, then sum across strata and calc CIs
  bio_abund_df <- station_cpue %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'STATION_ID', 'STRATUM', groups_out, 'TOTAL_AREA')))) %>%
                  dplyr::summarise(COUNT = sum(COUNT), CPUE = sum(CPUE), CPUE_KG = sum(CPUE_KG)) %>%
                  # Scale to abundance by strata
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'STRATUM', groups_out)))) %>%
                  dplyr::reframe(AREA = TOTAL_AREA,
                                 MEAN_CPUE = mean(CPUE),
                                 N_CPUE = dplyr::n(),
                                 VAR_CPUE = (stats::var(CPUE)*(AREA^2))/N_CPUE,
                                 MEAN_CPUE_MT = mean(CPUE_MT),
                                 N_CPUE_MT = dplyr::n(),
                                 VAR_CPUE_MT = (stats::var(CPUE_MT)*(AREA^2))/N_CPUE_MT,
                                 MEAN_CPUE_LBS = mean(CPUE_LBS),
                                 N_CPUE_LBS = dplyr::n(),
                                 VAR_CPUE_LBS = (stats::var(CPUE_LBS)*(AREA^2))/N_CPUE_LBS,
                                 ABUNDANCE = (MEAN_CPUE * AREA),
                                 BIOMASS_MT = (MEAN_CPUE_MT * AREA),
                                 BIOMASS_LBS = (MEAN_CPUE_LBS * AREA),
                                 N_STATIONS = length(unique(STATION_ID))) %>%
                  dplyr::distinct() %>%
                  # Sum across strata
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', groups_out)))) %>%
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
                  dplyr::mutate(N_STATIONS = ifelse((YEAR == 2000
                                                     & district == "BB"), 135, N_STATIONS)) %>%
                  dplyr::ungroup()# %>%
                # complete.cases()

  ## format output better!!
  # bio_abund_out <- bio_abund_out_EBS %>%
  #   mutate(#SPECIES_CODE = 69323,
  #          SPECIES = species,
  #          DISTRICT = district,
  #          SEX = "MALE",
  #          SIZE_GROUP = paste(SEX, SIZE_CLASS_MM, sep = "_"),
  #          SHELL_CONDITION = "",
  #          MATURITY = "") %>%
  #   select(YEAR, SPECIES, DISTRICT, CATEGORY, MATURITY, SEX, SIZE_CLASS_MM, SHELL_CONDITION,
  #          ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI,
  #          BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI,
  #          BIOMASS_LBS, BIOMASS_LBS_CV, BIOMASS_LBS_CI)
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
