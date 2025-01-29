#' Calculate index of station-, district-, or region-level abundance and biomass
#'
#' @description This function calculates indices of abundance and biomass (mt and lbs)
#'              at the station-, district-, or region-level for a given crab species.
#'              Optional arguments also allow these indices to be calculated for subsets of
#'              crab by biometric categories such as size, sex, maturity, and shell condition.
#'
#' @param crab_data Object created from `crabpack::get_specimen_data()`.
#' @param species Character string. One of `c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR")`.
#' @param region Character string describing the region of interest. One of
#'               `c("EBS", "NBS"`). Defaults to `"EBS"` for Eastern Bering Sea.
#' @param district Character string. One or many of `c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173")`.
#'                 Defaults to `"ALL"` districts within the selected region if not specified.
#' @param years Numeric or integer vector of years. Default output is all available years in data.
#' @param sex Character string. One or both of `c("male", "female")`. Optional,
#'            `"male"` or `"female"` only will provide estimates for the selected sex,
#'            specifying both `"male"` and `"female"` will provide estimates for
#'            each of the selected sexes.
#' @param size_min Integer. Optional, desired lower range of crab size (inclusive).
#' @param size_max Integer. Optional, desired upper range of crab size (inclusive).
#' @param crab_category Character string. One or many of
#'                      `c("mature_male", "large_male", "legal_male", "preferred_male", "immature_male", "small_male", "sublegal_male", "mature_female", "immature_female", "all_categories")`.
#'                      Optional, specifying this parameter will provide estimates
#'                      for each of the selected categories; `"all_categories"` will
#'                      provide estimates for each relevant category for the given species.
#'                      If using a female category, maturity will be based on morphometric
#'                      maturity (default `female_maturity = "morphometric"`). Set
#'                      `female_maturity = "cutline"` if you want to define female maturity
#'                      based on ADF&G size cutlines. See `crabpack::set_crab_category()`
#'                      documentation for description of species-specific category combinations.
#' @param female_maturity Character string. One of `c("morphometric", "cutline")`.
#'                        Defaults to `"morphometric"` maturity for female crab. Morphometric
#'                        maturity biomass and abundance estimates are not available for
#'                        male crab at this time.
#' @param shell_condition Character string. One or many of `c("soft_molting", "new_hardshell", "oldshell", "very_oldshell", "all_categories")`.
#'                        Optional, specifying this parameter will provide estimates for
#'                        each of the selected shell conditions; `"all_categories"` will
#'                        provide estimates for each available shell condition category.
#' @param egg_condition Character string. One or many of `c("none", "uneyed", "eyed", "dead", "empty_cases", "hatching", "all_categories")`.
#'                      Optional, specifying this parameter will provide estimates for each of the
#'                      selected egg conditions; `"all_categories"` will provide estimates
#'                      for each available egg condition category. Note that specifying
#'                      `egg_condition` will return only female specimens in the final output.
#' @param clutch_size Character string. One or many of `c("immature", "mature_barren", "trace", "quarter", "half", "three_quarter", "full", "all_categories")`.
#'                    Optional, specifying this parameter will provide estimates for each of
#'                    the selected clutch sizes; `"all_categories"` will provide estimates for
#'                    each available clutch size category. Note that specifying `clutch_size`
#'                    will return only female specimens in the final output.
#' @param bin_1mm Boolean T/F. If `TRUE`, estimates will be provided for each 1mm bin
#'                within the size range specified in `size_min` and/or `size_max`,
#'                or for the full range of observed sizes in the data. Defaults to `FALSE`.
#' @param spatial_level Character string. One of `c("stratum", "district", "region")`.
#'                      Describes the spatial resolution of biomass and abundance output,
#'                      aggregated to the stratum, district (default), or regional scale.
#' @param replace_retow Boolean T/F. If `TRUE`, replace female Bristol Bay red king crab observations
#'                      with resampled data (haul type = 17) in years when a Bristol Bay
#'                      retow took place. Defaults to `TRUE`, please use care when interpreting
#'                      BBRKC outputs if using `FALSE`.
#' @param rm_corners Boolean T/F. If `TRUE`, remove corner stations from Pribilof and St. Matthew
#'                   Districts and calculate CPUE, abundance, and biomass based on a single
#'                   stratum within the surrounding district. Defaults to `FALSE`, this is to
#'                   be used for design-based data exercises only.
#'
#' @eval c("@return", "A data frame with area swept-expanded estimates of stratum-, district-, or
#'       region-level abundance, biomass (mt), and biomass (lbs) by year.",
#'       get_table_metadata("inst/extdata/metadata.csv", select = c("SPECIES", "YEAR", "TOTAL_AREA",
#'       "REGION", "DISTRICT", "ABUNDANCE", "ABUNDANCE_CV", "ABUNDANCE_CI", "BIOMASS_MT",
#'       "BIOMASS_MT_CV", "BIOMASS_MT_CI", "BIOMASS_LBS", "BIOMASS_LBS_CV", "BIOMASS_LBS_CI",
#'       "CATEGORY", "SEX_TEXT", "SHELL_TEXT", "EGG_CONDITION_TEXT", "CLUTCH_TEXT", "SIZE_1MM") ))
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
                          spatial_level = c("stratum", "district", "region")[2],
                          #output = c("abundance", "biomass_mt", "biomass_lbs")[1],
                          replace_retow = TRUE,
                          rm_corners = FALSE){


  ## ERROR: must specify just one output if length(output > 1)
  if(length(spatial_level) > 1){
    stop("Argument `spatial_level` must be of length = 1.")
  }


  # call calc_cpue()
  cpue <- crabpack::calc_cpue(crab_data = crab_data,
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
  group_cols <- cpue$group_cols

  # make a vector of years to re-expand zero-catch years by
  year_vec <- unique(station_cpue$YEAR)


  # Identify zero-catch stations for Northern District RKC and Hair Crab, and BKC Unstratified
  if(TRUE %in% (c("NORTH", "UNSTRAT") %in% unique(station_cpue$DISTRICT))){
    # Filter out zero-catch stations if District is NORTH or UNSTRAT
    zero_catch <- station_cpue %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'REGION', 'DISTRICT', 'STRATUM', 'TOTAL_AREA', 'STATION_ID')))) %>%
                  dplyr::reframe(TOTAL_CPUE = sum(CPUE)) %>%
                  dplyr::mutate(REMOVE = dplyr::case_when((DISTRICT %in% c("NORTH", "UNSTRAT") & TOTAL_CPUE == 0) ~ "remove",
                                                          TRUE ~ "keep"))

    station_cpue <- station_cpue %>%
                    dplyr::left_join(., zero_catch,
                                     by = c('YEAR', 'STATION_ID', 'REGION',
                                            'DISTRICT', 'STRATUM', 'TOTAL_AREA')) %>%
                    dplyr::filter(REMOVE == "keep") %>%
                    dplyr::select(-c('REMOVE', 'TOTAL_CPUE'))
  }



  ## Calculate abundance and biomass
  # Sum across haul, scale abundance, biomass, and variance to strata, then sum across strata and calc CIs
  bio_abund_stratum <- station_cpue %>%
                       # Scale to abundance by strata
                       dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'REGION', 'DISTRICT', 'STRATUM', 'TOTAL_AREA', group_cols)))) %>%
                       dplyr::reframe(MEAN_CPUE = mean(CPUE),
                                      N_CPUE = dplyr::n(),
                                      VAR_CPUE = (stats::var(CPUE)*(TOTAL_AREA^2))/N_CPUE,
                                      SD_CPUE = sqrt(VAR_CPUE),
                                      MEAN_CPUE_MT = mean(CPUE_MT),
                                      N_CPUE_MT = dplyr::n(),
                                      VAR_CPUE_MT = (stats::var(CPUE_MT)*(TOTAL_AREA^2))/N_CPUE_MT,
                                      SD_CPUE_MT = sqrt(VAR_CPUE_MT),
                                      MEAN_CPUE_LBS = mean(CPUE_LBS),
                                      N_CPUE_LBS = dplyr::n(),
                                      VAR_CPUE_LBS = (stats::var(CPUE_LBS)*(TOTAL_AREA^2))/N_CPUE_LBS,
                                      SD_CPUE_LBS = sqrt(VAR_CPUE_LBS),
                                      ABUNDANCE = (MEAN_CPUE * TOTAL_AREA),
                                      ABUNDANCE_CV = (SD_CPUE/ABUNDANCE),
                                      ABUNDANCE_CI = 1.96*(SD_CPUE),
                                      BIOMASS_MT = (MEAN_CPUE_MT * TOTAL_AREA),
                                      BIOMASS_MT_CV = (SD_CPUE_MT/BIOMASS_MT),
                                      BIOMASS_MT_CI = (1.96*SD_CPUE_MT),
                                      BIOMASS_LBS = (MEAN_CPUE_LBS * TOTAL_AREA),
                                      BIOMASS_LBS_CV = (SD_CPUE_LBS/BIOMASS_LBS),
                                      BIOMASS_LBS_CI = 1.96*(SD_CPUE_LBS),
                                      N_STATIONS = length(unique(STATION_ID))) %>%
                       dplyr::distinct()


  # Re-expand by year to add back in 0-catch years for Northern District RKC and Hair Crab, and BKC Unstratified
  if(TRUE %in% (c("NORTH", "UNSTRAT") %in% unique(station_cpue$DISTRICT))){
    bio_abund_stratum <- bio_abund_stratum %>%
                         dplyr::right_join(tidyr::expand_grid(SEX_TEXT = sex_combos,
                                                              CATEGORY = category_combos,
                                                              SHELL_TEXT = shell_combos,
                                                              EGG_CONDITION_TEXT = egg_combos,
                                                              CLUTCH_TEXT = clutch_combos,
                                                              SIZE_1MM = bin_combos,
                                                              YEAR = year_vec,
                                                              REGION = unique(station_cpue$REGION),
                                                              DISTRICT = unique(station_cpue$DISTRICT)),
                                           by = c('YEAR', 'REGION', 'DISTRICT', group_cols),
                                           relationship = "many-to-many") %>%
                         dplyr::select(dplyr::all_of(c("YEAR", "REGION", "DISTRICT", "STRATUM",
                                                       "TOTAL_AREA", "N_STATIONS", group_cols,
                                                       "MEAN_CPUE", "N_CPUE", "VAR_CPUE", "SD_CPUE",
                                                       "MEAN_CPUE_MT", "N_CPUE_MT", "VAR_CPUE_MT", "SD_CPUE_MT",
                                                       "MEAN_CPUE_LBS", "N_CPUE_LBS", "VAR_CPUE_LBS", "SD_CPUE_LBS",
                                                       "ABUNDANCE", "ABUNDANCE_CV", "ABUNDANCE_CI",
                                                       "BIOMASS_MT", "BIOMASS_MT_CV", "BIOMASS_MT_CI",
                                                       "BIOMASS_LBS", "BIOMASS_LBS_CV", "BIOMASS_LBS_CI"))) %>%
                         distinct()
  }

  if(spatial_level == "stratum"){
    # Format output dataframe
    stratum_out <- bio_abund_stratum %>%
                   dplyr::mutate(SPECIES = species) %>%
                   dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'REGION', 'DISTRICT',
                                                 'STRATUM', 'TOTAL_AREA', group_cols,
                                                 'ABUNDANCE', 'ABUNDANCE_CV', 'ABUNDANCE_CI',
                                                 'BIOMASS_MT', 'BIOMASS_MT_CV', 'BIOMASS_MT_CI',
                                                 'BIOMASS_LBS', 'BIOMASS_LBS_CV', 'BIOMASS_LBS_CI'))) %>%
                   replace(is.na(.), 0)

    return(stratum_out)
  }


  bio_abund_district <- bio_abund_stratum %>%
                        # Sum across strata
                        dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'REGION', 'DISTRICT', group_cols)))) %>%
                        dplyr::reframe(TOTAL_AREA = sum(TOTAL_AREA),
                                       MEAN_CPUE = sum(MEAN_CPUE),
                                       VAR_CPUE = sum(VAR_CPUE),
                                       SD_CPUE = sqrt(VAR_CPUE),
                                       N_CPUE = sum(N_CPUE),
                                       MEAN_CPUE_MT = sum(MEAN_CPUE_MT),
                                       VAR_CPUE_MT = sum(VAR_CPUE_MT),
                                       SD_CPUE_MT = sqrt(VAR_CPUE_MT),
                                       N_CPUE_MT = sum(N_CPUE_MT),
                                       MEAN_CPUE_LBS = sum(MEAN_CPUE_LBS),
                                       VAR_CPUE_LBS = sum(VAR_CPUE_LBS),
                                       SD_CPUE_LBS = sqrt(VAR_CPUE_LBS),
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
                        dplyr::mutate(N_STATIONS = ifelse((YEAR == 2000 & DISTRICT == "BB"), 135, N_STATIONS)) %>%
                        dplyr::ungroup()

  if(spatial_level == "district"){
    # Format output dataframe
    district_out <- bio_abund_district %>%
                    dplyr::mutate(SPECIES = species) %>%
                    dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'REGION', 'DISTRICT', 'TOTAL_AREA', group_cols,
                                                  'ABUNDANCE', 'ABUNDANCE_CV', 'ABUNDANCE_CI',
                                                  'BIOMASS_MT', 'BIOMASS_MT_CV', 'BIOMASS_MT_CI',
                                                  'BIOMASS_LBS', 'BIOMASS_LBS_CV', 'BIOMASS_LBS_CI'))) %>%
                    replace(is.na(.), 0)
    return(district_out)
  }


  bio_abund_region <- bio_abund_district %>%
                      # Sum across districts
                      dplyr::group_by(dplyr::across(dplyr::all_of(c('YEAR', 'REGION', group_cols)))) %>%
                      dplyr::reframe(TOTAL_AREA = sum(TOTAL_AREA),
                                     MEAN_CPUE = sum(MEAN_CPUE),
                                     VAR_CPUE = sum(VAR_CPUE),
                                     SD_CPUE = sqrt(VAR_CPUE),
                                     N_CPUE = sum(N_CPUE),
                                     MEAN_CPUE_MT = sum(MEAN_CPUE_MT),
                                     VAR_CPUE_MT = sum(VAR_CPUE_MT),
                                     SD_CPUE_MT = sqrt(VAR_CPUE_MT),
                                     N_CPUE_MT = sum(N_CPUE_MT),
                                     MEAN_CPUE_LBS = sum(MEAN_CPUE_LBS),
                                     VAR_CPUE_LBS = sum(VAR_CPUE_LBS),
                                     SD_CPUE_LBS = sqrt(VAR_CPUE_LBS),
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
                      dplyr::ungroup()

  if(spatial_level == "region"){
    # Format output dataframe
    region_out <- bio_abund_region %>%
                  dplyr::mutate(SPECIES = species) %>%
                  dplyr::select(dplyr::all_of(c('SPECIES', 'YEAR', 'REGION', 'TOTAL_AREA', group_cols,
                                                'ABUNDANCE', 'ABUNDANCE_CV', 'ABUNDANCE_CI',
                                                'BIOMASS_MT', 'BIOMASS_MT_CV', 'BIOMASS_MT_CI',
                                                'BIOMASS_LBS', 'BIOMASS_LBS_CV', 'BIOMASS_LBS_CI'))) %>%
                  replace(is.na(.), 0)
    return(region_out)
  }

}
