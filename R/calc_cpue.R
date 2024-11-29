#' Calculate haul-level catch per unit effort
#'
#' @description This function calculates and zero-fills weight and numerical
#'              catch per unit effort (area swept, km2)
#'
#' @inheritParams calc_bioabund
#' @param output character string. One of c("cpue", "bioabund"). Defaults output to
#'               "cpue", "bioabund" is used for internal package purposes, as the
#'               'calc_cpue()' function is called within 'calc_bioabund()'.
#'
#' @return A data frame with station-level area swept-expanded estimates of weight
#'         and numerical CPUE by year.
#'
#' @export

calc_cpue <- function(data_crab = NULL,
                      species = NULL,
                      region = c("EBS", "NBS")[1],
                      district = c("ALL", "BB", "NORTH", "NS", "PRIB", "STMATT", "UNSTRAT", "E166", "W166", "166TO173")[1],
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
                      replace_retow = TRUE,
                      output = c("cpue", "bioabund")[1]){


  # CALL set_variables() FUNCTION
  ## set inputs from calc_cpue....
  data_crab_cpue <- data_crab
  species_cpue <- species ## I think we need this to then assign crab category in the set_vars() function
  sex_cpue <- sex
  size_min_cpue <- size_min
  size_max_cpue <- size_max
  crab_category_cpue <- crab_category
  female_maturity_cpue <- female_maturity
  shell_condition_cpue <- shell_condition
  egg_condition_cpue <- egg_condition
  clutch_size_cpue <- clutch_size
  bin_1mm_cpue <- bin_1mm

  vars <- set_variables(data_crab = data_crab_cpue,
                        species = species_cpue,
                        sex = seex_cpue,
                        size_min = size_min_cpue,
                        size_max = size_max_cpue,
                        crab_category = crab_category_cpue,
                        female_maturity = female_maturity_cpue,
                        shell_condition = shell_condition_cpue,
                        egg_condition = egg_condition_cpue,
                        clutch_size = clutch_size_cpue,
                        bin_1mm = bin_1mm_cpue)

  # might need to unlist these??
  data_crab2 <- vars$data_crab2
  group_cols <- vars$group_cols


}
