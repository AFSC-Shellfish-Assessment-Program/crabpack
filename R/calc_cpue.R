#' Calculate haul-level catch per unit effort
#'
#' @description This function calculates and zero-fills weight and numerical
#'              catch per unit effort (area swept, km2)
#'
#' @inheritParams calc_bioabund
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
                      #female_maturity = c("morphological", "cutline")[1],
                      shell_condition = NULL,
                      egg_condition = NULL,
                      clutch_size = NULL,
                      bin_1mm = FALSE,
                      replace_retow = TRUE){




}
