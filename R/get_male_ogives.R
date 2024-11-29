
#' @description .... time series of Tanner Crab and Snow Crab male ogives...proportion mature
#'              Also yearly parameter estimates....
#'
#' @param species character string. One of c("Tanner Crab", "Snow Crab").
#'                c("TANNER", "SNOW").
#' @param district character string. One of c("ALL", "E166", "W166"). Defaults to
#'                 "ALL", "E166" and "W166" are to be used for Tanner Crab only.
#' @param output character string. One of c("estimates", "parameters"). Defaults
#'               to "estimates" to provide ___. Specify "parameters" for a table
#'               of model parameter estimates for each year.
#' @inheritParams get_specimen_data
#'
#' @return
#'
#' @export
#'

get_male_ogives <- function(species = NULL,
                            region = c("EBS", "NBS")[1], # not sure if we have ogive calcs for NBS??
                            district = c("ALL", "E166", "W166")[1],
                            output = c("estimates", "parameters")[1],
                            channel = NULL){



}
