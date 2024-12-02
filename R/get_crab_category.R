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


  ## **SOME SORT OF WARNING if wanting male, can't do morphometric, it's cutline only for this. But see Chionoecetes maturity tables?
  ##   Also something about "mature/immature male" for chionoecetes -- "large/small male"
  ## Hair --> no female maturity...cutline at least -- I guess could do morphometric? And legal/sublegal == mature/immature?

  ## THIS NEEDS TO GET LOADED FROM ORACLE
  # --> unpack it from crab_data object list!!
  # already filtered to species and region in the SQL call
  sizegroups <- crab_data$sizegroups #%>% #read.csv("sizegroup_district.csv") %>%
                # dplyr::filter(.data$SPECIES == species,
                #               .data$REGION == region)

  ## NEED TO MATCH DISTRICT.....


  # female maturity
  if(!is.null(female_maturity)){
    if(female_maturity == "morphological"){
      fem_mat <- crab_data %>%
                 dplyr::filter(.data$SEX == 2) %>%
                 mutate(CATEGORY = case_when(.data$CLUTCH_SIZE >= 1 ~ "mature_female",
                                             .data$CLUTCH_SIZE == 0 ~ "immature_female"))
    }

    if(female_maturity == "cutline"){
      # something with a size/category lookup!!
      imm_min <- 0
      imm_max <-
      mat_min <-
      mat_max <- 250

      female <- crab_data %>%
                dplyr::filter(.data$SEX == 2) #%>%
                # mutate(MAT_SEX = case_when(SIZE <...))
    }
  }

  if(is.na(sizegroups$SIZE_MIN)){
    size_min <- 0
  }

  if(is.na(sizegroups$SIZE_MAX)){
    size_max <- max(crab_data$SIZE_1MM) + 1
  }



  ## do this by district...
  ## this is where we can loop in BKC unstr --> "district" assignment?
  # assign mat/sex categories
  if(species %in% c("RKC", "BKC")){
    #mature/immature males and females
    mature <- crab_data %>%
      dplyr::filter(.data$SEX %in% 1:2) %>%
      dplyr::mutate(MAT_SEX = case_when((.data$SEX == 1 & .data$SIZE >= mat_lookup$cutline[mat_lookup$stock == district]) ~ "mature_male",
                                 (.data$SEX == 1 & .data$SIZE < mat_lookup$cutline[mat_lookup$stock == district]) ~ "immature_male",
                                 (.data$SEX == 2 & .data$CLUTCH_SIZE >= 1) ~ "mature_female",
                                 (.data$SEX == 2 & .data$CLUTCH_SIZE == 0) ~ "immature_female"))
    #legal males
    leg <- crab_data %>%
      dplyr::filter(.data$SEX == 1) %>%
      dplyr::mutate(MAT_SEX = case_when((.data$SIZE >= mat_lookup$legal[mat_lookup$stock == district]) ~ "legal_male"))

    if(species %in% c("SNOW", "TANNER", "HYBRID")){
      #industry-preferred males
      ind_pref <- crab_data %>%
        dplyr::filter(.data$SEX == 1) %>%
        dplyr::mutate(MAT_SEX = case_when((.data$SIZE >= mat_lookup$preferred[mat_lookup$stock == district]) ~ "preferred"))

      crab_dat2 <- rbind(mature, leg, ind_pref)
    } else{
      crab_dat2 <- rbind(mature, leg)
    }
  }

  if(species %in% c("HAIR")){
    #legal/sublegal males, and females
    crab_dat2 <- crab_data %>%
      dplyr::filter(.data$SEX %in% 1:2) %>%
      dplyr::mutate(MAT_SEX = case_when((.data$SEX == 1 & .data$SIZE >= mat_lookup$legal[mat_lookup$stock == district]) ~ "legal_male",
                                        (.data$SEX == 1 & .data$SIZE < mat_lookup$legal[mat_lookup$stock == district]) ~ "sublegal_male",
                                        (.data$SEX == 2) ~ "female"))
  }

  out_dat <- crab_dat2 %>%
             dplyr::filter(!is.na(.data$MAT_SEX))

  return(out_dat)
}
