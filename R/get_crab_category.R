#' Assign maturity-sex categories based on stock-specific size definitions
#' #' NEEDS MAT_LOOKUP!
#' @description
#'
#' @inheritParams calc_bioabund
#'
#' @return Returns a data frame with crab specimen data and a CATEGORY column with
#'         stock-specific maturity designations
#'
#' @export


get_crab_category  <- function(crab_data = NULL,
                               species = NULL,
                               region = c("EBS", "NBS")[1],
                               district = NULL,
                               crab_category = NULL,
                               female_maturity = c("morphological", "cutline")[1]) {


  ## **SOME SORT OF WARNING if wanting male, can't do morphometric, it's cutline only for this. But see Chionoecetes maturity tables?
  ##   Also something about "mature/immature male" for chionoecetes -- "large/small male"
  ## Hair --> no female maturity...cutline at least -- I guess could do morphometric? And legal/sublegal == mature/immature?


  # female maturity
  if(!missing(female_maturity)){
    if(female_maturity == "morphological"){

    }

    if(female_maturity == "cutline"){
      # something with a size/category lookup!!
      # imm_min <-
      # imm_max <-
      # mat_min <-
      # mat_max <-

      female <- crab_data %>%
                dplyr::filter(SEX == 2) %>%
                mutate(MAT_SEX = case_when(SIZE <...))
    }
  }

  if(is.na(SIZE_MIN)){
    size_min <- 0
  }

  if(is.na(SIZE_MAX)){
    size_max <- max(crab_data$SIZE_1MM) + 1
  }



  ## do this by district...
  ## this is where we can loop in BKC unstr --> "district" assignment?
  # assign mat/sex categories
  if(species %in% c("RKC", "BKC")){
    #mature/immature males and females
    mature <- crab_dat %>%
      dplyr::filter(SEX %in% 1:2) %>%
      mutate(MAT_SEX = case_when((SEX == 1 & get(SIZE_DEF) >= mat_lookup$cutline[mat_lookup$stock == stock]) ~ "mature_male",
                                 (SEX == 1 & get(SIZE_DEF) < mat_lookup$cutline[mat_lookup$stock == stock]) ~ "immature_male",
                                 (SEX == 2 & CLUTCH_SIZE >= 1) ~ "mature_female",
                                 (SEX == 2 & CLUTCH_SIZE == 0) ~ "immature_female"))
    #legal males
    leg <- crab_dat %>%
      dplyr::filter(SEX == 1) %>%
      mutate(MAT_SEX = case_when((get(SIZE_DEF) >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "legal_male"))

    if(species %in% c("SNOW", "TANNER", "HYBRID")){
      #industry-preferred males
      ind_pref <- crab_dat %>%
        dplyr::filter(SEX == 1) %>%
        mutate(MAT_SEX = case_when((get(SIZE_DEF) >= mat_lookup$preferred[mat_lookup$stock == stock]) ~ "Industry Preferred"))

      crab_dat2 <- rbind(mature, leg, ind_pref)
    } else{
      crab_dat2 <- rbind(mature, leg)
    }
  }

  if(species %in% c("HAIR")){
    #legal/sublegal males, and females
    crab_dat2 <- crab_dat %>%
      dplyr::filter(SEX %in% 1:2) %>%
      mutate(MAT_SEX = case_when((SEX == 1 & get(SIZE_DEF) >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "legal_male",
                                 (SEX == 1 & get(SIZE_DEF) < mat_lookup$legal[mat_lookup$stock == stock]) ~ "sublegal_male",
                                 (SEX == 2) ~ "Female"))
  }

  out_dat <- crab_dat2 %>% filter(!is.na(MAT_SEX))

  return(out_dat)
}
