#' PURPOSE: assign maturity-sex categories based on stock-specific size definitions
#' ARGUMENTS: crab_dat, stock
#' @param crab_dat Species-specific CrabHaul file
#' @param stock Character
#'         "BBRKC", "PribRKC", "NSRKC", "NorthRKC", "PribBKC", "StMattBKC", "BKCNBS",
#'         "TannerE", "TannerW", "TannerWNBS", "Snow", "SnowNBS",
#'         "Hybrid", "HybridNBS", "Hair", "HairNBS"
#' @return Returns a data frame with catch data and a MAT_SEX column with stock-specific maturity designations
#' @export
#' NEEDS MAT_LOOKUP!

get_maturity  <- function(crab_dat,
                          stock) {

  # define stock groups "kingcrab", "chionoecetes", "hair"
  stock_kingcrab <- c("BBRKC", "PribRKC", "NSRKC", "NorthRKC", "PribBKC", "StMattBKC", "BKCNBS")
  stock_chionoecetes <- c("TannerE", "TannerW", "TannerWNBS", "Snow", "SnowNBS", "Hybrid", "HybridNBS")
  stock_hair <- c("Hair", "HairNBS")

  # define size as "LENGTH_1MM" or "WIDTH_1MM" based on stock
  SIZE_DEF <- ifelse(stock %in% stock_chionoecetes, "WIDTH_1MM", "LENGTH_1MM")

  # assign mat/sex categories
  if(stock %in% c(stock_kingcrab, stock_chionoecetes)){
    #mature/immature males and females
    mature <- crab_dat %>%
      dplyr::filter(SEX %in% 1:2) %>%
      mutate(MAT_SEX = case_when((SEX == 1 & get(SIZE_DEF) >= mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Mature Male",
                                 (SEX == 1 & get(SIZE_DEF) < mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Immature Male",
                                 (SEX == 2 & CLUTCH_SIZE >= 1) ~ "Mature Female",
                                 (SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature Female"))
    #legal/pre-recruit males
    leg <- crab_dat %>%
      dplyr::filter(SEX == 1) %>%
      mutate(MAT_SEX = case_when((get(SIZE_DEF) >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "Legal Male",
                                 (get(SIZE_DEF) <= mat_lookup$recruit[mat_lookup$stock == stock]) ~ "Pre-recruit Male"))

    if(stock %in% stock_chionoecetes){
      #industry-preferred males
      ind_pref <- crab_dat %>%
        dplyr::filter(SEX == 1) %>%
        mutate(MAT_SEX = case_when((get(SIZE_DEF) >= mat_lookup$preferred[mat_lookup$stock == stock]) ~ "Industry Preferred"))

      crab_dat2 <- rbind(mature, leg, ind_pref)
    } else{
      crab_dat2 <- rbind(mature, leg)
    }
  }

  if(stock %in% stock_hair){
    #legal/sublegal males, and females
    crab_dat2 <- crab_dat %>%
      dplyr::filter(SEX %in% 1:2) %>%
      mutate(MAT_SEX = case_when((SEX == 1 & get(SIZE_DEF) >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "Legal Male",
                                 (SEX == 1 & get(SIZE_DEF) < mat_lookup$legal[mat_lookup$stock == stock]) ~ "Sublegal Male",
                                 (SEX == 2) ~ "Female"))
  }

  out_dat <- crab_dat2 %>% filter(!is.na(MAT_SEX))

  return(out_dat)
}
