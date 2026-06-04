#' Title
#'
#' @param ToHU
#' @param aggrFUN
#' @param TooLowLimit
#' @param verbose
#' @importFrom tidyr pivot_wider
#'
#' @returns
#' @export
#'
#' @examples
aggre_HU_Calc2 <- function(CalcedHU, ChemData = Gross, zeros_PAF,
                           aggrFUN = max, agg_jaar = F,
                           TooLowLimit = 0.0001,
                           National, verbose = FALSE){

  #if Sample as filtered AND not filtered tuples for otherwise same sample & substance: take filtered
  #by first aggregate for SampleID, substance, Filtered and take the aggrFUN (usually max) then
  #aggregate for SampleID, substance and then apply the aggrFUN;
  #basically: prefereably the filtered sample and the maximum active concentration

  # local helper for mutate SampleID for jaar aggegation
  mutate2jaar <- function(inSamples){
    calc_jaar <- inSamples %>% mutate(
      jaar = as.Date(THEdate, format = "%d-%m-%Y") %>%
        format("%Y")
    )
    if (agg_jaar) {#global to upper function)
      mutate(calc_jaar,
             SampleID = paste(Meetobject.lokaalID, jaar, sep = ":")) %>%
        return()
    } else {
      return(calc_jaar)
    }
  }

  Awarning <- list()
  excluded_rows <- data.frame()

  aggrFUN_name <- deparse(substitute(aggrFUN))

  CalcedHU <- mutate2jaar(CalcedHU)

  zeros_PAF <- mutate2jaar(zeros_PAF) %>%
    mutate(ActConc = 0.0) %>%
    select(substance_key, SampleID, ActConc, THEdate, jaar)


  # Preserve important columns for matching the aggregated data
  # This ensures we get columns from the row that produced the aggregated value
  preserved_cols_acute <- c("HU_acute", "Dev10Log_acute",
                      "Avg10Log_acute", "groep.fotoNL",
                      "PrimaryMoA" # "PAFacute", "PAFchronic", #"UseClass",
                      )
  preserved_cols_chronic <- c("HU_Chronic", "Dev10Log_chronic",
                            "Avg10Log_chronic", "groep.fotoNL",
                            "PrimaryMoA" # "PAFacute", "PAFchronic", #"UseClass",
  )
  preserved_cols_all <- c("Meetobject.lokaalID")

  incl_zero_limit <- bind_rows(
    CalcedHU %>% select(all_of(colnames(zeros_PAF))),
    zeros_PAF)

  # we will need all combo's of known, measured substances per samplesite
  AllSamples <- incl_zero_limit %>%
    select(SampleID) %>%
    distinct()

  if (agg_jaar) {
    aggTomsPAF <- incl_zero_limit %>%
      group_by(substance_key, SampleID, jaar) %>%
      summarise(
        ActConc = aggrFUN(ActConc),
        .groups = "drop"
      ) %>%
      filter(ActConc > 0)
  } else { # max per sample already taken in clean2
    aggTomsPAF <- incl_zero_limit %>%
      filter(ActConc > 0)
  }

  rejoin_acute <- aggTomsPAF %>%
    left_join(CalcedHU %>%
        select(substance_key, SampleID, ActConc, all_of(preserved_cols_acute))) %>%
        distinct() %>% # theoretically also multiple ActConc possible
    # leave very low values out
    filter(HU_acute > TooLowLimit) %>%
    rename(HU = HU_acute,
           Dev10Log = Dev10Log_acute,
           Avg10Log = Avg10Log_acute)

  msPAFacute <- HU2msPAF(rejoin_acute)

  rejoin_chronic <- aggTomsPAF %>%
    left_join(CalcedHU %>%
                select(substance_key, SampleID, ActConc, all_of(preserved_cols_chronic))) %>%
    distinct() %>% # theoretically also multiple ActConc possible
    # leave very low values out
    filter(HU_Chronic > TooLowLimit) %>%
    rename(HU = HU_Chronic,
           Dev10Log = Dev10Log_chronic,
           Avg10Log = Avg10Log_chronic)

  msPAFchronic <- HU2msPAF(rejoin_chronic)

  ClAll <- AllSamples %>%
    left_join(msPAFacute %>% rename(Acute = msPAF)) %>%
    left_join(msPAFchronic %>% rename(Chronic = msPAF))

  if (National == "Nederlands") {
    ClAll <- ClAll %>% mutate(
      Class = case_when(
        is.na(Acute) & Chronic > 0.05 ~ "Niet te bepalen (matig tot Zeer hoog)",
        is.na(Acute) & Chronic <= 0.05 & Chronic > 0.005 ~ "2-Gering",
        is.na(Acute) & Chronic <= 0.005 ~ "1-Geen",
        !is.na(Acute) & Acute > 0.10 ~ "5-Zeer hoog (Very high)",
        !is.na(Acute) & Acute <= 0.10 & Acute > 0.005 ~ "4-Hoog (High)",
        !is.na(Acute) & Acute <= 0.005 & is.na(Chronic) ~ "Niet te bepalen (Matig tot gering)",
        !is.na(Acute) & Acute <= 0.005 & Chronic > 0.05 ~ "3-Matig (Moderate)",
        !is.na(Acute) & Acute <= 0.005 & Chronic <= 0.05 & Chronic > 0.005 ~ "2-Gering (Low)",
        !is.na(Acute) & Acute <= 0.005 & Chronic <= 0.005 ~ "1-Geen",
        TRUE ~ NA_character_
      )
    )
  } else { #International

    ClAll <- ClAll %>% mutate(
        Class = case_when(
          is.na(Acute) & Chronic > 0.05 ~ "Undetermined (moderate to high)",
          is.na(Acute) & Chronic <= 0.05 & Chronic > 0.005 ~ "2-Low (Gering)",
          is.na(Acute) & Chronic <= 0.005 ~ "1-None (Geen)",
          !is.na(Acute) & Acute > 0.10 ~ "5-Very high (Zeer hoog)",
          !is.na(Acute) & Acute <= 0.10 & Acute > 0.005 ~ "4-High (Hoog)",
          !is.na(Acute) & Acute <= 0.005 & is.na(Chronic) ~ "Undetermined (low to moderate)",
          !is.na(Acute) & Acute <= 0.005 & Chronic > 0.05 ~ "3-Moderate (Matig)",
          !is.na(Acute) & Acute <= 0.005 & Chronic <= 0.05 & Chronic > 0.005 ~ "2-Low (Gering)",
          !is.na(Acute) & Acute <= 0.005 & Chronic <= 0.005 ~ "1-None (Geen)",
          TRUE ~ NA_character_
        )
      )
  }

  ClAllWide <- ClAll %>%
    select(-Acute, -Chronic) %>%
    tidyr::pivot_wider(
      names_from = groep.fotoNL,          # Name of substance group TODO generalise
      values_from = Class,                # others to wide
      id_cols = SampleID                  # The id-column
    )

  return(list(
    acute = msPAFacute %>%
      tidyr::pivot_wider(
        names_from = groep.fotoNL,          # Name of substance group TODO generalise
        values_from = msPAF,          # others to wide
        id_cols = SampleID                   # The id-column
      ),
    chronic = msPAFchronic %>%
      tidyr::pivot_wider(
        names_from = groep.fotoNL,          # Name of substance group TODO generalise
        values_from = msPAF,          # others to wide
        id_cols = SampleID                   # The id-column
      ),
    class = ClAllWide,
    warning = Awarning
  ))

}
