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
aggre_HU_Calc2 <- function(CalcedHU, ChemData = Gross, zeros_PAF = NULL,
                           aggrFUN = max, agg_jaar = F,
                           TooLowLimit = 0.0001,
                           National, verbose = FALSE){

  #if Sample as filtered AND not filtered tuples for otherwise same sample & substance: take filtered
  #by first aggregate for SampleID, substance, Filtered and take the aggrFUN (usually max) then
  #aggregate for SampleID, substance and then apply the aggrFUN;
  #basically: prefereably the filtered sample and the maximum active concentration

  # local helper for mutate SampleID for jaar aggegation
  mutate2jaar <- function(inSamples){
    calc_jaar <- inSamples |> dplyr::mutate(
      jaar = as.Date(THEdate, format = "%d-%m-%Y") |>
        format("%Y")
    )
    if (agg_jaar) {#global to upper function)
      return(dplyr::mutate(calc_jaar,
             SampleID = paste(Meetobject.lokaalID, jaar, sep = ":"))
      )
    } else {
      return(calc_jaar)
    }
  }

  Awarning <- list()
  excluded_rows <- data.frame()

  aggrFUN_name <- deparse(substitute(aggrFUN))

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

  CalcedHU <- mutate2jaar(CalcedHU) |>
    dplyr::mutate(Parameter.code = ifelse(is.na(Parameter.code) | Parameter.code %in% c("", "NVT"),
                                          substance_key,
                                          Parameter.code))

  if (!is.null(zeros_PAF)){
    zeros_PAF <- mutate2jaar(zeros_PAF) |>
      dplyr::mutate(ActConc = 0.0,
                    Parameter.code = ifelse(is.na(Parameter.code) | Parameter.code %in% c("", "NVT"),
                                            substance_key,
                                            Parameter.code)) |>
      dplyr::select(Parameter.code, substance_key, SampleID, ActConc, THEdate, jaar)

    incl_zero_limit <- dplyr::bind_rows(
      CalcedHU |> dplyr::select(all_of(colnames(zeros_PAF))),
      zeros_PAF)

  } else {
    incl_zero_limit <- CalcedHU |> dplyr::select(Parameter.code, substance_key, SampleID, ActConc, THEdate, jaar)
  }


  # we will need all combo's of known, measured substances per samplesite
  AllSamples <- incl_zero_limit |>
    dplyr::select(SampleID) |>
    dplyr::distinct()

  if (agg_jaar) {
    aggTomsPAF <- incl_zero_limit |>
      dplyr::group_by(substance_key, SampleID, jaar) |>
      dplyr::summarise(
        ActConc = aggrFUN(ActConc),
        .groups = "drop"
      ) |>
      dplyr::filter(ActConc > 0)
  } else { # max per sample already taken in clean2
    aggTomsPAF <- incl_zero_limit |>
      dplyr::filter(ActConc > 0)
  }

  rejoin_acute <- aggTomsPAF |>
    dplyr::left_join(CalcedHU |>
        dplyr::select(substance_key, SampleID, ActConc, dplyr::all_of(preserved_cols_acute))) |>
        dplyr::distinct() |> # theoretically also multiple ActConc possible
    # leave very low values out
    dplyr::filter(HU_acute > TooLowLimit) |>
    dplyr::rename(HU = HU_acute,
           Dev10Log = Dev10Log_acute,
           Avg10Log = Avg10Log_acute)

  msPAFacute <- HU2msPAF(rejoin_acute)

  rejoin_chronic <- aggTomsPAF |>
    dplyr::left_join(CalcedHU |>
    dplyr::select(substance_key, SampleID, ActConc, dplyr::all_of(preserved_cols_chronic))) |>
    dplyr::distinct() |> # theoretically also multiple ActConc possible
    # leave very low values out
    dplyr::filter(HU_Chronic > TooLowLimit) |>
    dplyr::rename(HU = HU_Chronic,
           Dev10Log = Dev10Log_chronic,
           Avg10Log = Avg10Log_chronic)

  msPAFchronic <- HU2msPAF(rejoin_chronic)

  ClAll <- AllSamples |>
    dplyr::left_join(msPAFacute |> dplyr::rename(Acute = msPAF)) |>
    dplyr::left_join(msPAFchronic |> dplyr::rename(Chronic = msPAF))

  if (National == "Nederlands") {
    ClAll <- ClAll |> dplyr::mutate(
      Class = dplyr::case_when(
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

    ClAll <- ClAll |> dplyr::mutate(
        Class = dplyr::case_when(
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

  ClAllWide <- ClAll |>
    dplyr::select(-Acute, -Chronic) |>
    tidyr::pivot_wider(
      names_from = groep.fotoNL,          # Name of substance group TODO generalise
      values_from = Class,                # others to wide
      id_cols = SampleID                  # The id-column
    )

  acute <- msPAFacute |>
    tidyr::pivot_wider(
      names_from = groep.fotoNL,          # Name of substance group TODO generalise
      values_from = msPAF,          # others to wide
      id_cols = SampleID                   # The id-column
    )
  chronic <- msPAFchronic |>
    tidyr::pivot_wider(
      names_from = groep.fotoNL,          # Name of substance group TODO generalise
      values_from = msPAF,          # others to wide
      id_cols = SampleID                   # The id-column
    )

  return(list(
    acute = acute,
    chronic = chronic,
    class = ClAllWide,
    warning = Awarning
  ))

}
