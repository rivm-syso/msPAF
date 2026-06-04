
#' Title
#'
#' @param asIM
#' @param ChemData
#' @param HU_results
#'
#' @returns
#' @export
#'
#' @examples
zeroPAFs <- function(asIM, ChemData, HU_results){
  # combine asIM$excludedData with "Limietsymbool <" and HU_results$excluded_rows
  as_zerosPAF <- asIM$excludedData %>%
    filter(ExclusionReason == "Limietsymbool <" &
             substance_key %in% ChemData$substance_key) %>%
    select(Meetobject.lokaalID, Limietsymbool, THEdate, SampleID, substance_key) %>%
    inner_join(ChemData %>%
                 select(substance_key, groep.fotoNL, PrimaryMoA))

  if (nrow(HU_results$excluded_rows) > 0) { #no reason to include "No qualified SSD for chemical"
    as_zerosPAF <- bind_rows(as_zerosPAF, HU_results$excluded_rows)
  }

  return(as_zerosPAF)
}
