#' Clean and Prepare Input Data for SSD Analysis
#'
#' This function cleans and processes the input data for SSD (Species Sensitivity Distribution) analysis.
#' It handles unit conversion, substance code checks, aggregation, and exclusion of rows with unknown SSD.
#'
#' @param inputData A `data.frame` containing measurement data. Must include columns such as \code{Concentration}, \code{substance_key}, \code{SampleID}, and \code{PreTreatment}.
#' @param SSDsubstanceData A `data.frame` with SSD substance information required for validation and processing.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Renames the \code{Concentration} column to \code{MeasuredValue}.
#'   \item Converts units and calculates concentrations using \code{convert_units_and_calculate_concentration}.
#'   \item Excludes rows with unknown or invalid SSD information.
#'   \item Checks and processes substance codes.
#'   \item Aggregates samples by taking the maximum value for each combination of \code{substance_key}, \code{SampleID}, and \code{PreTreatment}.
#'   \item Filters aggregated samples based on specific criteria for \code{PreTreatment}.
#' }
#'
#' @return A processed \code{data.frame} with cleaned and aggregated measurement data, ready for SSD analysis.
#'
#' @examples
#' \dontrun{
#' cleaned_data <- CleanFase2(inputData = my_data, SSDsubstanceData = ssd_data)
#' }
#'
#' @export
CleanFase2 <- function(inputData,
                       SSDsubstanceData,
                       init_inputwarnings,
                       National){

  excluded_rows <- data.frame()
  inputwarnings <- InputWarnings$new(init_inputwarnings)

  # exclude rows with unknown SSD
  NoSSD <- which(!inputData$substance_key %in% SSDsubstanceData$substance_key)
  NoSSDsubstance_key <- do.call( paste,
                              inputData[NoSSD,] %>%
                                pull(substance_key) %>% unique() %>% as.list())
  inputwarnings$add("NoSSD",
                    nl_text = paste("Stof heeft geen SSD : ", NoSSDsubstance_key),
                    en_text = paste("Substance has no SSD", NoSSDsubstance_key),
                    National = National)
  NoSSDRows <- inputData[NoSSD,]
  NoSSDRows$ExclusionReason <- "NoSSD"
  excluded_rows <- dplyr::bind_rows(excluded_rows, NoSSDRows)
  inputData <- inputData[-NoSSD, ]

  # Mind the column use for chem.key in:?!
  conv_result <- convert_units_and_calculate_concentration(inputData, SSDsubstanceData, UnitConversions,
                                                           inputwarnings, National)
  inputData <- conv_result$inputData
  excluded_rows <- dplyr::bind_rows(excluded_rows, conv_result$excluded_rows)

  # conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)] #TODO ??
  # attr(inputData, "conforms") <- conforms

  # for multiple measurements of the same sample (time/place/filtration) we take the max value
  non_max_idx <- which(
    inputData %>%
      group_by(substance_key, SampleID, NaFiltering) %>%
      mutate(
        max_Concentration = max(Concentration, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(is_not_max = Concentration != max_Concentration | is.na(Concentration)) %>%
      pull(is_not_max)
  )

  if (length(non_max_idx) > 0){
    inputwarnings$add(
      "NonMaxConcentration",
      nl_text = paste("Niet-maximale concentraties verwijderd voor aantal rijen: ", length(non_max_idx)),
      en_text = paste("Non-maximum concentrations removed for number of rows: ", length(non_max_idx)),
      National = National
    )
    NonMaxRows <- inputData[non_max_idx, ]
    NonMaxRows$ExclusionReason <- "NonMaxConcentration"
    excluded_rows <- dplyr::bind_rows(excluded_rows, NonMaxRows)

    inputData <- inputData[-non_max_idx, ]
  }

  # for samples with both a non-filtrated and a filtrated measurement, we take the filtered
  non_filtered_indices <- inputData %>%
    mutate(orig_row = row_number()) %>%  # add original row index
    group_by(substance_key, SampleID) %>%
    mutate(
      has_T = any(NaFiltering == TRUE)
    ) %>%
    ungroup() %>%
    filter(NaFiltering == FALSE, has_T) %>%
    pull(orig_row)

  inputwarnings$add(
    "NonFilteredDuplicate",
    nl_text = paste("Niet-gefilterde dubbele rijen verwijderd: ", length(non_filtered_indices)),
    en_text = paste("Non-filtered duplicate rows removed: ", length(non_filtered_indices)),
    National = National
  )
  if(length(non_filtered_indices)>0){
    NonFilteredRows <- inputData[non_filtered_indices, ]
    NonFilteredRows$ExclusionReason <- "NonFilteredRemoved"
    excluded_rows <- dplyr::bind_rows(excluded_rows, NonFilteredRows)
    inputData <- inputData[-non_filtered_indices, ]
  }



  #return
  output_mspaf <- list(
    inputData = inputData,
    inputwarnings = inputwarnings$warnings,
    excludedData = excluded_rows
  )

  return(output_mspaf)

}
