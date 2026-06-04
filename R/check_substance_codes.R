#' Map and Validate Chemical Identifiers to SSD CAS Numbers
#'
#' Maps user-supplied chemical codes or CAS numbers to the reference CAS/identifier used in the SSD dataset,
#' using a translation table (OtherChar) and SSDbron. Performs CAS format and checksum validation on unmapped
#' CAS codes, and collects warnings for invalid CAS codes or valid codes not present in SSDbron.
#'
#' @param inputData Data.frame. Input measurements, must contain \code{substance_key}
#' @param SSDbron Data.frame. Reference substances; must contain a \code{CAS} column (identifiers in SSD dataset).
#' @param inputwarnings An \code{InputWarnings} R6 object (see package docs) to collect any warnings generated.
#' @param National Character. Language for warnings ("Nederlands" or "English").
#'
#' @details
#' \itemize{
#'   \item If \code{Parameter.code} is empty, it is filled with \code{Parameter.CASnummer}.
#'   \item First, \code{Parameter.code} is mapped to \code{OtherChar$CAS} via \code{OtherChar$ChemCode}.
#'   \item If mapping is not found, and \code{Parameter.CASnummer} is present in \code{SSDbron}, it is used as the reference.
#'   \item If mapping is still not found, CAS codes are validated (format and checksum) using \code{CASClass}.
#'   \item Warnings are added via \code{inputwarnings} for:
#'     \enumerate{
#'       \item Invalid CAS codes (bad format or checksum).
#'       \item Valid CAS codes that are not present in \code{SSDbron}.
#'     }
#' }
#'
#' @return
#' The input data.frame, with an added column \code{CAS_reference} containing the mapped CAS/identifier used in \code{SSDbron}.
#' Warnings are added to the supplied \code{inputwarnings} object.
#' List with inputData (valid rows) and excluded_rows
#'
#' @seealso \code{\link{CASClass}}, \code{\link{InputWarnings}}
#'
#' @export
check_substance_codes <- function(inputData, OtherChar, SSDbron, inputwarnings, National = "Nederlands") {

  excluded_rows <- data.frame()

  # 1. & 2. substance_key (replacing ChemCode) finding now executed in leesIMformat self, molweight needed..

  # 3 Remove those without an chemical identifyer
  keep_rows <- !is.na(inputData$substance_key) & inputData$substance_key!= ""
  if(sum(keep_rows) < nrow(inputData)){
    msg_nl <- "regels verwijderd door een missende stof identificatie"
    msg_en <- "Missing subtance identifyer, removed rows"
    inputwarnings$add("NoSubstatance", msg_nl, msg_en, National)
    excluded_nosubstance <- inputData[!keep_rows, ]
    excluded_nosubstance$ExclusionReason <- "Missing substance identifier"
    excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_nosubstance)
    inputData <- inputData[keep_rows,]
  }

  # service of checking valid CAS is not executed ( TODO ? Then improve!)
  # # 4. For rows where mapping fails AND fallback not possible,
  # #    check for CAS validity and warn for invalid ones
  # mapped <- !is.na(inputData$substance_key) & inputData$substance_key != ""
  # cas_to_check <- inputData$substance_key[mapped]
  # cas_obj <- CASClass$new(cas_to_check)
  # valid_cas <- cas_obj$ValidCAS()

  # # Warn for invalid CAS codes (bad format/checksum)
  # if (any(!valid_cas & !is.na(cas_to_check) & cas_to_check != "")) {
  #   bad_cas <- unique(cas_to_check[!valid_cas & !is.na(cas_to_check) & cas_to_check != ""])
  #   msg_nl <- paste("Ongeldige CAS-code(s):", paste(bad_cas, collapse = ", "))
  #   msg_en <- paste("Invalid CAS code(s):", paste(bad_cas, collapse = ", "))
  #   inputwarnings$add("BadCAS", msg_nl, msg_en, National)
  #   # and remove them from inputs
  #   excluded_badcas <- inputData[inputData$substance_key %in% bad_cas, ]
  #   excluded_badcas$ExclusionReason <- "Invalid CAS code"
  #   excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_badcas)
  #   inputData <- inputData[!inputData$substance_key %in% bad_cas,]
  # }

  # Warn / exclude substances not found in SSDbron
  uniq_substance_key <- unique(inputData$substance_key)
  not_in_ssd <- uniq_substance_key[!uniq_substance_key %in% SSDbron$substance_key]
  if (length(not_in_ssd) > 0) {
    msg_nl <- paste("Chemische code(s) niet in SSDbron:", paste(not_in_ssd, collapse = ", "))
    msg_en <- paste("Chemical code(s) not in SSDbron:", paste(not_in_ssd, collapse = ", "))
    inputwarnings$add("CASnotSSD", msg_nl, msg_en, National)
    excluded_notssd <- inputData[inputData$substance_key %in% not_in_ssd, ]
    excluded_notssd$ExclusionReason <- "No SSD"
    excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_notssd)
    inputData <- inputData[!inputData$substance_key %in% not_in_ssd,]
  }

  list(inputData = inputData, excluded_rows = excluded_rows)
}
