#' @title IMformatMultiple
#' @name IMformatMultiple
#' @author Jaap Slootweg
#' @description
#' Reads all CSV and XLSX files from a ZIP archive, combines them into a single data frame, and processes the combined data with \code{leesIMformat}.
#' CSV files are read directly from the ZIP archive; XLSX files are temporarily extracted and then read.
#' All data is concatenated before cleaning and further processing.
#'
#' @param zipFile Path to the ZIP archive containing CSV and/or XLSX files in IM format.
#' @param National Language and format setting for input files. Use "Nederlands" for Dutch conventions (default), or "English" for international conventions.
#' @param SSDbron Source of SSD / chemical data. Default is "Gross".
#' @param MolMassName Name of the column for molar mass. Default is "MW.g.Mol".
#' @param verbose Logical; if \code{TRUE}, enables warning messages.
#'
#' @details
#' - CSV files are read directly from the ZIP archive without extraction.
#' - XLSX files are temporarily extracted to a file and deleted after reading.
#' - All data frames are row-bound (with missing columns filled as NA) before being passed to \code{leesIMformat}.
#' - The function automatically detects the correct field separator and decimal character based on the \code{National} argument.
#'
#' @return
#' A list as returned by \code{leesIMformat}, typically containing:
#' \itemize{
#'   \item \code{inputData}: the combined and processed input data
#'   \item \code{DataSamples}: sample metadata
#'   \item \code{inputwarnings}: any warnings encountered during processing
#' }
#'
#' @examples
#' \dontrun{
#' result <- IMformatMultiple("path/to/your/data.zip", National = "Nederlands")
#' head(result$inputData)
#' }
#' @seealso \code{\link{leesIMformat}}
#' @export
IMformatMultiple <- function(zipFile, National = "Nederlands", SSDbron = "Gross", MolMassName = "MW.g.Mol", verbose = TRUE) {
  # List files in the zip
  zip_files <- unzip(zipFile, list = TRUE)$Name

  # Identify CSV and XLSX files
  csv_files <- zip_files[grepl("\\.csv$", zip_files, ignore.case = TRUE)]
  xlsx_files <- zip_files[grepl("\\.xlsx$", zip_files, ignore.case = TRUE)]

  # 1. Read CSVs directly from zip
  csv_dfs <- lapply(csv_files, function(f) {
    # Guess separator/decimal based on National
    if (National == "Nederlands") {
      sep <- ";"; dec <- ","
    } else {
      sep <- ","; dec <- "."
    }
    con <- unz(zipFile, f)
    on.exit(try(close(con), silent = TRUE))
    df <- tryCatch(
      read.csv2(con, sep = sep, dec = dec, stringsAsFactors = FALSE),
      error = function(e) {
        warning(sprintf("Error reading CSV file %s in zip: %s", f, e$message))
        return(NULL)
      }
    )
    #close(con)
    df
  })
  csv_dfs <- Filter(Negate(is.null), csv_dfs)

  # 2. Unzip and read XLSX files (if any)
  xlsx_dfs <- lapply(xlsx_files, function(f) {
    temp_xlsx <- tempfile(fileext = ".xlsx")
    unzip(zipFile, files = f, exdir = dirname(temp_xlsx), overwrite = TRUE)
    temp_path <- file.path(dirname(temp_xlsx), f)
    df <- tryCatch(
      openxlsx::read.xlsx(temp_path),
      error = function(e) {
        warning(sprintf("Error reading XLSX file %s: %s", f, e$message))
        return(NULL)
      }
    )
    unlink(temp_path)
    df
  })
  xlsx_dfs <- Filter(Negate(is.null), xlsx_dfs)

  # 3. Combine all dataframes
  all_dfs <- c(csv_dfs, xlsx_dfs)
  if (length(all_dfs) == 0) {
    stop("No valid CSV or XLSX files found in zip.")
  }
  # Bind, filling missing columns with NA
  combined_df <- dplyr::bind_rows(all_dfs)

  # 4. Pass combined data to leesIMformat
  result <- leesIMformat(
    filename = combined_df,
    SSDbron = SSDbron,
   # MolMassName = MolMassName,
    National = National,
    verbose = verbose
  )

  return(result)
}
