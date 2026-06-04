# first a helper function
#' @title ArrangeIMdata
#' @name ArrangeIMdata
#' @author Jaap Slootweg
#' @description
#' Determines appropriate sorting columns for an IM data set and returns the data frame sorted accordingly.
#'
#' @param data A data frame to be sorted. Should contain at least \code{SampleID} and optionally \code{MeasuredValue} or \code{Concentration}.
#'
#' @details
#' The function checks for the presence of \code{MeasuredValue} and \code{Concentration} columns, preferring to sort by \code{SampleID} + \code{MeasuredValue} if both are present. If only \code{Concentration} is present, sorts by \code{SampleID} + \code{Concentration}. Otherwise, sorts by \code{SampleID} only.
#'
#' @return
#' The sorted data frame.
#'
#' @export
ArrangeIMdata <- function(data) {
  if ("orig_rownr" %in% names(data)) {
    sort_cols <- "orig_rownr"
  } else {
    sort_cols <- intersect(c("SampleID", "MeasuredValue", "Concentration"), names(data))
    if ("MeasuredValue" %in% sort_cols) {
      sort_cols <- c("SampleID", "MeasuredValue")
    } else if ("Concentration" %in% sort_cols) {
      sort_cols <- c("SampleID", "Concentration")
    } else {
      sort_cols <- "SampleID"
    }
  }
  dplyr::arrange(data, !!!rlang::syms(sort_cols))
}



#' @title CompareIMdataframes
#' @name CompareIMdataframes
#' @author Jaap Slootweg
#' @description
#' Compares two IM data frames (assumed cleaned and sorted), returning differences using \code{DiffDatasets}.
#'
#' @param data1 First data frame.
#' @param data2 Second data frame.
#' @param combine Logical; if TRUE (default), include a combined diff output.
#' @param ... Further arguments passed to \code{DiffDatasets} (e.g., compare_cols).
#'
#' @return
#' A list with:
#' \itemize{
#'   \item \code{diff_1_not_2}: Rows in \code{data1} not in \code{data2}
#'   \item \code{diff_2_not_1}: Rows in \code{data2} not in \code{data1}
#'   \item \code{combined}: Combined diff (if combine=TRUE)
#' }
#'
#' @export
CompareIMdataframes <- function(data1, data2, combine = TRUE,
                                colselection = c("Parameter.CASnummer","Eenheid.code","Grootheid.code","Limietsymbool","Begindatum",
                                                 "Resultaatdatum","Meetobject.lokaalID","Numeriekewaarde",
                                                 "Alfanumeriekewaarde","Hoedanigheid.code",
                                                 "THEdate","SampleID","PreTreatment"),
                                ...) {

  data1 <- ArrangeIMdata(data1)
  data2 <- ArrangeIMdata(data2)

  diffs <- DiffDatasets(data1[,colselection], data2[,colselection], combine = combine, ...)
  # or use testthat::expect_equal()

  result <- list(
    diff_1_not_2 = diffs$diff_1_not_2,
    diff_2_not_1 = diffs$diff_2_not_1
  )
  if (!is.null(diffs$combined)) {
    result$combined <- diffs$combined
  }
  result
}


#' @title CompareIMformat
#' @name CompareIMformat
#' @author Jaap Slootweg
#' @description
#' Reads and cleans two IM format files using \code{leesIMformat}, sorts the cleaned data, compares them using \code{CompareIMdataframes}, and returns both the cleaned data and the differences.
#'
#' @param file1 Path to the first input file (CSV, XLSX, or data.frame).
#' @param file2 Path to the second input file (CSV, XLSX, or data.frame).
#' @param National Language/format setting for the first file ("Nederlands" or "English"). Default: "Nederlands".
#' @param SSDbron Source of SSD / chemical data for the first file. Default: "Gross".
#' @param MolMassName Name of the column for molar mass for the first file. Default: "MW.g.Mol".
#' @param National2 Language/format for the second file. Default: same as \code{National}.
#' @param SSDbron2 Source of SSD / chemical data for the second file. Default: same as \code{SSDbron}.
#' @param MolMassName2 Molar mass column for the second file. Default: same as \code{MolMassName}.
#' @param verbose Logical; if \code{TRUE}, enables warning messages.
#' @param combine Logical; if TRUE (default), include a combined diff output.
#' @param ... Further arguments passed to \code{DiffDatasets} (e.g., compare_cols).
#'
#' @return
#' A list with
#' \itemize{
#'   \item \code{data1}: Cleaned and sorted data from the first file
#'   \item \code{data2}: Cleaned and sorted data from the second file
#'   \item \code{diff_1_not_2}: Rows in \code{data1} not in \code{data2}
#'   \item \code{diff_2_not_1}: Rows in \code{data2} not in \code{data1}
#'   \item \code{combined}: Combined diff (if combine=TRUE)
#' }
#'
#' @export
CompareIMformat <- function(file1, file2,
                            National = "Nederlands",
                            SSDbron = "Gross",
                            MolMassName = "MW.g.Mol",
                            National2 = National,
                            SSDbron2 = SSDbron,
                            MolMassName2 = MolMassName,
                            verbose = TRUE,
                            combine = TRUE,
                            ...) {
  # Clean both files
  res1 <- leesIMformat(file1, National = National, SSDbron = SSDbron, MolMassName = MolMassName, verbose = verbose)
  res2 <- leesIMformat(file2, National = National2, SSDbron = SSDbron2, MolMassName = MolMassName2, verbose = verbose)


  # Compare data frames
  diffs <- CompareIMdataframes(res1$inputData[,colselection], res1$inputData[,colselection], combine = combine, ...)

  # Return results, including combined if present
  result <- list(
    data1 = res1$inputData,
    data2 = res1$inputData,
    diff_1_not_2 = diffs$diff_1_not_2,
    diff_2_not_1 = diffs$diff_2_not_1
  )
  if (!is.null(diffs$combined)) {
    result$combined <- diffs$combined
  }
  result
}
