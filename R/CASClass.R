#' CASClass: R6 Class for Handling CAS Registry Numbers
#'
#' An R6 class for storing, validating, converting, and looking up CAS (Chemical Abstracts Service) registry numbers.
#'
#' @field cas_codes A character vector of unique CAS codes.
#' @field match2nonunique An integer vector mapping the original (possibly non-unique) input to unique CAS codes.
#' @field unique_validity A logical vector indicating the validity of each unique CAS code.
#' @field uniq_cas_int An integer vector of unique CAS codes (core digits only, no checksum).
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(cas_codes = NULL)}{Creates a new `CASClass` object with specified CAS codes.}
#'   \item{ValidCAS(CAScodes = NULL)}{Validates CAS codes for correct format and checksum. Returns a logical vector.}
#'   \item{set_from_int(cas_integer)}{Sets CAS codes from an integer vector of core digits (no checksum). Returns formatted CAS codes.}
#'   \item{cas_to_int_core(cas_codes = NULL)}{Extracts integer core digits from CAS registry numbers, excluding checksum.}
#'   \item{lookup_in_db(db, ...)}{Looks up CAS codes in a database (requires a compatible database connection object).}
#' }
#'
#' @examples
#' obj <- CASClass$new(c("50-00-0", "7732-18-5"))
#' obj$ValidCAS()
#' obj$set_from_int(c(773218, 50000))
#' obj$cas_to_int_core(c("7732-18-5", "50-00-0"))
#'
#' @export
CASClass <- R6::R6Class(
  "CASClass",
  public = list(
    # Fields
    cas_codes = NULL,       #  vector of unique cas codes
    match2nonunique = NULL, #  the index to the original, possibly non-unique vector
    unique_validity = NULL, #  vector of validity of cas_codes (apply match for original)
    uniq_cas_int = NULL,    #  vector of unique CAS as integer of core_digits only; no checksum

    #' @description
    #' Create a new CASClass object.
    #'
    #' @param cas_codes Optional character vector of CAS registry numbers to initialize the object.
    #'   Only unique CAS codes are stored in the object.
    #' @return A new \code{CASClass} object.
    initialize = function(cas_codes = NULL) {
      self$cas_codes <- as.character(unique(cas_codes))
      self$match2nonunique <- match(cas_codes, self$cas_codes)
    },

    #' @description
    #' Calculate CAS checksum digit for a vector of CAS core digits (no dashes, no checksum).
    #'
    #' @param nodash_vec Character vector of CAS core digits (no dashes, no checksum digit).
    #' @return Integer vector of checksum digits.
    .cas_calculate_checksum = function(nodash_vec) {
      CAS_WEIGHTS <- c(9,8,7,6,5,4,3,2,1)
      digitlist <- strsplit(nodash_vec, "")
      n_digits <- sapply(digitlist, length)
      maxlen <- max(n_digits)
      # Build matrix, pad with 0 at the front
      mat <- matrix(0, nrow=length(nodash_vec), ncol=maxlen)
      for(i in seq_along(digitlist)) {
        d <- as.integer(digitlist[[i]])
        mat[i, (maxlen - length(d) + 1):maxlen] <- d
      }
      # For each CAS code, select the proper weights and calculate checksum
      checks <- integer(length(nodash_vec))
      for(i in seq_along(nodash_vec)) {
        len <- n_digits[i]
        if (len > 9 || len < 2) {
          checks[i] <- NA  # not a valid length for CAS core digits
        } else {
          weights <- CAS_WEIGHTS[(10-len):9]
          digits <- mat[i, (maxlen-len+1):maxlen]
          checks[i] <- sum(digits * weights) %% 10
        }
      }
      checks
    },

    #' @description
    #' Validate CAS codes for correct format and checksum.
    #'
    #' @param CAScodes Optional character vector of CAS codes. If NULL, uses object's codes.
    #' @return Logical vector: TRUE if valid, FALSE otherwise (in input order).
    ValidCAS = function(CAScodes = NULL) {
      # Use object's codes if argument is NULL
      if (!is.null(CAScodes)) {
        self$cas_codes <- as.character(unique(CAScodes))
        self$match2nonunique <- match(CAScodes, self$cas_codes)
      }
      cascodes <- self$cas_codes

      valid_format <- grepl("^\\d{2,7}-\\d{2}-\\d$", cascodes)
      out <- rep(FALSE, length(cascodes))
      idx <- which(valid_format)
      if (length(idx) == 0) return(out)

      # Remove dashes
      nodash <- gsub("-", "", cascodes[idx])
      # Get the core digits (everything except the last digit)
      core_digits <- substr(nodash, 1, nchar(nodash) - 1)
      # Get the check digit (last digit)
      check_digit <- as.integer(substr(nodash, nchar(nodash), nchar(nodash)))

      # Calculate expected checksum using your new function
      calculated_checksum <- self$.cas_calculate_checksum(core_digits)

      # Compare calculated checksum to actual check digit
      out[idx] <- calculated_checksum == check_digit

      # Store results in fields
      self$unique_validity <- out
      names(self$unique_validity) <- self$cas_codes
      # in non-unique order:
      return(out[self$match2nonunique])

    },

    #' @description
    #' Set CAS codes from integer representation (core digits only).
    #'
    #' @param cas_integer Integer vector of CAS core digits (no checksum).
    #' @return Character vector of formatted CAS codes (in input order).
    set_from_int = function(cas_integer) {
      cas_as_int = as.integer(cas_integer)
      self$uniq_cas_int <- unique(cas_as_int)
      self$match2nonunique <- match(cas_as_int, self$uniq_cas_int)

      check_sums <- self$.cas_calculate_checksum(as.character(self$uniq_cas_int))

      part1_str <- self$uniq_cas_int %/% 100  # all but the last two digits
      part2_str <- self$uniq_cas_int %% 100   # last two digits
      # Final CAS codes
      cas_codes <- paste0(part1_str, "-",
                          sprintf("%02d", part2_str), "-",
                          check_sums)

      self$cas_codes <- cas_codes

      # in matching order
      self$cas_codes[self$match2nonunique]

    },

    #' @description
    #' Extracts the integer core digits from CAS registry numbers.
    #' Removes dashes and the checksum digit, returning the core digits as an integer.
    #' Only processes unique CAS codes and returns results in input order.
    #'
    #' @param cas_codes Character vector of CAS registry numbers (e.g. "7732-18-5").
    #' @return Integer vector of CAS core digits (excluding checksum), in input order.
    cas_to_int_core = function(cas_codes = NULL) {
      if(is.null(cas_codes)) {
        cas_codes =  as.character(self$cas_codes)
      } else
      { cas_codes <- as.character(cas_codes)
      }
      uniq_codes <- unique(cas_codes)
      match2nonunique <- match(cas_codes, uniq_codes)
      nodash <- gsub("-", "", uniq_codes)
      core_digits <- substr(nodash, 1, nchar(nodash) - 1)
      int_core <- as.integer(core_digits)
      int_core[match2nonunique]
    },

    #' @description
    #' Lookup information for CAS codes in a database.
    #'
    #' @param db PostgreSQLdb R6 object (connected).
    #' @param bigtable Name of the table in the database (default: "chem_keys").
    #' @param bigtable_schema Schema name (default: "public").
    #' @param join_col Name of the column in bigtable to join on (default: "CASRN").
    #' @param result_cols Columns to return (default: c("CASRN", "PREFERRED_NAME", "MS_READY_SMILES")).
    #' @param lookup_vec Optional override for lookup values (default: object's cas_codes).
    #' @return Data.frame with results.
    lookup_in_db = function(db,
                            bigtable = "chem_keys",
                            bigtable_schema = "public",
                            join_col = "CASRN",
                            result_cols = c("CASRN", "PREFERRED_NAME", "MS_READY_SMILES"),
                            lookup_vec = NULL) {
      if (is.null(lookup_vec)) {
        lookup_vec <- self$cas_codes
      }
      # call the db method (assumed to be called lookup_in_bigtable)
      db$lookup_in_bigtable(
        lookup_vec = lookup_vec,
        bigtable = bigtable,
        bigtable_schema = bigtable_schema,
        join_col = join_col,
        result_cols = result_cols
      )
    }

  )
)
