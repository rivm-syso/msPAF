#' @title DiffDatasets
#' @description
#' Compare two data frames and return the rows unique to each,
#' including their row numbers in the original data frames.
#'
#' @param data1 First data frame (already sorted as desired).
#' @param data2 Second data frame (already sorted as desired).
#' @param compare_cols Character vector: columns to use for comparison.
#'        If NULL, use intersection of column names in data1 and data2.
#' @param combine Logical; if TRUE (default), return a combined diff with origin column.
#'
#' @return A list with:
#'   - diff_1_not_2: rows in data1 not in data2, plus .rownum_1 (row number in data1)
#'   - diff_2_not_1: rows in data2 not in data1, plus .rownum_2 (row number in data2)
#'   - combined: (if combine=TRUE) a data frame with all diffs, ordered by row number, with '1_or_2' column
#'
#' @export
DiffDatasets <- function(data1, data2, compare_cols = NULL, combine = TRUE) {
  # Determine columns to compare
  if (is.null(compare_cols)) {
    compare_cols <- intersect(names(data1), names(data2))
  }

  # Subset to comparison columns
  data1_sub <- data1[, compare_cols, drop = FALSE]
  data2_sub <- data2[, compare_cols, drop = FALSE]

  # Add row numbers
  data1_sub$.rownum_1 <- seq_len(nrow(data1_sub))
  data2_sub$.rownum_2 <- seq_len(nrow(data2_sub))

  # Find diffs using anti_join
  diff_1_not_2 <- dplyr::anti_join(data1_sub, data2_sub, by = compare_cols)
  diff_2_not_1 <- dplyr::anti_join(data2_sub, data1_sub, by = compare_cols)

  out <- list(
    diff_1_not_2 = diff_1_not_2,
    diff_2_not_1 = diff_2_not_1
  )

  if (combine) {
    # Add origin and row number columns for combining
    if (nrow(diff_1_not_2) > 0) {
      diff_1_not_2$`1_or_2` <- "<<<"
      diff_1_not_2$.rownum <- diff_1_not_2$.rownum_1
    }
    if (nrow(diff_2_not_1) > 0) {
      diff_2_not_1$`1_or_2` <- ">>>"
      diff_2_not_1$.rownum <- diff_2_not_1$.rownum_2
    }
    # Make sure both have all columns
    all_cols <- unique(c(names(diff_1_not_2), names(diff_2_not_1)))
    diff_1_not_2 <- tibble::add_column(diff_1_not_2, !!!setNames(vector("list", length(setdiff(all_cols, names(diff_1_not_2)))), setdiff(all_cols, names(diff_1_not_2))))
    diff_2_not_1 <- tibble::add_column(diff_2_not_1, !!!setNames(vector("list", length(setdiff(all_cols, names(diff_2_not_1)))), setdiff(all_cols, names(diff_2_not_1))))
    # Bind and arrange
    combined <- dplyr::bind_rows(diff_1_not_2, diff_2_not_1)
    combined <- dplyr::arrange(combined, .rownum)
    out$combined <- combined
  }

  out
}


#' IM Column Name Mapper (English <-> Dutch)
#' @export
IMColNameMapper <- R6::R6Class(
  "IMColNameMapper",
  public = list(
    mapping = NULL,
    initialize = function(mapping) {
      self$mapping <- mapping
    },
    to_dutch = function(df) {
      eng_names <- names(self$mapping)
      dutch_names <- unname(self$mapping)
      matches <- which(names(df) %in% eng_names)
      names(df)[matches] <- self$mapping[names(df)[matches]]
      df
    },
    to_english = function(df) {
      # reverse mapping
      rev_map <- setNames(names(self$mapping), self$mapping)
      dutch_names <- names(rev_map)
      matches <- which(names(df) %in% dutch_names)
      names(df)[matches] <- rev_map[names(df)[matches]]
      df
    }
  )
)

#' Check Alfanumeriekewaarde column and add consistency warnings
#'
#' Ensures 'Alfanumeriekewaarde' exists in inputData, checks for
#' inconsistent use of limit symbols and numeric values, and adds warnings.
#'
#' @param inputData The data.frame to check/modify
#' @param inputwarnings An InputWarnings R6 object
#' @param National Language string ("Nederlands" or "English")
#' @return The modified inputData (with Alfanumeriekewaarde column if needed)
#' @export
check_alfanumeriekewaarde <- function(inputData, inputwarnings, National) {
  # Ensure column exists
  if (!"Alfanumeriekewaarde" %in% names(inputData)) {
    inputData$Alfanumeriekewaarde <- ""
  }

  # Check for inconsistent limit symbols
  HasBT.STAlfa <- (
    !is.na(inputData$Alfanumeriekewaarde) &
      startsWith(
        ifelse(is.na(inputData$Alfanumeriekewaarde),"",inputData$Alfanumeriekewaarde),
        prefix = "<") &
      inputData$Limietsymbool != "<"
  ) |
    (
      !is.na(inputData$Alfanumeriekewaarde) &
        startsWith(
          ifelse(is.na(inputData$Alfanumeriekewaarde),"",inputData$Alfanumeriekewaarde),
          prefix = ">") &
        inputData$Limietsymbool != ">"
    )
  if (any(HasBT.STAlfa)) {
    inputwarnings$add(
      "InconsistentLimiet",
      "< en/of > symbool in alfanum; ontbreekt als Limietsymbool",
      "< and/or > symbol in alfanum; missing as Limietsymbool",
      National
    )
  }

  # Check for numeric in alfanum with missing Numeriekewaarde
  AlsoNumeric <- suppressWarnings(as.numeric(inputData$Alfanumeriekewaarde))
  if (any(!is.na(AlsoNumeric) & is.na(inputData$Numeriekewaarde))) {
    inputwarnings$add(
      "InconsistentAlfaNum",
      "Waarde(n) in alfanum lijken te ontbreken als Numeriekewaarde",
      "value(s) in alfanum that seems missing as Numeriekewaarde",
      National
    )
  }

  inputData
}

#' Ensure Numeriekewaarde is numeric and warn on dropped rows
#'
#' Converts Numeriekewaarde to numeric (handles Dutch commas), removes rows with NA,
#' and adds appropriate warnings to inputwarnings.
#'
#' @param inputData The data.frame to check/modify
#' @param inputwarnings An InputWarnings R6 object
#' @param National Language string ("Nederlands" or "English")
#' @return List with inputData (valid rows) and excluded_rows
#' @export
check_numeriekewaarde <- function(inputData, inputwarnings, National) {
  # Ensure numeric, handle comma/point
  if (!is.numeric(inputData$Numeriekewaarde)) {
    inputData$Numeriekewaarde <- gsub(",", ".", inputData$Numeriekewaarde)
    inputData$Numeriekewaarde <- as.numeric(inputData$Numeriekewaarde)
  }

  excluded_rows <- data.frame()
  # Remove NAs and warn if any dropped
  if (anyNA(inputData$Numeriekewaarde)) {
    NumNA <- sum(is.na(inputData$Numeriekewaarde))
    excluded_rows <- inputData[is.na(inputData$Numeriekewaarde), ]
    excluded_rows$ExclusionReason <- if (National == "Nederlands") "Niet-numerieke waarde" else "Non-numeric value"
    inputData <- inputData[!is.na(inputData$Numeriekewaarde), ]
    inputwarnings$add(
      if (National == "Nederlands") "Niet-Numeriek" else "NonNumeric",
      paste("Geen Numerieke waarde, aantal rijen overgeslagen:", NumNA),
      paste("Non Numeric Numeriekewaarde, rows deleted:", NumNA),
      National
    )
  }
  list(inputData = inputData, excluded_rows = excluded_rows)
}

#' Remove rows with Limietsymbool < or > and warn if any removed
#'
#' @param inputData The data.frame to check/modify
#' @param inputwarnings An InputWarnings R6 object
#' @param National Language string ("Nederlands" or "English")
#' @param verbose Logical, whether to add warnings
#' @param UsedParameters Character vector of required columns (for subsetting)
#' @param OptionalParameters Character vector of optional columns (for subsetting)
#' @return The modified inputData (with limit rows removed)
#' @export
filter_limietsymbool <- function(inputData, inputwarnings, National, verbose, UsedParameters, OptionalParameters) {
  # now split > and < handling! Only < should be included in the percentile calculations in aggregate HU's
  keep_cols <- intersect(c(UsedParameters, OptionalParameters), names(inputData))
  trimmed <- trimws(as.character(inputData$Limietsymbool))
  inputData$Limietsymbool <- ifelse(is.na(trimmed), "", trimmed)

  belowLimit <- sum(inputData$Limietsymbool == "<")
  excluded_rows <- data.frame()
  if (belowLimit > 0) {
    if (verbose) {
      inputwarnings$add(
        "Below limit",
        paste(belowLimit, "rijen verwijderd met Limietsymbool <"),
        paste(belowLimit, "number of rows removed with Limietsymbool <"),
        National
      )
    }
    excluded_rows <- inputData[inputData$Limietsymbool == "<", keep_cols, drop=FALSE]
    excluded_rows$ExclusionReason <- if (National == "Nederlands") "Limietsymbool <" else "Limietsymbool <"
    inputData <- inputData[inputData$Limietsymbool != "<", keep_cols, drop=FALSE]
  }

  aboveLimit <- sum(inputData$Limietsymbool == ">")
  if (aboveLimit > 0) {
    if (verbose) {
      inputwarnings$add(
        "Above limit",
        paste(aboveLimit, "rijen verwijderd met Limietsymbool >"),
        paste(aboveLimit, "number of rows removed with Limietsymbool >"),
        National
      )
    }
    excluded_rows_above <- inputData[inputData$Limietsymbool == ">", keep_cols, drop=FALSE]
    excluded_rows_above$ExclusionReason <- if (National == "Nederlands") "Limietsymbool >" else "Limietsymbol >"
    inputData <- inputData[inputData$Limietsymbool != ">", keep_cols, drop=FALSE]
    excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_rows_above)
  }
  list(inputData = inputData, excluded_rows = excluded_rows)
}

#' Handle special grootheden and parameter code conversions
#'
#' This function processes and corrects known peculiarities in the input data
#' for water quality measurements, specifically for certain grootheden/parameter
#' codes that require conversion or special handling according to the IM standard.
#'
#' The following conversions are performed:
#' \describe{
#'   \item{T}{If Grootheid.code contains \code{"T"} but not \code{"Tw"}, all \code{"T"} are interpreted as water temperature (\code{"Tw"}).}
#'   \item{Corg}{If Parameter.code is \code{"Corg"} and Hoedanigheid.code ends with \code{"nf"}, these are recoded to \code{"DOC"} (Dissolved Organic Carbon) after filtration.}
#'   \item{OS}{If Parameter.code is \code{"OS"}, these are recoded to \code{"TSS"} (Total Suspended Solids).}
#' }
#'
#' For each conversion, a warning is added to the inputwarnings object (in Dutch or English, as appropriate).
#'
#' @param inputData The data.frame containing the measurement data.
#' @param inputwarnings An InputWarnings R6 object for collecting warnings.
#' @param National The language string ("Nederlands" or "English").
#' @param verbose Logical; if TRUE, adds extra warnings for Corg and OS conversions.
#' @return The modified inputData data.frame with the above conversions applied.
#' @export
handle_grootheden_peculiarities <- function(inputData, inputwarnings, National, verbose) {
  grootheden <- unique(inputData[, c("Grootheid.code", "Parameter.code")])

  # T = Tw
  if ("T" %in% grootheden$Grootheid.code &&
      !("Tw" %in% grootheden$Grootheid.code)) {
    inputwarnings$add(
      "AquoCode.Tw",
      "T gevonden, geen Tw; alle T is gelezen als watertemperature (Tw)",
      "T found but no Tw; assuming all T is temperature of water (Tw)",
      National
    )
    inputData$Grootheid.code[inputData$Grootheid.code == "T"] <- "Tw"
  }

  # Corg = DOC (for Hoedanigheid.code ending with nf)
  if ("Corg" %in% grootheden$Parameter.code) {
    idx <- inputData$Parameter.code == "Corg" & endsWith(tolower(inputData$Hoedanigheid.code), "nf")
    inputData$Parameter.code[idx] <- "DOC"
    if (verbose) {
      inputwarnings$add(
        "Corg2DOC",
        "Waarden van Corg, na filtering worden als DOC gebruikt",
        "Corg values 'na filtering' are set as modifyer DOC",
        National
      )
    }
  }

  # OS = TSS
  if ("OS" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "OS"] <- "TSS"
    if (verbose) {
      inputwarnings$add(
        "OS2TSS",
        "Waarden van OS worden als TSS gebruikt",
        "OS values are set as modifyer TSS",
        National
      )
    }
  }
  inputData
}

#' Ensure THEdate and SampleID columns from date/location info
#'
#' Attempts to create a THEdate column using Begindatum or Resultaatdatum,
#' removes rows with missing dates, and adds a warning if any removed.
#' Creates a SampleID column combining location and date.
#'
#' @param inputData The data.frame to modify.
#' @param inputwarnings An InputWarnings R6 object for collecting warnings.
#' @param National Language string ("Nederlands" or "English").
#' @return The modified inputData (with THEdate and SampleID).
#' @export
ensure_thedate_and_sampleid <- function(inputData, inputwarnings, National) {
  # Try to get THEdate from Begindatum (as character)
  inputData$THEdate <- tryCatch(
    as.character(inputData$Begindatum),
    error = function(cond) return(NA),
    silent = TRUE
  )
  # Fill missing THEdate from Resultaatdatum
  missing_idx <- is.na(inputData$THEdate)
  if (any(missing_idx)) {
    inputData$THEdate[missing_idx] <- tryCatch(
      as.character(inputData$Resultaatdatum[missing_idx]),
      error = function(cond) return(NA),
      silent = TRUE
    )
  }
  # Warn and remove rows with missing THEdate
  if (any(is.na(inputData$THEdate))) {
    n_missing <- sum(is.na(inputData$THEdate))
    inputwarnings$add(
      "NoDateData",
      paste(n_missing, "rijen zonder datum zijn verwijderd"),
      paste(n_missing, "data with a missing date are removed"),
      National
    )
    inputData <- inputData[!is.na(inputData$THEdate), ]
  }
  # setup Samples: unique date-locations
  if (all(c("Meetobject.lokaalID", "THEdate") %in% names(inputData))) {
    ToSample <- inputData[, c("Meetobject.lokaalID", "THEdate")]
    Astmp <- paste0(ToSample$Meetobject.lokaalID, ":", ToSample$THEdate)
    inputData$SampleID <- unlist(Astmp)
  }
  inputData
}

pivot_samples <- function(
    DataSamples, ModData, inputwarnings,
    National, Modifyers
) {
  # Determine present codes: those in Modifyers$ModifMODname that are present in ModData
  code_col <- if ("Grootheid.code" %in% names(ModData)) "Grootheid.code" else "Parameter.code"
  value_col <- if ("Grootheid.code" %in% names(ModData)) "Numeriekewaarde" else "Concentration"
  present_codes <- Modifyers$ModifMODname[Modifyers$ModifMODname %in% ModData[[code_col]]]

  for (VarName in present_codes) {
    DataSamples <- DataSamples %>%
      left_join(
        ModData %>%
          filter(!!sym(code_col) == VarName) %>%
          select(SampleID, value = !!sym(value_col)),
        by = "SampleID"
      ) %>%
      mutate(!!VarName := value) %>%
      select(-value)
  }
  DataSamples
}

lookup_molweight <- function(inputData, SSDbron) {
  # Check if CAS column exists in inputData
  has_cas <- "Parameter.CASnummer" %in% colnames(inputData)
  has_substance_key <- "substance_key" %in% colnames(inputData)

  # AquoCode join
  out <- inputData %>%
    left_join(SSDbron %>% select(AquoCode, CAS, MW.g.Mol) %>% filter(!is.na(AquoCode)),
              by = c("Parameter.code" = "AquoCode")) %>%
    rename(MW_g_Mol_1 = MW.g.Mol)

  # Conditional CAS join
  if (has_cas) {
    out <- out %>%
      left_join(SSDbron %>% select(AquoCode, CAS, MW.g.Mol) %>% filter(!is.na(CAS)),
                by = c("Parameter.CASnummer" = "CAS")) %>%
      rename(MW_g_Mol_2 = MW.g.Mol)
  } else {
    out$MW_g_Mol_2 <- NA
  }

  # Conditional substance_key join
  if (has_substance_key) {
    out <- out %>%
      left_join(SSDbron %>% select(CAS, MW.g.Mol) %>% filter(!is.na(CAS)) %>%
                  mutate(CAS_paste = paste0("CAS:", CAS)) %>%
                  select(CAS_paste, MW.g.Mol) ,
                by = c("substance_key" = "CAS_paste")) %>%
      rename(MW_g_Mol_3 = MW.g.Mol)
  } else {
    out$MW_g_Mol_3 <- NA
  }

  # Prioritize: AquoCode match > CAS match > substance_key match
  out %>%
    mutate(
      MW_g_Mol = case_when(
        !is.na(MW_g_Mol_1) ~ MW_g_Mol_1,
        !is.na(MW_g_Mol_2) ~ MW_g_Mol_2,
        !is.na(MW_g_Mol_3) ~ MW_g_Mol_3,
        TRUE ~ NA_real_
      )
    ) %>%
    pull(MW_g_Mol)
}
#' Convert units and compute concentrations for mapped substances
#'
#' Assumes inputData already has correct CAS codes. Converts units, computes concentrations,
#' and applies corrections for N/P forms. Warns and removes rows with unknown units or CAS.
#'
#' @param inputData Data.frame with measurement data (must have CAS, Numeriekewaarde, Eenheid.code, Hoedanigheid.code)
#' @param SSDbron Data.frame with reference substances (must have CAS and molecular mass column)
#' @param AllUnitConversions Data.frame of unit conversions (must have Unit_in, Unit_out, Multiply_by, Mul_molmass)
#' @param inputwarnings An InputWarnings R6 object
#' @param National Language string ("Nederlands" or "English")
#' @param MolMassName Name of molecular mass column in SSDbron
#' @param TargetUnit The unit to convert to (default "ug/l")
#' @return inputData with Concentration column added/updated
#' @export
convert_units_and_calculate_concentration <- function(
    inputData, SSDbron, UnitConversions, inputwarnings, National, TargetUnit = "ug/l"
) {
  #add the no-conversion needed for vectorisation
  AddUnitConversions <- data.frame(
    Unit_in = unique(UnitConversions$Unit_out),
    Unit_out = unique(UnitConversions$Unit_out),
    Multiply_by = 1,
    Mul_molmass = F
  )
  AllUnitConversions <- rbind (UnitConversions, AddUnitConversions)

  # 1. Match input units to conversion table (now flexible with TargetUnit)
  TranslateUnits <- AllUnitConversions[AllUnitConversions$Unit_out == TargetUnit, ]
  GivenUnits <- inputData$Eenheid.code
  Factors <- TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in), ]

  # 2. Warn and remove unknown units
  excluded_rows <- data.frame()
  if (anyNA(Factors$Unit_in)) {
    UnknownUnits <- unique(GivenUnits[is.na(Factors$Unit_in)])
    UnknownUnits_str <- paste("'", paste(UnknownUnits, collapse = "','"), "'", sep = "")
    inputwarnings$add(
      "Unknown",
      paste(UnknownUnits_str, "onbekende concentratie eenheid; De rijen zijn verwijderd"),
      paste(UnknownUnits_str, "unknown concentration unit(s); row(s) deleted"),
      National
    )
    keep <- !is.na(Factors$Unit_in)
    excluded_rows <- inputData[!keep, , drop = FALSE]
    excluded_rows$ExclusionReason <- if (National == "Nederlands") "Onbekende eenheid" else "Unknown unit"
    inputData <- inputData[keep, , drop = FALSE]
    Factors <- Factors[keep, , drop = FALSE]
    row.names(inputData) <- NULL
    row.names(Factors) <- NULL
  }

  # 3. Match chemical code between inputData and SSDbron
  if ("substance_key" %in% colnames(inputData)) {
    MatchChem <- match(inputData$substance_key, SSDbron$substance_key)
    stopifnot(!anyNA(MatchChem)) # must be picked up at earlier tests!
  } else {
    # no checked substance_key.. for modifying IONS?!
    match_aquo <- match(inputData$Parameter.code, SSDbron$AquoCode)
    match_aquo <- match(inputData$Parameter.code, SSDbron$AquoCode)
    if ("Parameter.CASnummer" %in% colnames(inputData)) {
      match_cas <- match(inputData$Parameter.CASnummer, SSDbron$CAS)
    } else {
      match_cas <- rep(NA_integer_, nrow(inputData))
    }
    MatchChem <- ifelse(!is.na(match_aquo), match_aquo, match_cas)
    # Find rows where neither AquoCode nor CAS matched
    missing_idx <- which(is.na(MatchChem))

    if (anyNA(MatchChem)) {
      # Collect the missing codes for warning
      missing_aquo <- unique(inputData$Parameter.code[missing_idx])

      # If CAS is present, include missing CAS numbers too
      if ("Parameter.CASnummer" %in% colnames(inputData)) {
        missing_cas <- unique(inputData$Parameter.CASnummer[missing_idx])
        missing_info <- paste(
          "AquoCode: '", paste(missing_aquo, collapse = "','"), "'",
          "; CAS: '", paste(missing_cas, collapse = "','"), "'",
          sep = ""
        )
      } else {
        missing_info <- paste("AquoCode: '", paste(missing_aquo, collapse = "','"), "'", sep = "")
      }
      inputwarnings$add(
        "Unknown",
        paste(missing_info, "niet gevonden in SSDbron; De rijen zijn verwijderd"),
        paste(missing_info, "not found in SSDbron; row(s) deleted"),
        National
      )
      keep <- !is.na(MatchChem)
      excluded_cas <- inputData[!keep, , drop = FALSE]
      excluded_cas$ExclusionReason <- if (National == "Nederlands") "Stof(code) niet in SSDbron" else "substance not in SSDbron"
      excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_cas)
      inputData <- inputData[keep, , drop = FALSE]
      Factors <- Factors[keep, , drop = FALSE]
      MatchChem <- MatchChem[keep]
      row.names(inputData) <- NULL
      row.names(Factors) <- NULL
    }
  }

  # 4. Calculate concentration
  inputData$Concentration <- inputData$Numeriekewaarde * Factors$Multiply_by
  toMul <- which(Factors$Mul_molmass)
  if (length(toMul) > 0) {

    molmass_values <- SSDbron[MatchChem[toMul], "orig_Molweight"]
    # Warn and remove if molecular mass is missing
    na_molmass <- is.na(molmass_values)
    if (any(na_molmass)) {
      missing_molmass_cas <- unique(inputData$CAS[toMul][na_molmass])
      missing_molmass_str <- paste("'", paste(missing_molmass_cas, collapse = "','"), "'", sep = "")
      inputwarnings$add(
        "Unknown",
        paste(missing_molmass_str, "heeft geen molmassa in SSDbron; De rijen zijn verwijderd"),
        paste(missing_molmass_str, "missing molecular mass in SSDbron; row(s) deleted"),
        National
      )
      keep <- rep(TRUE, nrow(inputData))
      keep[toMul[na_molmass]] <- FALSE
      excluded_molmass <- inputData[!keep, , drop = FALSE]
      excluded_molmass$ExclusionReason <- if (National == "Nederlands") "Ontbrekende molmassa" else "Missing molecular mass"
      excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_molmass)
      inputData <- inputData[keep, , drop = FALSE]
      Factors <- Factors[keep, , drop = FALSE]
      MatchChem <- MatchChem[keep]
      toMul <- which(Factors$Mul_molmass)
      molmass_values <- SSDbron[MatchChem[toMul], "orig_Molweight"] # vector has changed!
      row.names(inputData) <- NULL
      row.names(Factors) <- NULL
    }
    inputData$Concentration[toMul] <- inputData$Concentration[toMul] * molmass_values
  }

  # 5. Special handling for N/P forms (if needed)
  if ("Hoedanigheid.code" %in% names(inputData)){
    inputData$NaFiltering <- endsWith(inputData$Hoedanigheid.code, "nf")
    AsN <- which(inputData$Hoedanigheid.code %in% c("N", "Nnf"))
    if (length(AsN) > 0) {
      ATOMMASSN <- 14.0067
      inputData$Concentration[AsN] <- inputData$Concentration[AsN] / ATOMMASSN * inputData$orig_Molweight[AsN]
    }
    AsP <- which(inputData$Hoedanigheid.code %in% c("P", "Pnf"))
    if (length(AsP) > 0) {
      ATOMMASSP <- 30.973762
      inputData$Concentration[AsP] <- inputData$Concentration[AsP] / ATOMMASSP * inputData$orig_Molweight[AsP]
    }
  }

  list(inputData = inputData, excluded_rows = excluded_rows)
}

# Call after convert_units_and_calculate_concentration, so proper MolWeight is used!
MatchChem_optReplace <- function(inputData, SSDbron, ChemReplace, inputwarnings, National){
  if (!is.null(ChemReplace)) { # create proper lookup substance_key from various identifyers (EC number, Dutch aquocode, wrong CASRN)

    stopifnot ("ChemCode" %in% names(ChemReplace),
               "substance_key" %in% names(ChemReplace))
    # # Some CAS users omit the - - (thus avoiding excel CAS - date messing) make it consistent Or replace the "-" by "#"
    cas_digits <- gsub("[^0-9]", "", inputData$Parameter.CASnummer)
    inputData$Parameter.CASnummer <- sub("^(\\d+)(\\d{2})(\\d)$", "\\1-\\2-\\3", cas_digits)

    ChemReplace <- ChemReplace %>% distinct(ChemCode, .keep_all = TRUE)
    Lookup <- inputData %>% select(Parameter.code, Parameter.CASnummer) %>%
      mutate(is_ec = startsWith(as.character(Parameter.code), "EC:")) %>%   # 1. Add EC flag
      left_join(ChemReplace, by = c("Parameter.code" = "ChemCode")) %>%
      rename(substance_key_1 = substance_key) %>%
      left_join(ChemReplace, by = c("Parameter.CASnummer" = "ChemCode"))  %>%
      mutate(substance_key = case_when(
                 is_ec & !is.na(substance_key_1) ~ substance_key_1,  # EC match present: use it
                 !is.na(substance_key_1) ~ substance_key_1,          # code match present: use it
                 TRUE ~ substance_key                                # otherwise, use CAS match
               )) %>%
      pull(substance_key)
  }

}


#' Fill or create DataSamples modifier columns with defaults
#'
#' For each modifier (TSS, POC, DOC, pH, Tw, Ca, Mg, Na, Cl), fills NA values with
#' the corresponding default from ModifierDefaults, or creates the column if missing.
#'
#' @param DataSamples Data.frame of samples to update
#' @param ModifierDefaults Data.frame (single row) with default values for all modifiers
#' @return Updated DataSamples
#' @export
fill_modifiers_with_defaults <- function(DataSamples, ModifierDefaults) {
  # Get column names from ModifierDefaults
  modifier_columns <- names(ModifierDefaults)

  for (col in modifier_columns) {
    default_val <- ModifierDefaults[[col]][1]
    colname <- gsub("\\s*\\([^)]*\\)", "", col)
    if (colname %in% names(DataSamples)) {
      DataSamples[[colname]][is.na(DataSamples[[colname]])] <- default_val
    } else {
      DataSamples[[colname]] <- rep(default_val, nrow(DataSamples))
    }
  }

  DataSamples
}

