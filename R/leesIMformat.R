#' @title leesIMformat
#' @name leesIMformat
#' @author jaap slootweg
#' @description inlezen IM data, het format voor waterschappen en het informatiehuis water:
# https://www.waterkwaliteitsportaal.nl/WKP.WebApplication/Beheer/Data/Bulkdata
#' to test: source("R/ValidCAS.R"); source("R/leesIMformat.R"); data(Gross, Modifyers, UnitConversions, ModifierDefaults, OtherChar);
#' leesIMformat("tests/testthat/testdata/SampleDataMetInt.csv", National = "English")
#' @param filename the input filename
#' @param SSDbron source of SSD / chemical data
#' @param gross_name name of the source of SSD / chemical data
#' @param muNames column names in SSDbron of SSD for acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"
#' @param sigmaNames  names in SSDbron of SSD for acute = "Acute2.0Dev10LogMassTox.ug.L" chronic = "Chronic2.0Dev10LogMassTox.ug.L"
#' @param sep field seperator character in csv file; default  (dutch) ";"
#' @param dec decimal character in csv file; default (dutch) ","
#' @param verbose on warning messages (= T)
#' @export
#' @return list of
#' 1) dataframe inputData = input concentrations, 2) DataSamples, 3) excludedData = rows excluded during processing, 4) inputwarnings
leesIMformat <- function(filename,
                         SSDbron = "Gross",
                         National, verbose = T,
                         include_cas_reference = FALSE,
                         app_columns_only = TRUE,
                         gross_name = "Gross2025") {
  #1 preparations #####
  # Read input file and split grootheden and parameters #####

  # Prepare sep and dec for .csv and .csv in .zip
  if (National == "Nederlands") {
    sep = ";"
    dec = ","
  } else {
    sep = ","
    dec = "."
  }

  if ("data.frame" %in% class(filename)) {
    inputData <- filename
  } else {
    file_to_read <- filename
    # Check for zip
    if (grepl("\\.zip$", tolower(file_to_read))) {
      # In a zip file, there can be a .csv or a .xlsx
      zipfile <- unzip(file_to_read, list = TRUE)$Name

      # .csv can be read in from the .zip directly
      if (endsWith(tolower(zipfile), ".csv")) {
        con <- unz(file_to_read, zipfile)
        inputData <- read.csv2(con, sep = sep, dec = dec, stringsAsFactors = FALSE)
      }

      # .xlsx in the zip file needs to be extracted.
      if (endsWith(tolower(zipfile), ".xlsx")) {
        extracted <- unzip(file_to_read, files = zipfile[1], exdir =  tempdir())
        file_to_read <- extracted
      }
    }

    # Now handle based on extension
    if (endsWith(tolower(file_to_read), ".xlsx")) {
      inputData <- openxlsx::read.xlsx(file_to_read)
    }
    if (endsWith(tolower(file_to_read), ".csv")) {
      inputData <- read.csv2(file = file_to_read, sep = sep, dec = dec, stringsAsFactors = FALSE)
    }
  }

  inputData$orig_rownr <- seq_len(nrow(inputData))

  # store for input to leesIMformat_app function below
  copy_inputData <- inputData

  # cleaning steps self

  #warnings/messages are collected in R6 object inputwarnings; see helpers.R
  init_warnings <- data.frame(
    code = "version",
    warningText = get_package_version_string("msPAF", National = National)
  )
  inputwarnings <- InputWarnings$new (init_warnings)

  inputwarnings$add("Gross", nl_text = paste0("Gross versie:  ",as.character(gross_name)[1]),
                    en_text = paste0("Gross version: ",as.character(gross_name)[1]), National = National)

  # in app as reactive (SSDbron (former Gross) might not change)
  if ("list" %in% class(SSDbron) && !"data.frame" %in% class(SSDbron)) {#data.frame is also a list
    SSDbron <- SSDbron[[gross_name]]
  }
  if (!("data.frame" %in% class(SSDbron))){
    SSDbron <- try(get(SSDbron))
    if (!("data.frame" %in% class(SSDbron))) {
        inputwarnings$add("NoSSDbron",
                          nl_text = "De SSD gegevens missen als bestand",
                          en_text = "datasource for SSD's not found",
                          National = National)
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings$warnings
      ))
    }
  }
  # For now, we only have CAS values as id for SSD, we can prepare for the near future
  if (!"substance_key" %in% colnames(SSDbron)){
    SSDbron$substance_key <- paste0("CAS:", SSDbron$CAS)
  }

  #names(inputData)
  UsedParameters <-
    c(
      "Parameter.code",
      "Parameter.CASnummer",
      # "Parameter.groep", "Typering.code",
      "Eenheid.code",
      "Grootheid.code",
      "Limietsymbool",
      "Begindatum",
      "Resultaatdatum",
      "Meetobject.lokaalID",
      "Numeriekewaarde"
    )
  OptionalParameters <- c(
    "Alfanumeriekewaarde",
    "Hoedanigheid.code",
    "orig_rownr",
    "substance_key",
    "SampleID",
    "THEdate",
    "orig_Molweight"
  )
  Eng2UsedTranslate <- IMColNameMapper$new(c(
    Parameter.code = "Parameter.code",
    CAS = "Parameter.CASnummer",
    Unit = "Eenheid.code",
    H2OParameter = "Grootheid.code",
    Quantification = "Hoedanigheid.code",
    Limit = "Limietsymbool",
    SampleDate = "Begindatum",
    Location = "Meetobject.lokaalID",
    MeasuredValue = "Numeriekewaarde"
  ))

  if (National != "Nederlands" && any(names(Eng2UsedTranslate$mapping) %in% names(inputData))) {
    #conform column names to Dutch IMformat
    #remember to pass the conformation
    conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate$mapping)]
    #check minimal list of columns (CAS, Location, SampleDate, MeasuredValue)
    NeededColumns <- c("CAS", "Location", "SampleDate", "MeasuredValue", "Unit")
    if (!all(NeededColumns %in% names(inputData))) {
      missingcolumns <- NeededColumns[!(NeededColumns %in% names(inputData))]
      inputwarnings$add(
        "MissingColumns",
        paste("benodigde kolommen:", paste(missingcolumns, collapse = ", ")),
        paste("needed columns:", paste(missingcolumns, collapse = ", ")),
        National
      )
      inputwarnings$add(
        "Language Setting",
        "De Engelse instelling vereist een , als scheidingsteken en een . als decimaal",
        "The English setting requires a , separator and a . decimal",
        National
      )
      return(list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings$warnings
      ))
    }

    # Use Eng2UsedTranslate to rename to Dutch if needed
    if (National != "Nederlands" && any(names(Eng2UsedTranslate$mapping) %in% names(inputData))) {
      inputData <- Eng2UsedTranslate$to_dutch(inputData)
    }

    # Only to comply with IMformat
    inputData$Resultaatdatum <- inputData$Begindatum

    # future aggregation; also for excluded_rows!
    # Convert to Date (day-month-year format)
    date_obj <- as.Date(inputData$Resultaatdatum, format = "%d-%m-%Y")
    # Extract the year
    inputData$jaar <- format(date_obj, "%Y")

    # Ensure minimal columns exist
    if (!"Parameter.code" %in% names(inputData)) inputData$Parameter.code <- ""
    inputData$Parameter.code[is.na(inputData$Parameter.code)] <- ""
    if (!"Limietsymbool" %in% names(inputData)) inputData$Limietsymbool <- ""
    if (!"Grootheid.code" %in% names(inputData)) inputData$Grootheid.code <- ""

    # Now check for all required Dutch columns
    if (!all(UsedParameters %in% names(inputData))) {
      missingcolumns <- UsedParameters[!(UsedParameters %in% names(inputData))]
      inputwarnings$add(
        "Missende kolommen",
        paste("nodig zijn:", paste(missingcolumns, collapse = ", ")),
        paste("needed columns:", paste(missingcolumns, collapse = ", ")),
        National
      )
      inputwarnings$add(
        "Taal/Language",
        "Het Nederlandse formaat gebruikt ; als scheiding, en , als decimaal",
        "The international format uses , as separator and . as decimal",
        National
      )
      return(list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings$data
      ))
    }
  }

  # All checks on alfanumeriekewaarde
  inputData <- check_alfanumeriekewaarde(inputData, inputwarnings, National)

  #test numeric
  numcheck_result <- check_numeriekewaarde(inputData, inputwarnings, National)
  inputData <- numcheck_result$inputData
  excluded_rows <- numcheck_result$excluded_rows

  #"Hoedanigheid.code" is optional; avoid errors if it's missing
  if (!"Hoedanigheid.code" %in% names(inputData)) {
    inputData$Hoedanigheid.code <- ""
  } else {
    inputData$Hoedanigheid.code <- ifelse(is.na(inputData$Hoedanigheid.code),
                                          "",
                                          as.character(inputData$Hoedanigheid.code))
  }
  # "na filtering" can be combined with other "hoedanigheid"
  inputData$PreTreatment <- ifelse(endsWith(tolower(inputData$Hoedanigheid.code), "nf"), "nf", "")
  #TODO also for international?
  # TODO warnings if PreTreatment T/F in same sample
  # TODO warnings if NH4 & NH3 in same sample, How to establish same sample if multiple samples per year?!

  inputData$orig_Molweight <- lookup_molweight(inputData, SSDbron)

  #'   \item{T}{If Grootheid.code contains \code{"T"} but not \code{"Tw"}, all \code{"T"} are interpreted as water temperature (\code{"Tw"}).}
  #'   \item{Corg}{If Parameter.code is \code{"Corg"} and Hoedanigheid.code ends with \code{"nf"}, these are recoded to \code{"DOC"} (Dissolved Organic Carbon) after filtration.}
  #'   \item{OS}{If Parameter.code is \code{"OS"}, these are recoded to \code{"TSS"} (Total Suspended Solids).}
  inputData <- handle_grootheden_peculiarities(inputData, inputwarnings, National, verbose)

  # find a date and combine the location and date into a Sample ID
  inputData <- ensure_thedate_and_sampleid(inputData, inputwarnings, National)

  #modifyers & substance matching
  ions <- c("Ca","Mg","Na","Cl")
  grootheden <- Modifyers$ModifMODname[!Modifyers$ModifMODname %in% ions]
  MODgrootheden <- bind_rows(
    inputData[inputData$Grootheid.code %in% Modifyers$ModifMODname,
              c("SampleID", "Grootheid.code", "Eenheid.code", "Numeriekewaarde")],
    inputData[inputData$Parameter.code %in% Modifyers$ModifMODname,
              c("SampleID", "Parameter.code", "Eenheid.code", "Numeriekewaarde")] %>%
      rename(Grootheid.code = Parameter.code) # sometimes I'm NOT happy with the aquocodes
  )
  MODIons <-
    inputData[inputData$Parameter.code %in% ions,
              c("SampleID", "Parameter.code", "Eenheid.code", "Numeriekewaarde")]

  #remove from inputData, Ions as MODifying factors are not considered as toxicant!
  modifier_excluded <- inputData[
    inputData$Grootheid.code %in% Modifyers$ModifMODname |
      inputData$Parameter.code %in% ions,
  ]
  if (nrow(modifier_excluded) > 0) {
    modifier_excluded$ExclusionReason <- if (National == "Nederlands"){
      "Modificerende variabele"} else{ "Modifying variable"}
    excluded_rows <- dplyr::bind_rows(excluded_rows, modifier_excluded)
  }
  inputData <-
    inputData[!(
      inputData$Grootheid.code %in% Modifyers$ModifMODname |
        inputData$Parameter.code %in% Modifyers$ModifMODname
    ), ]


  # apply unitconversions to MODIons
  modions_result <- convert_units_and_calculate_concentration(MODIons, SSDbron, UnitConversions,
                                                       inputwarnings, National)
  MODIons <- modions_result$inputData
  # Note: We don't track exclusions from MODIons since they're already excluded as modifiers
  # No conversions for MODgrootheden, just check units?

  #combine the defaults, MOD & MOD  #####
  DataSamples <- data.frame(SampleID = unique(inputData$SampleID),
                            stringsAsFactors = F)

  # pivot for grootheden
  DataSamples <- pivot_samples(DataSamples, ModData = MODgrootheden, Modifyers = Modifyers,
                               inputwarnings = inputwarnings, National = National)
  DataSamples <- pivot_samples(DataSamples, ModData = MODIons, Modifyers = Modifyers,
                               inputwarnings = inputwarnings, National = National)
  # extend with defaults
  DataSamples <- fill_modifiers_with_defaults(DataSamples, ModifierDefaults)

  #for easier name handling in package
  #REPLACED inputData$ChemCode <- inputData$Parameter.code, BY

  OtherCharAttr <- attr(SSDbron, which = "OtherChar")
  if (is.null(OtherCharAttr)) {
    inputwarnings$add("MissingOtherChar",
                      nl_text = "Chem. key (aquocodes!) vertaling mist",
                      en_text = "Chem. key translation table is missing",
                      National)
    inputData$substance_key <- inputData$Parameter.CASnummer
  } else {
    if (any(duplicated(OtherCharAttr$ChemCode))){
      inputwarnings$add("duplic_OtherChar",
                        nl_text = "Dubbele ChemCode in 'OtherChar'",
                        en_text = "Duplication for ChemCode in 'OtherChar'",
                        National)
    }
    inputData$substance_key <- MatchChem_optReplace(inputData, SSDbron, OtherCharAttr, inputwarnings, National)
  }

  # filter only substances
  noSubstance <- which(is.na(inputData$substance_key))
  noSubParameters <- do.call( paste,
    inputData[noSubstance,] %>%
    pull(Parameter.code) %>% unique() %>% as.list())
  inputwarnings$add("NoTox",
                    nl_text = paste("Parameter.code niet als toxisch herkend voor: ", noSubParameters),
                    en_text = paste("Substance not matched to toxic effect", noSubParameters),
                    National = National)
  noSubstanceRows <- inputData[noSubstance,]
  noSubstanceRows$ExclusionReason <- "NoTox"
  excluded_rows <- dplyr::bind_rows(excluded_rows, noSubstanceRows)
  inputData <- inputData[-noSubstance, ]

  # TODO never cases like ??:
  # inputData %>%
  #      group_by(SampleID, substance_key) %>%
  #      summarise(
  #          has_empty = any(Limietsymbool == "" & PreTreatment == ""),
  #          has_lt    = any(Limietsymbool == "<" & PreTreatment == "nf")
  #      ) %>%
  #      filter(has_empty & has_lt)

  # Verify possible detection/quantification limit - by the < character ONLY now, we need substance_key and SampleID
  filter_result <- filter_limietsymbool(inputData, inputwarnings, National, verbose, UsedParameters, OptionalParameters)
  inputData <- filter_result$inputData
  excluded_rows <- dplyr::bind_rows(excluded_rows, filter_result$excluded_rows)

  # REMOVED; prioritise usecase to allow multiple measurement (for example per year)
  # inputData <- clean_filtering_pairs(inputData, "PreTreatment", group_cols = "SampleID", National = National, inputwarnings = inputwarnings)


  if (National == "Nederlands"){
    print("Na leesIMformat(), zijn de volgende verwijderd:")
  }else{
    print("After leesIMformat(), this amount is excluded:")
  }
  print(table(excluded_rows$ExclusionReason))

  #return
  output_mspaf <- list(
    inputData = inputData,
    DataSamples = DataSamples,
    inputwarnings = inputwarnings$warnings,
    excludedData = excluded_rows
  )

  return(output_mspaf)
}

