##############################################################################################
#First, make leesIMformat work################################################################
##############################################################################################
# Development code for turning the leesIMformat.R code itself into smaller functions
# Use this: https://gitlab.rivm.nl/slootwej/mspaf/-/blob/master/R/leesIMformat.R?ref_type=heads
# And use the Shiny package as starting point: https://github.com/rivm-syso/msPAFcalculator/blob/main/R/leesIMformat.R

rm(list=ls())
devtools::load_all()
library(tidyverse)

# Input file
Gross <- SSDplusListRDS$Gross2023

# Variables of the function
filename = "./data/msPAFtoolExampleNL.xlsx"
SSDbron = "Gross"
MolMassName = "MW.g.Mol"
National = "Nederlands"
verbose = T

# read inputfile as input to new script and old function both #####
if ("data.frame" %in% class(filename)) {
  inputData <- filename
} else {
  if (endsWith(filename, ".xlsx")) {
    inputData <- openxlsx::read.xlsx(filename)
  } else {
    if (National == "Nederlands") {
      sep = ";"
      dec = ","
    } else {
      sep = ","
      dec = "."
    }
    inputData <- read.csv2(file = filename, sep = sep, dec = dec, stringsAsFactors = F)
  }
}

inputData$orig_rownr <- seq_len(nrow(inputData))

# store for input to leesIMformat_app function below
copy_inputData <- inputData

# cleaning steps self

#warnings/messages are collected in R6 object inputwarnings; see helpers.R
updateDate <- format(file.info("server.R")$mtime, "%Y-%m-%d")

inputwarnings <- InputWarnings$new(get_package_version_string("msPAF"))
inputwarnings$add(updateDate, nl_text = "versie app", en_text = "app version", National = National)

# in app as reactive (SSDbron (former Gross) might not change)
if (!("data.frame" %in% class(SSDbron))){
  SSDbron <- try(get(SSDbron))
  if (!("data.frame" %in% class(SSDbron))) {
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoSSDbron","De SSD gegevens missen als bestand")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoSSDbron","datasource for SSD's not found")
    }
    return(  list(
      inputData = data.frame(),
      DataSamples = data.frame(),
      inputwarnings = inputwarnings
    ))
  }
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
  "orig_rownr"
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
      inputwarnings = inputwarnings$data
    ))
  }

  # Use col_mapper to rename to Dutch if needed
  if (National != "Nederlands" && any(names(col_mapper$mapping) %in% names(inputData))) {
    inputData <- col_mapper$to_dutch(inputData)
  }

  # Only to comply with IMformat
  inputData$Resultaatdatum <- inputData$Begindatum

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
inputData <- check_numeriekewaarde(inputData, inputwarnings, National)

#"Hoedanigheid.code" is optional; avoid errors if it's missing
if (!"Hoedanigheid.code" %in% names(inputData)) {
  inputData$Hoedanigheid.code <- ""
} else {
  inputData$Hoedanigheid.code <- ifelse(is.na(inputData$Hoedanigheid.code),
                                        "",
                                        as.character(inputData$Hoedanigheid.code))
}


# Verify possible detection/quantification limit - by the < character
inputData <- filter_limietsymbool(inputData, inputwarnings, National, verbose, UsedParameters, OptionalParameters)

#'   \item{T}{If Grootheid.code contains \code{"T"} but not \code{"Tw"}, all \code{"T"} are interpreted as water temperature (\code{"Tw"}).}
#'   \item{Corg}{If Parameter.code is \code{"Corg"} and Hoedanigheid.code ends with \code{"nf"}, these are recoded to \code{"DOC"} (Dissolved Organic Carbon) after filtration.}
#'   \item{OS}{If Parameter.code is \code{"OS"}, these are recoded to \code{"TSS"} (Total Suspended Solids).}
inputData <- handle_grootheden_peculiarities(inputData, inputwarnings, National, verbose)

# find a date and combine the location and date into a Sample ID
inputData <- ensure_thedate_and_sampleid(inputData, inputwarnings, National)

# # Some CAS users omit the - - (thus avoiding excel CAS - date messing) make it consistent
cas_digits <- gsub("[^0-9]", "", inputData$Parameter.CASnummer)
inputData$Parameter.CASnummer <- sub("^(\\d+)(\\d{2})(\\d)$", "\\1-\\2-\\3", cas_digits)

#modifyers & substance matching
ions <- c("Ca","Mg","Na","Cl")
grootheden <- Modifyers$ModifMODname[!Modifyers$ModifMODname %in% ions]
MODgrootheden <-
  inputData[inputData$Grootheid.code %in% Modifyers$ModifMODname,
            c("SampleID", "Grootheid.code", "Eenheid.code", "Numeriekewaarde")]
MODIons <-
  inputData[inputData$Parameter.code %in% ions,
            c("SampleID", "Parameter.code", "Eenheid.code", "Numeriekewaarde")]


#remove from inputData, Ions as MODifying factors are not considered as toxicant!
inputData <-
  inputData[!(
    inputData$Grootheid.code %in% Modifyers$ModifMODname | #never mind the ions...
      inputData$Parameter.code %in% ions
  ), ]


# apply unitconversions to MODIons
MODIons <- convert_units_and_calculate_concentration(MODIons, SSDbron, UnitConversions,
                                                    inputwarnings, National, MolMassName)

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
inputData$ChemCode <- inputData$Parameter.code
if ("MeasuredValue" %in% names(inputData)) {
  inputData$MeasuredValue <- NULL #free name for Concentration column
}
names(inputData)[names(inputData)=="Concentration"] <- "MeasuredValue"

# "na filtering" can be combined with other "hoedanigheid"
inputData$PreTreatment <- ifelse(endsWith(tolower(inputData$Hoedanigheid.code), "nf"), "nf", "")
#TODO also for international?

inputData <- clean_filtering_pairs(inputData, "PreTreatment", group_cols = "SampleID", National = National)

inputData <- convert_units_and_calculate_concentration(inputData, SSDbron, UnitConversions,
                                          inputwarnings, National, MolMassName)


conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)] #TODO ??
attr(inputData, "conforms") <- conforms

inputData <- check_substance_codes(inputData, OtherChar,
                      SSDbron = SSDbron,
                      inputwarnings = inputwarnings,
                      National = National)


#return
output_mspaf <- list(
  inputData = inputData,
  DataSamples = DataSamples,
  inputwarnings = inputwarnings
)

##############################################################################################
#Compare with Github/App version##############################################################
##############################################################################################

leesIMformat_app <- function(filename,
                             SSDbron = "Gross",
                             MolMassName = "MW.g.Mol",
                             National, verbose = T) {
  #1 preparations #####
  #warnings/messages are collected in data.frame inputwarnings; code will be used for interface
  updateDate <- format(file.info("server.R")$mtime, "%Y-%m-%d")
  inputwarnings <-
    data.frame(#making sure it exists; first line is empty
      code = "Versionnumber",
      warningText = paste("Version ", updateDate),
      stringsAsFactors = F)
  if (!("data.frame" %in% class(SSDbron))){
    SSDbron <- try(get(SSDbron))
    if (!("data.frame" %in% class(SSDbron))) {
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","De SSD gegevens missen als bestand")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","datasource for SSD's not found")
      }
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
  }
  # read inputfile and split grootheden and parameters #####
  if ("data.frame" %in% class(filename)) {
    inputData <- filename
  } else {
    if (endsWith(filename, ".xlsx")) {
      inputData <- openxlsx::read.xlsx(filename)
    } else {
      if (National == "Nederlands") {
        sep = ";"
        dec = ","
      } else {
        sep = ","
        dec = "."
      }
      inputData <- read.csv2(file = filename, sep = sep, dec = dec, stringsAsFactors = F)
    }
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
    #N.B. JS: added to code for easier comparison:
    "orig_rownr"
  )
  Eng2UsedTranslate <- list(
    Parameter.code = "Parameter.code",
    CAS = "Parameter.CASnummer",
    Unit = "Eenheid.code",
    H2OParameter = "Grootheid.code",
    Quantification = "Hoedanigheid.code",
    Limit = "Limietsymbool",
    SampleDate = "Begindatum",
    Location = "Meetobject.lokaalID",
    MeasuredValue = "Numeriekewaarde"
  )

  if (National != "Nederlands" && any(names(Eng2UsedTranslate) %in% names(inputData))) {
    #conform column names to Dutch IMformat
    #remember to pass the conformation
    conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)]
    #check minimal list of columns (CAS, Location, SampleDate, MeasuredValue)
    NeededColumns <- c("CAS", "Location", "SampleDate", "MeasuredValue", "Unit")
    if (!all(NeededColumns %in% names(inputData))){
      missingcolumns <- NeededColumns[!(NeededColumns %in% names(inputData))]
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("MissingColumns",warningText = do.call(paste,as.list(c("needed columns:", missingcolumns))))
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Language Setting", "The English setting requiers a , separator and a . decimal")
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
    names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)] <- Eng2UsedTranslate[conforms]
    # only to comply to IMformat
    inputData$Resultaatdatum <- inputData$Begindatum
    # minimally columns
    if (!"Parameter.code" %in% names(inputData)){
      inputData$Parameter.code <- ""
    }
    inputData$Parameter.code[is.na(inputData$Parameter.code)] <- ""

    if (!"Limietsymbool" %in% names(inputData)){
      inputData$Limietsymbool <- ""
    }
    if (!"Grootheid.code" %in% names(inputData)){
      inputData$Grootheid.code <- ""
    }
  } else conforms <- NA

  if(!all(UsedParameters %in% names(inputData))){
    missingcolumns <- UsedParameters[!(UsedParameters %in% names(inputData))]

    inputwarnings[1 + nrow(inputwarnings), ] <-
      c("Missende kolommen",warningText = do.call(paste,as.list(c("nodig zijn:", missingcolumns))))
    #      ifelse (National == "Nederlands",
    #      c("Missende kolommen",warningText = do.call(paste,as.list(c("nodig zijn:", missingcolumns)))),
    #      c("MissingColumns",warningText = do.call(paste,as.list(c("needed columns:", missingcolumns)))))
    if(National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Taal/Language","Het Nederlandse formal gebruikt ; als scheiding, en , als decimaal")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Taal/Language","The international format uses , as separator and . as decimal")
    }
    return(  list(
      inputData = data.frame(),
      DataSamples = data.frame(),
      inputwarnings = inputwarnings
    ))
  }
  #Alfanumeriekewaarde is optional; avoid errors if it's missing
  if (!"Alfanumeriekewaarde" %in% names(inputData)) {
    inputData$Alfanumeriekewaarde <- ""
  }
  #correct where Alfanumeriekewaarde contains "<" and or a numeric value & Numeriekewaarde is missing
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
  if (any(HasBT.STAlfa))
    inputwarnings[1 + nrow(inputwarnings), ] <-
    c("InconsistentLimiet", "< and/or > symbol in alfanum; missing as Limietsymbool")
  AlsoNumeric <- as.numeric(inputData$Alfanumeriekewaarde)
  if (any(!is.na(AlsoNumeric) & is.na(inputData$Numeriekewaarde)))
    inputwarnings[1 + nrow(inputwarnings), ] <-
    c("InconsistentAlfaNum", "value(s) in alfanum that seems missing as Numeriekewaarde")

  #test numeric
  if (!is.numeric(inputData$Numeriekewaarde)) {
    #column read as character; dutch decimal comma should be "."
    inputData$Numeriekewaarde <- gsub(",", ".",inputData$Numeriekewaarde)
    inputData$Numeriekewaarde <- as.numeric(inputData$Numeriekewaarde)
  }

  if(anyNA(inputData$Numeriekewaarde)){
    NumNA <- length(which(is.na(inputData$Numeriekewaarde)))
    inputData <- inputData[!is.na(inputData$Numeriekewaarde),]

    if(National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Niet-Numeriek",paste("Geen Numerieke waarde, aantal rijen overgeslagen:", NumNA))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NonNumeric",paste("Non Numeric Numeriekewaarde, rows deleted:", NumNA))
    }
  }

  #"Hoedanigheid.code" is optional; avoid errors if it's missing
  if (!"Hoedanigheid.code" %in% names(inputData)) {
    inputData$Hoedanigheid.code <- ""
  } else {
    inputData$Hoedanigheid.code <- ifelse(is.na(inputData$Hoedanigheid.code),
                                          "",
                                          as.character(inputData$Hoedanigheid.code))
  }

  inputData$Limietsymbool <- as.character(inputData$Limietsymbool)
  outLimit <- length(which(inputData$Limietsymbool %in% c("<", ">")))
  if (outLimit > 0) {
    if (verbose)
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Buiten limiet indicatie",
            paste(outLimit, "rijen verwijderd met Limietsymbool < or >"))
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Out of limit",
            paste(outLimit, "number of rows removed with Limietsymbool < or >"))
      }
    inputData <-
      inputData[!inputData$Limietsymbool %in% c("<", ">"), c(UsedParameters, OptionalParameters)]
  }

  grootheden <-
    unique(inputData[, c("Grootheid.code", "Parameter.code")])

  #MODgrootheden <- grootheden[grootheden$Grootheid.code %in% Modifyers$ModifMODname,]
  #pecularities: T = Tw; Corg = DOC (nf-na filtering);
  if ("T" %in% grootheden$Grootheid.code &
      (!"Tw" %in% grootheden$Grootheid.code)) {
    if (National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("AquoCode.Tw",
          "T gevonden, geen Tw; alle T is gelezen als watertemperature (Tw)")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("AquoCode.Tw",
          "T found but no Tw; assuming all T is temperature of water (Tw)")
    }
    inputData$Grootheid.code[inputData$Grootheid.code == "T"] <- "Tw"
  }
  if ("Corg" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "Corg" &
                               endsWith(tolower(inputData$Hoedanigheid.code), suffix = "nf")] <-
      "DOC"
    if (verbose)
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Corg2DOC",
            "Waarden van Corg, na filtering worden als DOC gebruikt")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Corg2DOC",
            "Corg values 'na filtering' are set as modifyer DOC")
      }
  }
  if ("OS" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "OS"] <- "TSS"
    if (verbose)
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("OS2TSS", "Waarden van OS worden als TSS gebruikt")
      }  else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("OS2TSS", "OS values are set as modifyer TSS")
      }
  }

  #yes, again, to make sure changes in inputData are in?
  #grootheden <- unique(inputData[,

  # date and create SampleID from location/date ##### optional in future function?
  inputData$THEdate <-
    tryCatch(
      #      as.Date(
      as.character(
        inputData$Begindatum
        #        ,tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%y", "%m-%d-%Y", "%m/%d/%Y")
      ),
      error = function(cond)
        return(NA),
      silent = T
    )
  inputData$THEdate[is.na(inputData$THEdate)] <-
    tryCatch(
      as.character(
        #    as.Date(
        inputData$Resultaatdatum[is.na(inputData$THEdate)]
        #      ,tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%y", "%m-%d-%Y", "%m/%d/%Y")
      ),
      error = function(cond)
        return(NA),
      silent = T
    )
  if (any(is.na(inputData$THEdate)))
    if (National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoDateData", "rijen zonder datum zijn verwijderd")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoDateData", "data with a missing date are removed")
    }
  inputData <- inputData[!is.na(inputData$THEdate), ]
  #back to string to prevent mis-interpretation
  inputData$THEdate <- format(inputData$THEdate)
  #setup Samples: unique date-locations
  ToSample <- inputData[, c("Meetobject.lokaalID", "THEdate")]
  Astmp <-
    paste0(ToSample$Meetobject.lokaalID,
           ":",ToSample$THEdate)
  inputData$SampleID <- unlist(Astmp) #

  #modifyers & substance matching
  MODgrootheden <-
    inputData[inputData$Grootheid.code %in% Modifyers$ModifMODname, ]
  MODIons <-
    inputData[inputData$Parameter.code %in% Modifyers$ModifMODname,]
  #remove from inputData, Ions as MODifying factors are not considered as toxicant!
  inputData <-
    inputData[!(
      inputData$Grootheid.code %in% Modifyers$ModifMODname |
        inputData$Parameter.code %in% Modifyers$ModifMODname
    ), ]

  # prep UnitConversions ##### (loaded in Globals.R)

  #add the no-conversion needed for vectorisation
  AddUnitConversions <- data.frame(
    Unit_in = unique(UnitConversions$Unit_out),
    Unit_out = unique(UnitConversions$Unit_out),
    Multiply_by = 1,
    Mul_molmass = F
  )
  AllUnitConversions <- rbind (UnitConversions, AddUnitConversions)

  #combine the defaults, MOD & MOD  #####
  DataSamples <- data.frame(SampleID = unique(inputData$SampleID),
                            stringsAsFactors = F)

  # unit conversions #####

  #next sections to separate function?
  #possibly values in inputData #loop modifying variables for grootheid and Ions separate
  for (VarName in Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODgrootheden$Grootheid.code]) {
    #VarName = Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODgrootheden$Grootheid.code][1]
    RightRows <-
      which(MODgrootheden$Grootheid.code == VarName, arr.ind = T) # VarName rows in MODgrootheden (relevant part of input)
    TranslateUnits <-
      AllUnitConversions[AllUnitConversions$Unit_out == Modifyers$MODnameUnit[match(VarName, Modifyers$ModifMODname)], ]
    GivenUnits <- MODgrootheden$Eenheid.code[RightRows]
    Factors <-
      TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ] #order in subset of MODgrootheden  = data source
    if (anyNA(Factors$Unit_in)) {
      UnknownUnits <- unique(GivenUnits)
      UnknownUnits <-
        UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
      #quote the units to make the message clear
      UnknownUnits <- paste("'", paste(UnknownUnits, sep = "','"), "'", sep = "")
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          do.call(paste, as.list(c(UnknownUnits, "Unit not found for", VarName))))
      Factors$Multiply_by[is.na(Factors$Unit_in)] <- 1
      Factors$Div_molmass[is.na(Factors$Unit_in)] <- F
    }
    MatchSample <-
      match(DataSamples$SampleID, MODgrootheden$SampleID[RightRows]) #order in DataSamples = data target
    DataSamples[,VarName] <- Factors$Multiply_by[MatchSample] *
      MODgrootheden$Numeriekewaarde[RightRows][MatchSample]
    #Molmass not checked
  }
  for (VarName in Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODIons$Parameter.code]) { #VarName = "Ca"
    #VarName = Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODIons$Parameter.code][5]
    RightRows <- which(MODIons$Parameter.code == VarName, arr.ind = T)
    #unit conversion for this VarName
    TranslateUnits <-
      AllUnitConversions[AllUnitConversions$Unit_out == Modifyers$MODnameUnit[match(VarName, Modifyers$ModifMODname)], ]
    GivenUnits <- MODIons$Eenheid.code[RightRows]
    Factors <-
      TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ]  #order in MODIons  = data source
    if (anyNA(Factors$Unit_in)) {
      UnknownUnits <- unique(GivenUnits)
      UnknownUnits <-
        UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
      #quote them, to make a clear message and a single string
      UnknownUnits <- paste("'",
                            do.call(paste,c(as.list(UnknownUnits), list(sep = "','"))),
                            "'")
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Unknown",
            paste(
              UnknownUnits,
              "Onbekende eenheid voor",
              VarName,
              ", NIET omgerekend"
            ))
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Unknown",
            paste(
              UnknownUnits,
              "Unit(s) not found for",
              VarName,
              "NOT converted"
            ))
      }
      #assuming unit is correct, append
      Factors$Multiply_by[is.na(Factors$Unit_in)] <- 1
      Factors$Div_molmass[is.na(Factors$Unit_in)] <- F
    }
    MatchSample <-
      match(DataSamples$SampleID, MODIons$SampleID[RightRows])  #order in DataSamples = data target
    DataSamples[,VarName] <- Factors$Multiply_by[MatchSample] *
      MODIons$Numeriekewaarde[RightRows][MatchSample]
    if (!is.na(Modifyers$MW.g.Mol[match(VarName, Modifyers$ModifMODname)])) {
      #possibly multiply by molar mass [g/mol]
      toMul <- which(Factors$Mul_molmass[MatchSample])
      if (length(toMul) > 0) {
        DataSamples[toMul, VarName] <- DataSamples[toMul, VarName] *
          Modifyers$MW.g.Mol[match(VarName, Modifyers$ModifMODname)]
      }
    }
  }

  #Units of substances and  hoedanigheid N/C  nf  NF and prepare for msPAF#####
  #match substances; register with CAS but not matched

  #force valid CAS, but keep the original CAS code
  #NB the CAS variable will be the ultimate match to the SSD data
  MatchChar2CAS <- match(inputData$Parameter.code, OtherChar$ChemCode)
  MatchChar2CAS[is.na(MatchChar2CAS)] <- match(inputData$Parameter.CASnummer[is.na(MatchChar2CAS)], OtherChar$ChemCode)
  inputData$CAS <- OtherChar$CAS[MatchChar2CAS] #init. with OtherChar, if present. Might be NA, might be update by:
  UniqCas <- unique(inputData$Parameter.CASnummer)
  CheckCASvalidUniq <- ValidCAS(UniqCas)
  CheckCASvalid <- inputData$Parameter.CASnummer %in% UniqCas[CheckCASvalidUniq]
  inputData$CAS[CheckCASvalid & is.na(MatchChar2CAS)] <- inputData$Parameter.CASnummer[CheckCASvalid& is.na(MatchChar2CAS)]
  unused <-
    inputData[is.na(inputData$CAS), c("Parameter.code", "Parameter.CASnummer")]
  NonSSDbronCAS <- unique(inputData$CAS[!is.na(inputData$CAS) & !inputData$CAS %in% SSDbron$CAS])
  if (length(NonSSDbronCAS) > 0) {
    CASnotChemlist <- do.call(paste,as.list(unique(NonSSDbronCAS)))
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NAmatchedCode",
          paste (nrow(NonSSDbronCAS), paste("stoffen komen niet voor in de stoffenlijst", CASnotChemlist)))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NAmatchedCode",
          paste (nrow(NonSSDbronCAS), paste("substances with CAS not in chemistry list", CASnotChemlist)))
    }
  }

  #remove all non substance measurements
  inputData <- inputData[!is.na(inputData$CAS), ]
  inputData <- inputData[!inputData$CAS %in% NonSSDbronCAS, ]

  #unit conversion for the toxic substances
  TranslateUnits <-
    AllUnitConversions[AllUnitConversions$Unit_out == "ug/l", ] #the unit in SSD's
  GivenUnits <- inputData$Eenheid.code
  Factors <-
    TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ]
  if (anyNA(Factors$Unit_in)) {
    UnknownUnits <- unique(GivenUnits)
    UnknownUnits <-
      UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
    UnknownUnits <- paste("'",
                          do.call(paste,c(as.list(UnknownUnits), list(sep = "','"))),
                          "'")
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          paste(
            UnknownUnits,
            "onbekende concentratie eenheid; De rijen zijn verwijderd"
          ))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          paste(
            UnknownUnits,
            " unknown concentration unit(s); row(s) deleted"
          ))
    }
    inputData <- inputData[!is.na(Factors$Unit_in),]
    Factors <- Factors[!is.na(Factors$Unit_in),]
  }

  #Match to SSD data source
  MatchChem <- match(inputData$CAS, SSDbron$CAS)
  inputData$Concentration <- inputData$Numeriekewaarde * Factors$Multiply_by
  toMul <- which(Factors$Mul_molmass)
  if (length(toMul) > 0) {
    inputData$Concentration[toMul] <- inputData$Concentration[toMul] *
      SSDbron[MatchChem[toMul],MolMassName]
  }
  inputData$NaFiltering <- endsWith(inputData$Hoedanigheid.code, "nf")
  AsN <- which(inputData$Hoedanigheid.code %in% c("N", "Nnf")) # correct to molmass of whole molecule
  if (length(AsN)>0) {
    ATOMMASSN <- 14.0067
    inputData$Concentration[AsN] <- inputData$Concentration[AsN] / ATOMMASSN * SSDbron[MatchChem[AsN],MolMassName]
  }
  AsP <- which(inputData$Hoedanigheid.code %in% c("P", "Pnf")) # correct to molmass of whole molecule
  if (length(AsP)>0) {
    ATOMMASSP <- 30.973762
    inputData$Concentration[AsP] <- inputData$Concentration[AsP] / ATOMMASSP * SSDbron[MatchChem[AsP],MolMassName]
  }

  #with defaults #from the one row in ModifierDefaults
  if ("TSS" %in% names(DataSamples))
    DataSamples$TSS[is.na(DataSamples$TSS)] <-
    ModifierDefaults$`TSS(mg/L)`[1] else #new column
      DataSamples$TSS <- ModifierDefaults$`TSS(mg/L)`[1]
  if ("POC" %in% names(DataSamples))
    DataSamples$POC[is.na(DataSamples$POC)] <-
    ModifierDefaults$`POC(mg/kg)`[1] else #new column
      DataSamples$POC <- ModifierDefaults$`POC(mg/kg)`[1]
  if ("DOC" %in% names(DataSamples))
    DataSamples$DOC[is.na(DataSamples$DOC)] <-
    ModifierDefaults$`DOC(mg/L)`[1] else #new column
      DataSamples$DOC <- ModifierDefaults$`DOC(mg/L)`[1]
  if ("pH" %in% names(DataSamples))
    DataSamples$pH[is.na(DataSamples$pH)] <-
    ModifierDefaults$`pH(units)`[1] else #new column
      DataSamples$pH <- ModifierDefaults$`pH(units)`[1]
  if ("Tw" %in% names(DataSamples))
    DataSamples$Tw[is.na(DataSamples$Tw)] <-
    ModifierDefaults$`Tw(oC)`[1] else #new column
      DataSamples$Tw <- ModifierDefaults$`Tw(oC)`[1]
  if ("Ca" %in% names(DataSamples))
    DataSamples$Ca[is.na(DataSamples$Ca)] <-
    ModifierDefaults$`Ca(ug/L)`[1] else #new column
      DataSamples$Ca <- ModifierDefaults$`Ca(ug/L)`[1]
  if ("Mg" %in% names(DataSamples))
    DataSamples$Mg[is.na(DataSamples$Mg)] <-
    ModifierDefaults$`Mg(ug/L)`[1] else #new column
      DataSamples$Mg <- ModifierDefaults$`Mg(ug/L)`[1]
  if ("Na" %in% names(DataSamples))
    DataSamples$Na[is.na(DataSamples$Na)] <-
    ModifierDefaults$`Na(ug/L)`[1] else #new column
      DataSamples$Na <- ModifierDefaults$`Na(ug/L)`[1]
  if ("Cl" %in% names(DataSamples))
    DataSamples$Cl[is.na(DataSamples$Cl)] <-
    ModifierDefaults$`Cl(ug/L)`[1] else #new column
      DataSamples$Cl <- ModifierDefaults$`Cl(ug/L)`[1]

  #for easier name handling in package
  inputData$ChemCode <- inputData$Parameter.code
  if ("MeasuredValue" %in% names(inputData)) {
    inputData$MeasuredValue <- NULL #free name for Concentration column
  }
  names(inputData)[names(inputData)=="Concentration"] <- "MeasuredValue"
  inputData$PreTreatment <- ifelse(inputData$NaFiltering,"nf","")
  attr(inputData, "conforms") <- conforms

  #return
  list(
    inputData = inputData,
    DataSamples = DataSamples,
    inputwarnings = inputwarnings
  )
}


# https://github.com/rivm-syso/msPAFcalculator/blob/main/R/ValidCAS.R
ValidCAS <- function(CAScode, checkDash = T){
  CAScode <- sapply(CAScode, trimws)

  hasCASstring <- grepl("CAS", CAScode)
  onlynumordash <- !grepl("[a-z,A-Z]", CAScode)
  hasCASpattern <- grepl("\\d+-\\d\\d-\\d", CAScode)

  #if non number characters are present it should contain "CAS"
  poss.CAS <- onlynumordash | hasCASstring

  if (checkDash) {#accept CAS code only with the -dd-d pattern;
    poss.CAS <- poss.CAS & hasCASpattern
  }
  #only the numbers for the !not.CAS
  CascodeNodash <- stringr::str_extract(gsub("-", "", CAScode[poss.CAS]), "[[:digit:]]+")

  CasCodeSingleChar <- sapply(CascodeNodash, strsplit, "")
  poss.CAS[poss.CAS] <- sapply(CasCodeSingleChar, function(x) {
    length(x) > 4
  })

  if (!any(poss.CAS)) return(poss.CAS) #prevent error and faster


  CasCodeLastNumber <- sapply(CasCodeSingleChar, function(x){
    as.numeric(x[length(x)])
  })
  CasCodeChecksum <- sapply(CasCodeSingleChar, function(x){
    revx <- rev(x)
    sum(sapply(1:(length(x)-1), function(y)
      y*as.numeric(revx[y+1])))
  })

  #combine the poss.CAS and checksummed
  poss.CAS[poss.CAS] <- (CasCodeChecksum %% 10) == CasCodeLastNumber
  poss.CAS
}

output_mspafcalculator <- leesIMformat_app(
  filename = copy_inputData,
  SSDbron = "Gross", National = "Nederlands"
)

compare_IMs <- CompareIMdataframes(output_mspaf$inputData, output_mspafcalculator$inputData)
write.csv(compare_IMs$diff_1_not_2, file = "/rivm/s/slootwej/tmp/compare_IMs12.csv")
write.csv(compare_IMs$diff_2_not_1, file = "/rivm/s/slootwej/tmp/compare_IMs21.csv")
write.csv(compare_IMs$combined, file = "/rivm/s/slootwej/tmp/compare_IMs_combined.csv")
##############################################################################################
#Test the workflow############################################################################
##############################################################################################

rm(list=ls())
devtools::load_all()
library(tidyverse)

# Input file
Gross <- SSDplusListRDS$Gross2023

# Variables of the function
filename = "./data/msPAFtoolExampleNL.xlsx"
SSDbron = "Gross"
MolMassName = "MW.g.Mol"
National = "Nederlands"
verbose = T

data <- leesIMformat(filename, SSDbron, MolMassName, National, verbose)
data2 <- HU_Calc2(data$inputData, ChemData = Gross)
top95 <- function(x) quantile(x, probs = 0.95) # 95th percentile
data2_95 <- HU_Calc2(data$inputData, ChemData = Gross, aggrFUN = top95)
data2_75 <- HU_Calc2(data$inputData, ChemData = Gross, aggrFUN = function(x) quantile(x, probs = 0.75))
data2_med <- HU_Calc2(data$inputData, ChemData = Gross, aggrFUN = median)
data3 <- HU2msPAFs(data2$PAF)

#tmp <- data$inputData
#tmp$CAS <- tmp$ChemCode
#tmp$MeasuredValue <- tmp$Concentration
#data4 <- HU_Calc2_old(tmp, ChemData = Gross)
#data5 <- HU2msPAFs_old(data4)




rm(list=ls())
devtools::load_all()
library(tidyverse)

# Input file
Gross <- SSDplusListRDS$Gross2023

# Variables of the function
filename = "./data/msPAFtoolExampleInternational.xlsx"
SSDbron = "Gross"
MolMassName = "MW.g.Mol"
National = "English"
verbose = T

data <- leesIMformat(filename, SSDbron, MolMassName, National, verbose)
data2 <- HU_Calc2(data$inputData, ChemData = Gross)
data3 <- HU2msPAFs(data2$PAF)
##############################################################################################
#Test the workflow with seperate aggregate function###########################################
##############################################################################################

rm(list=ls())
devtools::load_all()
HU_Calc2_old <- function (ToHU, ChemData = Gross, ChemReplace = NULL, EnvData = NULL,
                          muNames = c(acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"),
                          sigmaNames = c(acute = "Acute2.0Dev10LogMassTox.ug.L", chronic = "Chronic2.0Dev10LogMassTox.ug.L"),
                          DefaultFiltered = T, aggrFUN = max, DevRange = c(0.2,2),
                          status_bioavailability=TRUE, TooLowLimit = 0.0001){

  Awarning <- list()
  excluded_rows <- data.frame()

  if (!all(c(muNames,sigmaNames) %in% names(ChemData))) {
    Awarning[["muSigmaMissing"]] <- paste("Missing mu and/or sigma;", muNames, sigmaNames)
    emptyPAF <- data.frame()
    emptyPAF$ExclusionReason <- NA_character_
    attr(emptyPAF, "warning") <- Awarning
    return(list(PAF = emptyPAF, excluded_rows = data.frame(), warning = Awarning))
  }
  # Rename columns to match expected names if they come from leesIMformat
  # ChemCode now contains the mapped CAS/identifier (from check_substance_codes)
  if ("ChemCode" %in% names(ToHU) && !"CAS" %in% names(ToHU)) {
    ToHU$CAS <- ToHU$ChemCode
  }
  # Concentration is the calculated concentration value in ug/L
  if ("Concentration" %in% names(ToHU) && !"MeasuredValue" %in% names(ToHU)) {
    ToHU$MeasuredValue <- ToHU$Concentration
  }

  needDataColumns <- c("PreTreatment","CAS", "SampleID", "ChemCode", "MeasuredValue")
  if(!all(needDataColumns %in% names(ToHU))){
    missingColumns <- needDataColumns [!needDataColumns %in% names(ToHU)]
    Awarning[["missingDataColumn"]] <- paste("Missing data column;", missingColumns)
    ToHU$ExclusionReason <- "Missing required data column"
    excluded_rows <- dplyr::bind_rows(excluded_rows, ToHU)
    emptyPAF <- data.frame()
    return(list(PAF = emptyPAF, excluded_rows = excluded_rows, warning = Awarning))
  }


  if ("PreTreatment" %in% names(ToHU)) {
    ToHU$Filtered <- endsWith(tolower(ToHU$PreTreatment), "nf")   #na filtratie
  } else {
    ToHU$Filtered <- DefaultFiltered
  }

  #if Sample as filtered AND not filtered tuples for otherwise same sample & substance: take filtered
  #by first aggregate for SampleID, substance, Filtered and take the aggrFUN (usually max) then
  #aggregate for SampleID, substance and then the minimum;
  #basically: the filtered sample of the maximum concentration
  aggrFUN_name <- deparse(substitute(aggrFUN))
  aggToHU <- aggregate(MeasuredValue~CAS+SampleID+ChemCode+Filtered , data = ToHU, FUN = aggrFUN)
  if (nrow(aggToHU) < nrow(ToHU)) {
    Awarning[["multiMeasurements"]] <- paste("Substance has multiple samples; one taken (", aggrFUN_name, ")")
    # Track which rows were removed during aggregation
    # Create a composite key to identify which rows were kept
    agg_excluded_idx <- vector("logical", nrow(ToHU))
    for (i in seq_len(nrow(aggToHU))) {
      # Find rows in ToHU that match this aggregated row
      match_idx <- which(ToHU$CAS == aggToHU$CAS[i] &
                           ToHU$SampleID == aggToHU$SampleID[i] &
                           ToHU$ChemCode == aggToHU$ChemCode[i] &
                           ToHU$Filtered == aggToHU$Filtered[i])
      if (length(match_idx) > 1) {
        # Mark all but the first (which was kept by aggregate/max) as excluded
        agg_excluded_idx[match_idx[-1]] <- TRUE
      }
    }

    if (any(agg_excluded_idx)) {
      agg_excluded <- ToHU[agg_excluded_idx, ]
      agg_excluded$ExclusionReason <- paste("Multiple measurements aggregated (kept", aggrFUN_name, ")")
      excluded_rows <- dplyr::bind_rows(excluded_rows, agg_excluded)
      print(paste0(sum(agg_excluded_idx), " row(s) excluded during aggregation (multiple measurements per CAS/SampleID/ChemCode/Filtered combination)"))
    }
  }

  order_var <- with(aggToHU, order(SampleID, CAS, ChemCode, MeasuredValue))
  keep_rows <- logical(nrow(aggToHU)) #init booleans to keep, we keep the first one
  keep_rows[order_var[1]] <- TRUE
  current_group <- aggToHU[order_var[1], c("SampleID", "CAS", "ChemCode")]
  for (i in 2:length(order_var)) {
    # If the current group is not the same as the previous group
    if (any(aggToHU[order_var[i], c("SampleID", "CAS", "ChemCode")] != current_group)) {
      # Update the current group
      current_group <- aggToHU[order_var[i], c("SampleID", "CAS", "ChemCode")]
      # Mark the row to be kept
      keep_rows[order_var[i]] <- TRUE
    }
  }
  deletedRows <- length(order_var) - sum(keep_rows)
  if (deletedRows > 0){
    Awarning[["nonFiltered"]] <- "filtered a Non-Filtered sample, because filtered is also in"
    deleted_idx <- which(!keep_rows)
    deleted_data <- aggToHU[deleted_idx, ]
    deleted_data$ExclusionReason <- "Non-filtered sample excluded (filtered version exists)"
    excluded_rows <- dplyr::bind_rows(excluded_rows, deleted_data)
  }
  PAF <- aggToHU[keep_rows,] #outside of if; init of PAF

  if (!is.null(ChemReplace)) { #update SSD for chemicals from Chemreplace with "leenSSD's"
    #this (in the default version) includes NH4 - alikes where NH3 is the toxicant; see also the recalculation of concentration for NH4-alikes
    stopifnot ("CAS" %in% names(ChemReplace),
               "CASReplace" %in% names(ChemReplace))
    ToChange <- match(ChemReplace$CAS, ChemData$CAS)
    MatchToChange <- match(ChemReplace$CASReplace, ChemData$CAS)
    ChemData[ToChange,muNames] <- ChemData[MatchToChange, muNames]
    ChemData[ToChange,sigmaNames] <- ChemData[MatchToChange, sigmaNames]
  }

  ChemMatch <- match(PAF$CAS, ChemData$CAS)
  if (anyNA(ChemMatch)){
    MissingChem <- as.list(unique(PAF$CAS[is.na(ChemMatch)]))
    Awarning[["NoSSD"]] <- paste("No qualified SSD for", do.call(paste,MissingChem))
    excluded_noSSD <- PAF[is.na(ChemMatch), ]
    excluded_noSSD$ExclusionReason <- "No qualified SSD for chemical"
    excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_noSSD)
    PAF <- PAF[!is.na(ChemMatch),]
    ChemMatch <- ChemMatch[!is.na(ChemMatch)]
  }
  #All all needed chemical data
  PAF$MOI <- ChemData$M.O.I[ChemMatch]
  PAF$KocNew <- ChemData$KocNew[ChemMatch]
  PAF$KocNew <- ifelse(is.na(PAF$KocNew), ChemData$logKpSuspendedMatter[ChemMatch], PAF$KocNew) #Add ED 3-1-22
  PAF$PrimaryMoA <- ChemData$PrimaryMoA[ChemMatch]
  PAF$Avg10Log_acute <- ChemData[ChemMatch,muNames["acute"]]
  PAF$Dev10Log_acute <- ChemData[ChemMatch,sigmaNames["acute"]]
  PAF$Dev10Log_acute[PAF$Dev10Log_acute < DevRange[1] | PAF$Dev10Log_acute > DevRange[2]] <- 0.7
  PAF$Avg10Log_chronic <- ChemData[ChemMatch,muNames["chronic"]]
  PAF$Dev10Log_chronic <- ChemData[ChemMatch,sigmaNames["chronic"]]
  PAF$Dev10Log_chronic[PAF$Dev10Log_chronic < DevRange[1] | PAF$Dev10Log_chronic > DevRange[2]] <- 0.7
  PAF$UseClass <- ChemData$UseClass[ChemMatch]
  PAF$groep.fotoNL <- ChemData$groep.fotoNL[ChemMatch]


  # Use Default modifiers if none are specified -----------------------------
  if (is.null(EnvData)) EnvData <- ModifierDefaults
  if (nrow(EnvData)==1){
    names(EnvData) <- sub("\\(.+\\)", "", names(EnvData))
    SampleMatch <- rep(1,nrow(PAF))
  } else {
    if (nrow(PAF) == nrow(EnvData)) {
      SampleMatch <- 1:nrow(EnvData)
    } else {
      stopifnot("SampleID" %in% names(EnvData) & "SampleID" %in% names(PAF))
      SampleMatch <- match(PAF$SampleID, EnvData$SampleID)
    }
  }
  PAF$TSS <- EnvData$TSS[SampleMatch]

  #save for NHx, Inorganic, all dissolved, all bioav.
  PAF$DissConc <- PAF$MeasuredValue
  iToHUisNH4 <- which(PAF$ChemCode == "sNH3NH4") #from specific dutch aquocode; sum of NH3 and NH4
  #NH4 / sNH3NH4 are considered the actual measured NH4 concentration, possibly replacing the calculated C[NH3]
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    PAF$DissConc[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * PAF$MeasuredValue[iToHUisNH4]
    #remove others in samples
    iToDel <- which(PAF$SampleID %in% PAF$SampleID[iToHUisNH4] & PAF$ChemCode %in% c("NH3", "NH4"))
    if (length(iToDel) > 0 ){
      excluded_NH4_a <- PAF[iToDel, ]
      excluded_NH4_a$ExclusionReason <- "NH3/NH4 duplicate removed (sNH3NH4 exists)"
      excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_NH4_a)
      PAF <- PAF[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(PAF$CAS, ChemData$CAS)
    }
  }
  iToHUisNH4 <- which(PAF$CAS == "14798-03-9") # == NH4
  # replaced by toxicant NH3, mind the "LeenSSD" for this!
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    PAF$DissConc[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * PAF$MeasuredValue[iToHUisNH4]
    #remove others in samples
    iToDel <- which(PAF$SampleID %in% PAF$SampleID[iToHUisNH4] & PAF$CAS == "7664-41-7") # == NH3
    if (length(iToDel) > 0 ){
      excluded_NH4_b <- PAF[iToDel, ]
      excluded_NH4_b$ExclusionReason <- "NH3 removed (NH4 is the measured value)"
      excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_NH4_b)
      PAF <- PAF[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(PAF$CAS, ChemData$CAS)
    }
  }


  # Correction for bioavailability of metals/OC -----------------------------

  #metals / Organics differ
  metals <- !is.na(PAF$MOI) & PAF$MOI == "M"
  SpecialM <- !is.na(ChemData$CoefLogMeTot[ChemMatch]) & ChemData$CoefLogMeTot[ChemMatch] != 0
  #stopifnot(!any(is.na(SpecialM)))
  organic <- !is.na(PAF$MOI) & PAF$MOI == "O"

  #Correction of metal concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    PAF$DissConc[metals] <- ifelse(PAF$Filtered[metals], PAF$MeasuredValue[metals],
                                   PAF$MeasuredValue[metals] / (1 + PAF$TSS[metals] * 10 ^ -6 * 10 ^ PAF$KocNew[metals])
    )
  }

  #for Inorganic & metals, excluding SpecialM (overwritten in the next line)
  PAF$ActConc <- PAF$DissConc

  #Correction of special metal concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    PAF$ActConc[SpecialM] <- 10 ^ (
      ChemData$Intercept[ChemMatch[SpecialM]] +
        ChemData$CoefLogMeTot[ChemMatch[SpecialM]] * log10(PAF$DissConc[SpecialM]) +
        ChemData$CoefLogDOC[ChemMatch[SpecialM]] * log10(EnvData$DOC[SampleMatch[SpecialM]]) +
        ChemData$CoefDOC[ChemMatch[SpecialM]] * EnvData$DOC[SampleMatch[SpecialM]] +
        ChemData$CoefCa[ChemMatch[SpecialM]] * EnvData$Ca[SampleMatch[SpecialM]] +
        ChemData$CoefLogCa[ChemMatch[SpecialM]] * log10(EnvData$Ca[SampleMatch[SpecialM]]) +
        ChemData$CoefMg[ChemMatch[SpecialM]] * EnvData$Mg[SampleMatch[SpecialM]] +
        ChemData$CoefNa[ChemMatch[SpecialM]] * EnvData$Na[SampleMatch[SpecialM]] +
        ChemData$CoefLogCl[ChemMatch[SpecialM]] * log10(EnvData$Cl[SampleMatch[SpecialM]]) +
        ChemData$CoefpH[ChemMatch[SpecialM]] * EnvData$pH[SampleMatch[SpecialM]]
    )
  }

  #Correction of organic content concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    POC <- ModifierDefaults$`POC(mg/kg)`
    PAF$ActConc[organic] <- ifelse(PAF$Filtered[organic], PAF$MeasuredValue[organic],
                                   PAF$MeasuredValue[organic] /
                                     (1 + PAF$TSS[organic] * 10 ^ -6 * POC * 10 ^ -6 * 10 ^ PAF$KocNew[organic])
    )


  }

  if (nrow(PAF) == 0) {
    PAF$ExclusionReason <- NA_character_
    return(list(PAF = data.frame(), excluded_rows = excluded_rows, warning = Awarning))
  }

  #acute
  PAF$HU_acute <- PAF$ActConc / (10 ^ PAF$Avg10Log_acute)
  PAF$PAFacute <- pnorm(log10(PAF$HU_acute),
                        mean = 0,
                        sd = PAF$Dev10Log_acute)
  #Avoid NA
  PAF$HU_acute[is.na(PAF$HU_acute)] <- 0

  #chronic
  PAF$HU_Chronic <- PAF$ActConc / (10 ^ PAF$Avg10Log_chronic)
  PAF$PAFchronic <- pnorm(log10(PAF$HU_Chronic),
                          mean = 0,
                          sd = PAF$Dev10Log_chronic)

  #Avoid NA
  PAF$HU_Chronic[is.na(PAF$HU_Chronic)] <- 0

  #Extend with columns "TooLowAcute" and "TooLowChronic"
  TooLowText <- paste("<", TooLowLimit)
  PAF$TooLowAcute <- ifelse(PAF$HU_acute < TooLowLimit, TooLowText, "")
  PAF$TooLowChronic <- ifelse(PAF$HU_Chronic < TooLowLimit, TooLowText, "")

  #keep all relevant attributes
  AllNames <- c("CAS","ChemCode","UseClass",
                "Meetobject.lokaalID", "THEdatum",
                "MeasuredValue", "ActConc",
                "HU_acute", "HU_Chronic", "PAFacute", "PAFchronic",
                "groep.fotoNL","PrimaryMoA","SampleID",
                "TooLowAcute", "TooLowChronic",
                "Avg10Log_acute","Dev10Log_acute",
                "Avg10Log_chronic","Dev10Log_chronic")

  PAF <- PAF[,AllNames[AllNames %in% names(PAF)]]

  # Add ExclusionReason column as NA for all kept rows
  if(!("ExclusionReason" %in% names(PAF))){
    PAF$ExclusionReason <- NA_character_
  }

  return(list(PAF = PAF, excluded_rows = excluded_rows, warning = Awarning))
}
library(tidyverse)

# Input file
Gross <- SSDplusListRDS$Gross2023

# Variables of the function
filename = "./data/msPAFtoolExampleNL.xlsx"
SSDbron = "Gross"
MolMassName = "MW.g.Mol"
National = "Nederlands"
verbose = T

data <- leesIMformat(filename, SSDbron, MolMassName, National, verbose)
data2 <- HU_Calc2(data$inputData, ChemData = Gross)
data3 <- aggre_HU_Calc2(data2$PAF)

data5 <- HU2msPAFs(data3$PAF)





data4 <- HU_Calc2_old(data$inputData, ChemData = Gross)
data6 <- HU2msPAFs(data4$PAF)
library(testthat)
try(expect_equal(data6$acute, data5$acute))
try(expect_equal(data6$chronic, data5$chronic))
try(expect_equal(data6$class, data5$class))
try(expect_equal(nrow(data6$excluded_rows), nrow(data5$excluded_rows)))


#Test other aggregation

data <- leesIMformat(filename, SSDbron, MolMassName, National, verbose)
data2 <- HU_Calc2(data$inputData, ChemData = Gross)
data3 <- aggre_HU_Calc2(data2$PAF, aggrFUN = function(x) quantile(x, probs = 0.75))
data5 <- HU2msPAFs(data3$PAF)
data4 <- HU_Calc2_old(data$inputData, ChemData = Gross, aggrFUN = function(x) quantile(x, probs = 0.75))
data6 <- HU2msPAFs(data4$PAF)
library(testthat)
try(expect_equal(data3$PAF, data4$PAF))
try(expect_equal(data6$acute, data5$acute))
try(expect_equal(data6$chronic, data5$chronic))
try(expect_equal(data6$class, data5$class))
try(expect_equal(nrow(data6$excluded_rows), nrow(data5$excluded_rows)))





rm(list=ls())
devtools::load_all()
library(tidyverse)
# Input file
Gross <- SSDplusListRDS$Gross2023
# Variables of the function
filename = "./data/msPAFtoolExampleNL.xlsx"
SSDbron = "Gross"
MolMassName = "MW.g.Mol"
National = "Nederlands"
verbose = T
data <- leesIMformat(filename, SSDbron, MolMassName, National, verbose)
data2 <- HU_Calc2(data$inputData, ChemData = Gross)
data3 <- aggre_HU_Calc2(data2$PAF, aggrFUN = function(x) quantile(x, probs = 0.75))
data4 <- aggre_HU_Calc2(data2$PAF, aggrFUN = function(x) quantile(x, probs = 0.75), verbose = TRUE)
library(testthat)
try(expect_equal(data3$PAF, data4$PAF))
data3 <- aggre_HU_Calc2(data2$PAF)
data4 <- aggre_HU_Calc2(data2$PAF, verbose = TRUE)
try(expect_equal(data3$PAF, data4$PAF))
data5 <- HU2msPAFs(data4$PAF)
data6 <- HU2msPAFs(data3$PAF)
try(expect_equal(data5$PAF, data6$PAF))
try(expect_equal(data5$acute, data6$acute))

##############################################################################################
#https://github.com/rivm-syso/msPAFcalculator/pull/8/changes##################################
##############################################################################################
rm(list=ls())
devtools::load_all()
#library(tidyverse)
leesIMformat_old <- function(filename,
                         SSDbron = "Gross",
                         MolMassName = "MW.g.Mol",
                         National, verbose = T,
                         include_cas_reference = FALSE, app_columns_only = TRUE) {
  #1 preparations #####
  # read inputfile as input to new script and old function both #####
  if ("data.frame" %in% class(filename)) {
    inputData <- filename
  } else {
    if (endsWith(filename, ".xlsx")) {
      inputData <- openxlsx::read.xlsx(filename)
    } else {
      if (National == "Nederlands") {
        sep = ";"
        dec = ","
      } else {
        sep = ","
        dec = "."
      }
      inputData <- read.csv2(file = filename, sep = sep, dec = dec, stringsAsFactors = F)
    }
  }

  inputData$orig_rownr <- seq_len(nrow(inputData))

  # store for input to leesIMformat_app function below
  copy_inputData <- inputData

  # cleaning steps self

  #warnings/messages are collected in R6 object inputwarnings; see helpers.R
  updateDate <- format(file.info("server.R")$mtime, "%Y-%m-%d")

  inputwarnings <- InputWarnings$new(get_package_version_string("msPAF"))
  inputwarnings$add(updateDate, nl_text = "versie app", en_text = "app version", National = National)

  # in app as reactive (SSDbron (former Gross) might not change)
  if (!("data.frame" %in% class(SSDbron))){
    SSDbron <- try(get(SSDbron))
    if (!("data.frame" %in% class(SSDbron))) {
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","De SSD gegevens missen als bestand")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","datasource for SSD's not found")
      }
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
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
    "orig_rownr"
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
        inputwarnings = inputwarnings$data
      ))
    }

    # Use Eng2UsedTranslate to rename to Dutch if needed
    if (National != "Nederlands" && any(names(Eng2UsedTranslate$mapping) %in% names(inputData))) {
      inputData <- Eng2UsedTranslate$to_dutch(inputData)
    }

    # Only to comply with IMformat
    inputData$Resultaatdatum <- inputData$Begindatum

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


  # Verify possible detection/quantification limit - by the < character
  filter_result <- filter_limietsymbool(inputData, inputwarnings, National, verbose, UsedParameters, OptionalParameters)
  inputData <- filter_result$inputData
  excluded_rows <- dplyr::bind_rows(excluded_rows, filter_result$excluded_rows)

  #'   \item{T}{If Grootheid.code contains \code{"T"} but not \code{"Tw"}, all \code{"T"} are interpreted as water temperature (\code{"Tw"}).}
  #'   \item{Corg}{If Parameter.code is \code{"Corg"} and Hoedanigheid.code ends with \code{"nf"}, these are recoded to \code{"DOC"} (Dissolved Organic Carbon) after filtration.}
  #'   \item{OS}{If Parameter.code is \code{"OS"}, these are recoded to \code{"TSS"} (Total Suspended Solids).}
  inputData <- handle_grootheden_peculiarities(inputData, inputwarnings, National, verbose)

  # find a date and combine the location and date into a Sample ID
  inputData <- ensure_thedate_and_sampleid(inputData, inputwarnings, National)

  # # Some CAS users omit the - - (thus avoiding excel CAS - date messing) make it consistent
  cas_digits <- gsub("[^0-9]", "", inputData$Parameter.CASnummer)
  inputData$Parameter.CASnummer <- sub("^(\\d+)(\\d{2})(\\d)$", "\\1-\\2-\\3", cas_digits)

  #modifyers & substance matching
  ions <- c("Ca","Mg","Na","Cl")
  grootheden <- Modifyers$ModifMODname[!Modifyers$ModifMODname %in% ions]
  MODgrootheden <-
    inputData[inputData$Grootheid.code %in% Modifyers$ModifMODname,
              c("SampleID", "Grootheid.code", "Eenheid.code", "Numeriekewaarde")]
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
      inputData$Grootheid.code %in% Modifyers$ModifMODname | #never mind the ions...
        inputData$Parameter.code %in% ions
    ), ]


  # apply unitconversions to MODIons
  modions_result <- convert_units_and_calculate_concentration(MODIons, SSDbron, UnitConversions,
                                                              inputwarnings, National, MolMassName)
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
  inputData$ChemCode <- inputData$Parameter.code
  if ("MeasuredValue" %in% names(inputData)) {
    inputData$MeasuredValue <- NULL #free name for Concentration column
  }
  names(inputData)[names(inputData)=="Concentration"] <- "MeasuredValue"

  # "na filtering" can be combined with other "hoedanigheid"
  inputData$PreTreatment <- ifelse(endsWith(tolower(inputData$Hoedanigheid.code), "nf"), "nf", "")
  #TODO also for international?

  inputData <- clean_filtering_pairs(inputData, "PreTreatment", group_cols = "SampleID", National = National, inputwarnings = inputwarnings)

  conv_result <- convert_units_and_calculate_concentration(inputData, SSDbron, UnitConversions,
                                                           inputwarnings, National, MolMassName)
  inputData <- conv_result$inputData
  excluded_rows <- dplyr::bind_rows(excluded_rows, conv_result$excluded_rows)


  conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)] #TODO ??
  attr(inputData, "conforms") <- conforms

  subcheck_result <- check_substance_codes(inputData, OtherChar,
                                           SSDbron = SSDbron,
                                           inputwarnings = inputwarnings,
                                           National = National)
  inputData <- subcheck_result$inputData
  excluded_rows <- dplyr::bind_rows(excluded_rows, subcheck_result$excluded_rows)


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
    inputwarnings = inputwarnings,
    excludedData = excluded_rows
  )

  return(output_mspaf)
}

leesIMformat_multi <- function(filename,
                         SSDbron = "Gross",
                         gross_name = "Gross",
                         MolMassName = "MW.g.Mol",
                         National, verbose = T) {
  #1 preparations #####
  #warnings/messages are collected in data.frame inputwarnings; code will be used for interface
  updateDate <- format(file.info("server.R")$mtime, "%Y-%m-%d")
  inputwarnings <-
    data.frame(#making sure it exists; first line is empty
      code = "Versionnumber",
      #gross_version = as.character(gross_name)[1],
      warningText = paste("Version ", updateDate),
      stringsAsFactors = F)

  # Add the gross version in the output
  inputwarnings[1 + nrow(inputwarnings), ] <- c("GrossVersion", as.character(gross_name)[1])
  # Add the R shiny app version in the output:
  inputwarnings[1 + nrow(inputwarnings), ] <- c("AppVersion", getAppVersion())
  # Add the Git head in the output:
  inputwarnings[1 + nrow(inputwarnings), ] <- c("GitHead",git_head)

  if (!("data.frame" %in% class(SSDbron))){
    SSDbron <- try(get(SSDbron))
    if (!("data.frame" %in% class(SSDbron))) {
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","De SSD gegevens missen als bestand")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","datasource for SSD's not found")
      }
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
  }

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
    "Hoedanigheid.code"
  )
  Eng2UsedTranslate <- list(
    Parameter.code = "Parameter.code",
    CAS = "Parameter.CASnummer",
    Unit = "Eenheid.code",
    H2OParameter = "Grootheid.code",
    Quantification = "Hoedanigheid.code",
    Limit = "Limietsymbool",
    SampleDate = "Begindatum",
    Location = "Meetobject.lokaalID",
    MeasuredValue = "Numeriekewaarde"
  )

  if (National != "Nederlands" && any(names(Eng2UsedTranslate) %in% names(inputData))) {
    #conform column names to Dutch IMformat
    #remember to pass the conformation
    conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)]
    #check minimal list of columns (CAS, Location, SampleDate, MeasuredValue)
    NeededColumns <- c("CAS", "Location", "SampleDate", "MeasuredValue", "Unit")
    if (!all(NeededColumns %in% names(inputData))){
      missingcolumns <- NeededColumns[!(NeededColumns %in% names(inputData))]
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("MissingColumns",warningText = do.call(paste,as.list(c("needed columns:", missingcolumns))))
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Language Setting", "The English setting requiers a , separator and a . decimal")
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
    names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)] <- Eng2UsedTranslate[conforms]
    # only to comply to IMformat
    inputData$Resultaatdatum <- inputData$Begindatum
    # minimally columns
    if (!"Parameter.code" %in% names(inputData)){
      inputData$Parameter.code <- ""
    }
    inputData$Parameter.code[is.na(inputData$Parameter.code)] <- ""

    if (!"Limietsymbool" %in% names(inputData)){
      inputData$Limietsymbool <- ""
    }
    if (!"Grootheid.code" %in% names(inputData)){
      inputData$Grootheid.code <- ""
    }
  } else conforms <- NA

  if(!all(UsedParameters %in% names(inputData))){
    missingcolumns <- UsedParameters[!(UsedParameters %in% names(inputData))]

    inputwarnings[1 + nrow(inputwarnings), ] <-
      c("Missende kolommen",warningText = do.call(paste,as.list(c("nodig zijn:", missingcolumns))))
    #      ifelse (National == "Nederlands",
    #      c("Missende kolommen",warningText = do.call(paste,as.list(c("nodig zijn:", missingcolumns)))),
    #      c("MissingColumns",warningText = do.call(paste,as.list(c("needed columns:", missingcolumns)))))
    if(National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Taal/Language","Het Nederlandse formal gebruikt ; als scheiding, en , als decimaal")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Taal/Language","The international format uses , as separator and . as decimal")
    }
    return(  list(
      inputData = data.frame(),
      DataSamples = data.frame(),
      inputwarnings = inputwarnings
    ))
  }
  #Alfanumeriekewaarde is optional; avoid errors if it's missing
  if (!"Alfanumeriekewaarde" %in% names(inputData)) {
    inputData$Alfanumeriekewaarde <- ""
  }
  #correct where Alfanumeriekewaarde contains "<" and or a numeric value & Numeriekewaarde is missing
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
  if (any(HasBT.STAlfa))
    inputwarnings[1 + nrow(inputwarnings), ] <-
    c("InconsistentLimiet", "< and/or > symbol in alfanum; missing as Limietsymbool")
  AlsoNumeric <- as.numeric(inputData$Alfanumeriekewaarde)
  if (any(!is.na(AlsoNumeric) & is.na(inputData$Numeriekewaarde)))
    inputwarnings[1 + nrow(inputwarnings), ] <-
    c("InconsistentAlfaNum", "value(s) in alfanum that seems missing as Numeriekewaarde")

  #test numeric
  if (!is.numeric(inputData$Numeriekewaarde)) {
    #column read as character; dutch decimal comma should be "."
    inputData$Numeriekewaarde <- gsub(",", ".",inputData$Numeriekewaarde)
    inputData$Numeriekewaarde <- as.numeric(inputData$Numeriekewaarde)
  }

  if(anyNA(inputData$Numeriekewaarde)){
    NumNA <- length(which(is.na(inputData$Numeriekewaarde)))
    inputData <- inputData[!is.na(inputData$Numeriekewaarde),]

    if(National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Niet-Numeriek",paste("Geen Numerieke waarde, aantal rijen overgeslagen:", NumNA))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NonNumeric",paste("Non Numeric Numeriekewaarde, rows deleted:", NumNA))
    }
  }

  #"Hoedanigheid.code" is optional; avoid errors if it's missing
  if (!"Hoedanigheid.code" %in% names(inputData)) {
    inputData$Hoedanigheid.code <- ""
  } else {
    inputData$Hoedanigheid.code <- ifelse(is.na(inputData$Hoedanigheid.code),
                                          "",
                                          as.character(inputData$Hoedanigheid.code))
  }

  inputData$Limietsymbool <- as.character(inputData$Limietsymbool)
  outLimit <- length(which(inputData$Limietsymbool %in% c("<", ">")))
  if (outLimit > 0) {
    if (verbose)
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Buiten limiet indicatie",
            paste(outLimit, "rijen verwijderd met Limietsymbool < or >"))
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Out of limit",
            paste(outLimit, "number of rows removed with Limietsymbool < or >"))
      }
    inputData <-
      inputData[!inputData$Limietsymbool %in% c("<", ">"), c(UsedParameters, OptionalParameters)]
  }

  grootheden <-
    unique(inputData[, c("Grootheid.code", "Parameter.code")])

  #MODgrootheden <- grootheden[grootheden$Grootheid.code %in% Modifyers$ModifMODname,]
  #pecularities: T = Tw; Corg = DOC (nf-na filtering);
  if ("T" %in% grootheden$Grootheid.code &
      (!"Tw" %in% grootheden$Grootheid.code)) {
    if (National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("AquoCode.Tw",
          "T gevonden, geen Tw; alle T is gelezen als watertemperature (Tw)")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("AquoCode.Tw",
          "T found but no Tw; assuming all T is temperature of water (Tw)")
    }
    inputData$Grootheid.code[inputData$Grootheid.code == "T"] <- "Tw"
  }
  if ("Corg" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "Corg" &
                               endsWith(tolower(inputData$Hoedanigheid.code), suffix = "nf")] <-
      "DOC"
    if (verbose)
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Corg2DOC",
            "Waarden van Corg, na filtering worden als DOC gebruikt")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Corg2DOC",
            "Corg values 'na filtering' are set as modifyer DOC")
      }
  }
  if ("OS" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "OS"] <- "TSS"
    if (verbose)
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("OS2TSS", "Waarden van OS worden als TSS gebruikt")
      }  else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("OS2TSS", "OS values are set as modifyer TSS")
      }
  }

  #yes, again, to make sure changes in inputData are in?
  #grootheden <- unique(inputData[,

  # date and create SampleID from location/date ##### optional in future function?
  inputData$THEdate <-
    tryCatch(
      #      as.Date(
      as.character(
        inputData$Begindatum
        #        ,tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%y", "%m-%d-%Y", "%m/%d/%Y")
      ),
      error = function(cond)
        return(NA),
      silent = T
    )
  inputData$THEdate[is.na(inputData$THEdate)] <-
    tryCatch(
      as.character(
        #    as.Date(
        inputData$Resultaatdatum[is.na(inputData$THEdate)]
        #      ,tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%y", "%m-%d-%Y", "%m/%d/%Y")
      ),
      error = function(cond)
        return(NA),
      silent = T
    )
  if (any(is.na(inputData$THEdate)))
    if (National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoDateData", "rijen zonder datum zijn verwijderd")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoDateData", "data with a missing date are removed")
    }
  inputData <- inputData[!is.na(inputData$THEdate), ]
  #back to string to prevent mis-interpretation
  inputData$THEdate <- format(inputData$THEdate)
  #setup Samples: unique date-locations
  ToSample <- inputData[, c("Meetobject.lokaalID", "THEdate")]
  Astmp <-
    paste0(ToSample$Meetobject.lokaalID,
           ":",ToSample$THEdate)
  inputData$SampleID <- unlist(Astmp) #

  #modifyers & substance matching
  MODgrootheden <-
    inputData[inputData$Grootheid.code %in% Modifyers$ModifMODname, ]
  MODIons <-
    inputData[inputData$Parameter.code %in% Modifyers$ModifMODname,]
  #remove from inputData, Ions as MODifying factors are not considered as toxicant!
  inputData <-
    inputData[!(
      inputData$Grootheid.code %in% Modifyers$ModifMODname |
        inputData$Parameter.code %in% Modifyers$ModifMODname
    ), ]

  # prep UnitConversions ##### (loaded in Globals.R)

  #add the no-conversion needed for vectorisation
  AddUnitConversions <- data.frame(
    Unit_in = unique(UnitConversions$Unit_out),
    Unit_out = unique(UnitConversions$Unit_out),
    Multiply_by = 1,
    Mul_molmass = F
  )
  AllUnitConversions <- rbind (UnitConversions, AddUnitConversions)

  #combine the defaults, MOD & MOD  #####
  DataSamples <- data.frame(SampleID = unique(inputData$SampleID),
                            stringsAsFactors = F)

  # unit conversions #####

  #next sections to separate function?
  #possibly values in inputData #loop modifying variables for grootheid and Ions separate
  for (VarName in Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODgrootheden$Grootheid.code]) {
    #VarName = Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODgrootheden$Grootheid.code][1]
    RightRows <-
      which(MODgrootheden$Grootheid.code == VarName, arr.ind = T) # VarName rows in MODgrootheden (relevant part of input)
    TranslateUnits <-
      AllUnitConversions[AllUnitConversions$Unit_out == Modifyers$MODnameUnit[match(VarName, Modifyers$ModifMODname)], ]
    GivenUnits <- MODgrootheden$Eenheid.code[RightRows]
    Factors <-
      TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ] #order in subset of MODgrootheden  = data source
    if (anyNA(Factors$Unit_in)) {
      UnknownUnits <- unique(GivenUnits)
      UnknownUnits <-
        UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
      #quote the units to make the message clear
      UnknownUnits <- paste("'", paste(UnknownUnits, sep = "','"), "'", sep = "")
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          do.call(paste, as.list(c(UnknownUnits, "Unit not found for", VarName))))
      Factors$Multiply_by[is.na(Factors$Unit_in)] <- 1
      Factors$Div_molmass[is.na(Factors$Unit_in)] <- F
    }
    MatchSample <-
      match(DataSamples$SampleID, MODgrootheden$SampleID[RightRows]) #order in DataSamples = data target
    DataSamples[,VarName] <- Factors$Multiply_by[MatchSample] *
      MODgrootheden$Numeriekewaarde[RightRows][MatchSample]
    #Molmass not checked
  }
  for (VarName in Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODIons$Parameter.code]) { #VarName = "Ca"
    #VarName = Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODIons$Parameter.code][5]
    RightRows <- which(MODIons$Parameter.code == VarName, arr.ind = T)
    #unit conversion for this VarName
    TranslateUnits <-
      AllUnitConversions[AllUnitConversions$Unit_out == Modifyers$MODnameUnit[match(VarName, Modifyers$ModifMODname)], ]
    GivenUnits <- MODIons$Eenheid.code[RightRows]
    Factors <-
      TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ]  #order in MODIons  = data source
    if (anyNA(Factors$Unit_in)) {
      UnknownUnits <- unique(GivenUnits)
      UnknownUnits <-
        UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
      #quote them, to make a clear message and a single string
      UnknownUnits <- paste("'",
                            do.call(paste,c(as.list(UnknownUnits), list(sep = "','"))),
                            "'")
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Unknown",
            paste(
              UnknownUnits,
              "Onbekende eenheid voor",
              VarName,
              ", NIET omgerekend"
            ))
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Unknown",
            paste(
              UnknownUnits,
              "Unit(s) not found for",
              VarName,
              "NOT converted"
            ))
      }
      #assuming unit is correct, append
      Factors$Multiply_by[is.na(Factors$Unit_in)] <- 1
      Factors$Div_molmass[is.na(Factors$Unit_in)] <- F
    }
    MatchSample <-
      match(DataSamples$SampleID, MODIons$SampleID[RightRows])  #order in DataSamples = data target
    DataSamples[,VarName] <- Factors$Multiply_by[MatchSample] *
      MODIons$Numeriekewaarde[RightRows][MatchSample]
    if (!is.na(Modifyers$MW.g.Mol[match(VarName, Modifyers$ModifMODname)])) {
      #possibly multiply by molar mass [g/mol]
      toMul <- which(Factors$Mul_molmass[MatchSample])
      if (length(toMul) > 0) {
        DataSamples[toMul, VarName] <- DataSamples[toMul, VarName] *
          Modifyers$MW.g.Mol[match(VarName, Modifyers$ModifMODname)]
      }
    }
  }

  #Units of substances and  hoedanigheid N/C  nf  NF and prepare for msPAF#####
  #match substances; register with CAS but not matched

  #force valid CAS, but keep the original CAS code
  #NB the CAS variable will be the ultimate match to the SSD data
  MatchChar2CAS <- match(inputData$Parameter.code, OtherChar$ChemCode)
  MatchChar2CAS[is.na(MatchChar2CAS)] <- match(inputData$Parameter.CASnummer[is.na(MatchChar2CAS)], OtherChar$ChemCode)
  inputData$CAS <- OtherChar$CAS[MatchChar2CAS] #init. with OtherChar, if present. Might be NA, might be update by:
  UniqCas <- unique(inputData$Parameter.CASnummer)
  CheckCASvalidUniq <- ValidCAS(UniqCas)
  CheckCASvalid <- inputData$Parameter.CASnummer %in% UniqCas[CheckCASvalidUniq]
  inputData$CAS[CheckCASvalid & is.na(MatchChar2CAS)] <- inputData$Parameter.CASnummer[CheckCASvalid& is.na(MatchChar2CAS)]
  unused <-
    inputData[is.na(inputData$CAS), c("Parameter.code", "Parameter.CASnummer")]
  NonSSDbronCAS <- unique(inputData$CAS[!is.na(inputData$CAS) & !inputData$CAS %in% SSDbron$CAS])
  if (length(NonSSDbronCAS) > 0) {
    CASnotChemlist <- do.call(paste,as.list(unique(NonSSDbronCAS)))
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NAmatchedCode",
          paste (nrow(NonSSDbronCAS), paste("stoffen komen niet voor in de stoffenlijst", CASnotChemlist)))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NAmatchedCode",
          paste (nrow(NonSSDbronCAS), paste("substances with CAS not in chemistry list", CASnotChemlist)))
    }
  }

  #remove all non substance measurements
  inputData <- inputData[!is.na(inputData$CAS), ]
  inputData <- inputData[!inputData$CAS %in% NonSSDbronCAS, ]

  #unit conversion for the toxic substances
  TranslateUnits <-
    AllUnitConversions[AllUnitConversions$Unit_out == "ug/l", ] #the unit in SSD's
  GivenUnits <- inputData$Eenheid.code
  Factors <-
    TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ]
  if (anyNA(Factors$Unit_in)) {
    UnknownUnits <- unique(GivenUnits)
    UnknownUnits <-
      UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
    UnknownUnits <- paste("'",
                          do.call(paste,c(as.list(UnknownUnits), list(sep = "','"))),
                          "'")
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          paste(
            UnknownUnits,
            "onbekende concentratie eenheid; De rijen zijn verwijderd"
          ))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          paste(
            UnknownUnits,
            " unknown concentration unit(s); row(s) deleted"
          ))
    }
    inputData <- inputData[!is.na(Factors$Unit_in),]
    Factors <- Factors[!is.na(Factors$Unit_in),]
  }

  #Match to SSD data source
  MatchChem <- match(inputData$CAS, SSDbron$CAS)
  inputData$Concentration <- inputData$Numeriekewaarde * Factors$Multiply_by
  toMul <- which(Factors$Mul_molmass)
  if (length(toMul) > 0) {
    inputData$Concentration[toMul] <- inputData$Concentration[toMul] *
      SSDbron[MatchChem[toMul],MolMassName]
  }
  inputData$NaFiltering <- endsWith(inputData$Hoedanigheid.code, "nf")
  AsN <- which(inputData$Hoedanigheid.code %in% c("N", "Nnf")) # correct to molmass of whole molecule
  if (length(AsN)>0) {
    ATOMMASSN <- 14.0067
    inputData$Concentration[AsN] <- inputData$Concentration[AsN] / ATOMMASSN * SSDbron[MatchChem[AsN],MolMassName]
  }
  AsP <- which(inputData$Hoedanigheid.code %in% c("P", "Pnf")) # correct to molmass of whole molecule
  if (length(AsP)>0) {
    ATOMMASSP <- 30.973762
    inputData$Concentration[AsP] <- inputData$Concentration[AsP] / ATOMMASSP * SSDbron[MatchChem[AsP],MolMassName]
  }

  #with defaults #from the one row in ModifierDefaults
  if ("TSS" %in% names(DataSamples))
    DataSamples$TSS[is.na(DataSamples$TSS)] <-
    ModifierDefaults$`TSS(mg/L)`[1] else #new column
      DataSamples$TSS <- ModifierDefaults$`TSS(mg/L)`[1]
  if ("POC" %in% names(DataSamples))
    DataSamples$POC[is.na(DataSamples$POC)] <-
    ModifierDefaults$`POC(mg/kg)`[1] else #new column
      DataSamples$POC <- ModifierDefaults$`POC(mg/kg)`[1]
  if ("DOC" %in% names(DataSamples))
    DataSamples$DOC[is.na(DataSamples$DOC)] <-
    ModifierDefaults$`DOC(mg/L)`[1] else #new column
      DataSamples$DOC <- ModifierDefaults$`DOC(mg/L)`[1]
  if ("pH" %in% names(DataSamples))
    DataSamples$pH[is.na(DataSamples$pH)] <-
    ModifierDefaults$`pH(units)`[1] else #new column
      DataSamples$pH <- ModifierDefaults$`pH(units)`[1]
  if ("Tw" %in% names(DataSamples))
    DataSamples$Tw[is.na(DataSamples$Tw)] <-
    ModifierDefaults$`Tw(oC)`[1] else #new column
      DataSamples$Tw <- ModifierDefaults$`Tw(oC)`[1]
  if ("Ca" %in% names(DataSamples))
    DataSamples$Ca[is.na(DataSamples$Ca)] <-
    ModifierDefaults$`Ca(ug/L)`[1] else #new column
      DataSamples$Ca <- ModifierDefaults$`Ca(ug/L)`[1]
  if ("Mg" %in% names(DataSamples))
    DataSamples$Mg[is.na(DataSamples$Mg)] <-
    ModifierDefaults$`Mg(ug/L)`[1] else #new column
      DataSamples$Mg <- ModifierDefaults$`Mg(ug/L)`[1]
  if ("Na" %in% names(DataSamples))
    DataSamples$Na[is.na(DataSamples$Na)] <-
    ModifierDefaults$`Na(ug/L)`[1] else #new column
      DataSamples$Na <- ModifierDefaults$`Na(ug/L)`[1]
  if ("Cl" %in% names(DataSamples))
    DataSamples$Cl[is.na(DataSamples$Cl)] <-
    ModifierDefaults$`Cl(ug/L)`[1] else #new column
      DataSamples$Cl <- ModifierDefaults$`Cl(ug/L)`[1]

  #for easier name handling in package
  names(inputData)[names(inputData)=="Parameter.code"] <- "ChemCode"
  if ("MeasuredValue" %in% names(inputData)) {
    inputData$MeasuredValue <- NULL #free name for Concentration column
  }
  names(inputData)[names(inputData)=="Concentration"] <- "MeasuredValue"
  inputData$PreTreatment <- ifelse(inputData$NaFiltering,"nf","")
  attr(inputData, "conforms") <- conforms

  #return
  list(
    inputData = inputData,
    DataSamples = DataSamples,
    inputwarnings = inputwarnings
  )
}
getAppVersion <- function() {
  if (file.exists("VERSION")) {
    readLines("VERSION", warn = FALSE)[1]
  } else if (file.exists("DESCRIPTION")) {
    desc <- read.dcf("DESCRIPTION")
    desc[1, "Version"]
  } else {
    "unknown"
  }
}
#what are the gross files to load in
gross_folder <- "./data"
SSDplusList <- readRDS(paste0(gross_folder,"/SSDplusList.RDS"))
for (name in names(SSDplusList)) {
  #get the object name, based on the name
  assign(name, SSDplusList[[name]], envir = .GlobalEnv)
}
gross_choices <- ls(pattern = "^Gross")

#get the git head
if (!is.null(attributes(SSDplusList)$githead)) {
  git_head <- attributes(SSDplusList)$githead
  if( grepl(" (HEAD) ",git_head, fixed = TRUE)){
    git_head <- unlist(strsplit(git_head," (HEAD) ",fixed = TRUE))[2]
  }
} else {
  git_head <- NULL
}
git_head <- attributes(SSDplusList)$githead
git_head <- unlist(strsplit(git_head," (HEAD) ",fixed = TRUE))[2]
##############################################################################################
# Input file
Gross <- SSDplusListRDS$Gross2023

# Variables of the function
filename = "./data/msPAFtoolExampleNL.xlsx"
SSDbron = "Gross"
gross_name = "Gross"
MolMassName = "MW.g.Mol"
National = "Nederlands"
verbose = T
data2 <- leesIMformat(filename, SSDbron = SSDbron, MolMassName = MolMassName, National = National, verbose = verbose)
data <- leesIMformat_old(filename, SSDbron = SSDbron, MolMassName = MolMassName, National = National, verbose = verbose)
#data3 <- leesIMformat_multi(filename, SSDbron = SSDbron, MolMassName = MolMassName, National = National, verbose = verbose)
#str(data)
#str(data2)
library(testthat)
try(expect_equal(data$inputData, data2$inputData))
