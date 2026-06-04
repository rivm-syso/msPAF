# ---
# title: "helpers"
# author: "Jaap Slootweg"
# date: "`r Sys.Date()`"
# output: html_document
# ---
#
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
#
# ## helper functions for cleaning input data to the calculation routines
#
# ```{r}
# First make sure the package is loaded
#  if(!require("msPAF")) {
    # print("installing msPAF package from git")
    # remotes::install_gitlab("slootwej/mspaf",
    #                           host = "https://gitlab.rivm.nl",
    #                         ref = "testing"
    #                           # build_vignettes = TRUE
    #                         )
#  }
rm(list=ls())
devtools::load_all()

Gross <- SSDplusListRDS$Gross2023

# version should go the the "warnings", for this we have:
version_string <- get_package_version_string("msPAF")

# ```
#
# ### IMColNameMapper
#
# This example demonstrates how to use the IMColNameMapper class to translate column
# names between English and Dutch. The mapper is initialized with a named vector where
# the English names are the names of the vector and the Dutch names are the values. The
# to_dutch() method converts all column names in a data frame from English to Dutch
# whenever a match exists in the mapping. The to_english() method performs the reverse
# operation by automatically constructing the inverse mapping and renaming Dutch
# columns back to English.
# In this demonstration, we define a simple mapper for three columns, create a small English
# data frame, convert it to Dutch, and then convert it back to English. This shows how the
# mapper can be embedded in data-processing workflows where both English and Dutch
# input files are supported.
#
# ```{r}
library(R6)
mapping <- c(
  AlphanumericValue = "Alfanumeriekewaarde",
  LimitSymbol = "Limietsymbool",
  NumericValue = "Numeriekewaarde"
)
mapper <- IMColNameMapper$new(mapping)
df_english <- data.frame(
  AlphanumericValue = c("<5", "10"),
  LimitSymbol = c("<", ""),
  NumericValue = c("5.0", "10.0"),
  stringsAsFactors = FALSE
)
df_dutch <- mapper$to_dutch(df_english)
df_back_to_english <- mapper$to_english(df_dutch)
list(english = df_english,
dutch = df_dutch,
back_to_english = df_back_to_english)
# ```
#
# ### check_alfanumeriekewaarde
#
# This example demonstrates how the function check_alfanumeriekewaarde() validates the
# relationship between the column Alfanumeriekewaarde and the columns Limietsymbool
# and Numeriekewaarde. The function ensures that the Alfanumeriekewaarde column exists,
# and then checks two types of inconsistencies. First, if the alphanumeric value begins with "
# <" or ">" but the Limietsymbool column does not contain the corresponding symbol, a
# limit inconsistency warning is added. Second, if a purely numeric value appears in the
# alphanumeric column while the NumericValue column is missing or NA, a warning is
# added because the numeric value appears misplaced.
# In this demonstration, a small dataset is constructed that intentionally contains both types
# of inconsistencies. An InputWarnings R6 object is created to collect warnings during the
# check. After running the function, the modified data and the collected warnings are
# printed. This illustrates how the function helps detect common data-entry issues in
# imported datasets.
#
#
# ```{r}
library(R6)

inputwarnings <- InputWarnings$new(version_string)
inputData <- data.frame(
Alfanumeriekewaarde = c("<5", "10", ">3"),
Limietsymbool = c("", "<", ""),
Numeriekewaarde = c(NA, 10, 3),
stringsAsFactors = FALSE
)
result <- check_alfanumeriekewaarde(inputData, inputwarnings, "English")
list(result = result,
warnings = inputwarnings$warnings)
# ```
# ### check_numeriekewaarde
#
# This example demonstrates how the function check_numeriekewaarde() validates and
# sanitizes the column Numeriekewaarde. The function first ensures that the column is
# stored as a true numeric vector. If commas are used as decimal separators, they are
# converted to dots before numeric conversion. Any values that cannot be converted to
# numbers become NA.
# After conversion, the function removes all rows where Numeriekewaarde is NA. When rows
# are removed, a warning is added to the supplied InputWarnings object so that the user is
# informed that non-numeric values were present in the input data. This ensures that
# downstream processing always uses clean numeric data and that data quality issues are
# explicitly reported.
# In the example below, the input dataset includes valid numeric values, values using a
# comma as decimal separator, and invalid text. The function converts the valid ones,
# removes the invalid row, and records a warning.
#
# ```{r}
library(R6)
inputwarnings <- InputWarnings$new(version_string)
inputData <- data.frame(
  Numeriekewaarde = c("10", "3,5", "abc", "7.2"),
  stringsAsFactors = FALSE
)
result <- check_numeriekewaarde(inputData, inputwarnings, "English")
list(result = result,
warnings = inputwarnings$warnings)
# ```
#
# ### filter_limietsymbool
#
# This example demonstrates how the function filter_limietsymbool() removes rows that
# contain the limit symbols "<" or ">" in the column Limietsymbool. These rows represent
# values outside the quantification range and are intentionally excluded from further
# numerical processing. The function also supports an optional warning mechanism. When
# verbose is TRUE, the function adds a warning to the InputWarnings object describing how 
# many rows were removed.
# In addition to filtering, the function subsets the returned data frame so that only the
# columns listed in UsedParameters and OptionalParameters are retained. Columns not
# included in those vectors are dropped from the output. This ensures that subsequent
# processing steps receive only the relevant variables.
# In the demonstration below, a dataset with multiple rows is constructed, including some
# with limit symbols. After filtering, those rows are removed, and a warning is recorded. The
# code shows the input data, the filtered result, and the collected warnings
#
# ```{r}
library(R6)
inputwarnings <- InputWarnings$new(version_string)
inputData <- data.frame(
  Limietsymbool = c("<", "", ">", "", ""),
  Value = c(1, 2, 3, 4, 5),
  Extra = c("A", "B", "C", "D", "E"),
  stringsAsFactors = FALSE
)
UsedParameters <- c("Value")
OptionalParameters <- c("Extra")
result <- filter_limietsymbool(
  inputData,
  inputwarnings,
  National = "English",
  verbose = TRUE,
  UsedParameters = UsedParameters,
  OptionalParameters = OptionalParameters
)
list(result = result,
warnings = inputwarnings$warnings)
# ```
#
# ### handle_grootheden_peculiarities
#
# This example demonstrates how the function handle_grootheden_peculiarities() corrects
# known special cases in measurement codes according to the IM standard. The function
# inspects combinations of Grootheid.code and Parameter.code and applies three types of
# conversions.
# First, if the column Grootheid.code contains the code "T" but not "Tw", all occurrences of
# "T" are interpreted as water temperature and recoded to "Tw". A warning is added to the
# InputWarnings object to document this assumption.
# Second, if Parameter.code is "Corg" and Hoedanigheid.code ends with "nf" (indicating “na
# filtering”), these measurements are interpreted as dissolved organic carbon and the
# parameter code is recoded to "DOC". When verbose is TRUE, a warning is added
# explaining that Corg values after filtration have been treated as DOC.
# Third, if Parameter.code is "OS", these measurements are interpreted as total suspended
# solids and the code is recoded to "TSS". Again, when verbose is TRUE, a warning is added
# indicating that OS values are treated as TSS.
# In the example below, the input dataset contains codes "T", "Corg" with and without "nf"
# in Hoedanigheid.code, and "OS". After calling the function, "T" is converted to "Tw", "Corg"
# combined with "nf" is converted to "DOC", and "OS" is converted to "TSS". The collected
# warnings show which assumptions were applied.
#
# ```{r}
library(R6)
inputwarnings <- InputWarnings$new(version_string)
inputData <- data.frame(
  Grootheid.code = c("T", "T", "Q", "Q"),
  Parameter.code = c("Param1", "Corg", "Corg", "OS"),
  Hoedanigheid.code = c("P", "Nnf", "nf", ""),
  stringsAsFactors = FALSE
)
result <- handle_grootheden_peculiarities(
  inputData,
  inputwarnings,
  National = "English",
  verbose = TRUE
)
list(result = result,
warnings = inputwarnings$warnings)

# ```
#
# ### ensure_thedate_and_sampleid
# This example demonstrates how the function ensure_thedate_and_sampleid() constructs a
# unified date column (THEdate) and a sample identifier (SampleID) from date and location
# information. The function first attempts to create THEdate from the column Begindatum. If
# Begindatum is missing or cannot be converted for some rows, those missing values are
# filled using Resultaatdatum instead. Rows for which both dates are missing are removed
# from the dataset. When such rows are removed, a warning is added to the InputWarnings
# object so that the loss of data is transparent.
# After the dates are cleaned and THEdate has been created for all remaining rows, the
# function formats THEdate back to character to avoid unintended date conversions later in
# the workflow. If both Meetobject.lokaalID (location identifier) and THEdate are present, a
# SampleID column is created by concatenating location and date with a colon, producing
# unique identifiers for each location-date combination.
# In the example below, one row has only Begindatum, another has only Resultaatdatum,
# and a third row has neither date. The function constructs THEdate from the available date
# columns, removes the row without any date, and builds the SampleID values from the
# remaining rows.
# ```{r}
library(R6)
inputwarnings <- InputWarnings$new(version_string)
inputData <- data.frame(
  Begindatum = c("2020-01-01", NA, NA),
  Resultaatdatum = c(NA, "2020-02-01", NA),
  Meetobject.lokaalID = c("LOC1", "LOC2", "LOC3"),
  stringsAsFactors = FALSE
)
result <- ensure_thedate_and_sampleid(
  inputData,
  inputwarnings,
  National = "English"
)
list(result = result,
warnings = inputwarnings$warnings)
# ```
# ### apply_modifying_variables
# This example demonstrates how the function apply_modifying_variables() fills modifier
# columns in the DataSamples table based on separate measurement data in ModData. Each
# modifier (for example, TSS, DOC, pH, etc.) is identified by a code listed in present_codes.
# For each code, the function selects the corresponding rows from ModData using either
# Grootheid.code or Parameter.code, converts the values to the target unit using
# UnitConversions, and then writes the resulting numeric values into the matching rows of
# DataSamples. Matching between ModData and DataSamples is done via the SampleID
# column so that each sample receives the appropriate modifier value.
# The function also checks whether all units appearing in ModData are known in the
# UnitConversions table. If an unknown unit is encountered, a warning is added to the
# InputWarnings object, and those rows are effectively passed through without conversion
# (Multiply_by is set to 1). Optionally, if use_molmass is TRUE and the modifier is associated
# with a molecular mass (for ions), the function multiplies the converted value by the molar
# mass for rows where Mul_molmass is TRUE, converting from mass-based to substance-based units.
# In the example below, we construct a simple DataSamples table with two samples and a
# ModData table with TSS measurements in two different units: a known unit "mg/L" and an
# unknown unit "foo". The UnitConversions table only contains a conversion for "mg/L" to
# "mg/L". After running apply_modifying_variables(), the TSS column is filled in DataSamples,
# and a warning is generated for the unknown unit.
#
# ```{r}
library(R6)
inputwarnings <- InputWarnings$new(version_string)
DataSamples <- data.frame(

SampleID = c("LOC1:2020-20-20", "LOC2:2020-20-20"),
  stringsAsFactors = FALSE
)

ModData <- data.frame(
  SampleID = c("LOC1:2020-01-01", "LOC2:2020-01-01"),
  Grootheid.code = c("TSS", "TSS"),
  Eenheid.code = c("mg/L", "foo"),
  Numeriekewaarde = c(10, 20),
  stringsAsFactors = FALSE
)

present_codes <- c("TSS")
TODO
result <- apply_modifying_variables(
  DataSamples = DataSamples,
  ModData = ModData,
  Modifyers = Modifyers,
  AllUnitConversions = UnitConversions,
  inputwarnings = inputwarnings,
  National = "English",
  present_codes = present_codes,
  use_molmass = FALSE
)
list(result = result,
warnings = inputwarnings$warnings)
# ```
#
# ### Substances
# We have a few issues related to substances, and their identification. Historically CAS registration numbers
# were the identifyer. Increased chemical knowledge makes updates on these CAS numbers needed. A part of the substances are actually mixtures of chemicals. Data on toxicity tests refer historically to CAS numbers, but
# US-EPA and ECHA cannot always (acurately) map the chemical to a CAS. In the dataset of SSDs and other properties
# The CAS numbers are still used, but the chemical identifyer CAN be different, either a EC number (from ECHA) or an IDENT from the US-EPA.
# There are several reasons why we need to "translate" the chemical reference in the input file to the chemical in the SSD-dataset. Firstly the use in the Netherland of the aquo-codes. Secondly the known updates of CAS codes. Thirdly the "groepstoffen" as used in the "Bestrijdingsmiddellen Atlas", that are in use when the detection of a single substance is not possible, but the signal is good enough to estimate a concentration of a group of very similar substances. Fourthly to assign an SSD from a well known chemical to a substance for which no trustworthy SSD can be determined. To deal with this we created a table of the translation, eg the possible code in the input file, and the code that is in the SSD-file, togeteher with the reason/origine. This dataset is available in the package as data (OtherChar)
# The Chemical reference in the input file can be in two columns, with Dutch names(Parameter.code - not always a substance) and Parameter.CASnummer. The English version has CAS for "Parameter.CASnummer". There are two scenario, the user has CAS numbers in the data, or prefers/mixes the CAS numbers with data in the Parameter.code. To deal with these scenarios robustly, the CAS, or "Parameter.CASnummer" is used if it is known in the SSD dataset. If
# Parameter.code is empty the Parameter.CASnummer is copied into Parameter.code. Then the Parameter.code are input to the mapping of Otherchar and if there is a match, this code is put to the Parameter.CASnummer. This way the Parameter.code always contains the reference from the user, the Parameter.CASnummer/CAS column contains the reference to the SSD dataset. In exceptional cases the values in Parameter.CASnummer/CAS column are identifyers that are not CAS.
# Lastly, Parameter.CASnummer/CAS values that are not in SSD dataset are verified for the checksum of CAS. This to make a warning to the user that either the CAS code is invalid, or has no (proper) SSD
#
# ```{r}
# Example data
inputData <- data.frame(
  Parameter.code = c(
    "SULFATE",                 # 1. Direct match via OtherChar
    "",                        # 2. Filled by CASnummer, matches SSDbron
    NA,                        # 3. Filled by CASnummer, matches SSDbron
    "INVALIDCAS",              # 4. Invalid code, filled by invalid CASnummer
    "BHT",                     # 5. Valid ChemCode, valid CAS in SSDbron
    "NotInOtherChar",          # 6. Not in OtherChar, CASnummer valid but not in SSDbron
    "NaCl",                    # 7. Direct match via OtherChar
    "Benzene",                 # 8. Direct match via OtherChar
    "HCl",                     # 9. Direct match via OtherChar
    "CO2",                     # 10. Direct match via OtherChar
    "CustomLowercase",         # 11. Case sensitivity test (should match)
    "DUPLICATE",               # 12. Duplicate ChemCode
    "",                        # 13. Both blank/NA
    NA,                        # 14. Both blank/NA
    "UNKNOWN",                 # 15. Not in OtherChar, CASnummer also not in SSDbron
    "H2SO4"                    # 16. Code in OtherChar, different CASnummer
  ),
  Parameter.CASnummer = c(
    "7664-93-9",               # 1. Also present in SSDbron
    "7732-18-5",               # 2. Present in SSDbron
    "7647-14-5",               # 3. Present in SSDbron
    "123-45-6",                # 4. Invalid CAS (bad checksum)
    "128-37-0",                # 5. Present in SSDbron
    "9000-01-1",               # 6. Valid CAS, not in SSDbron
    "",                        # 7. Blank CASnummer
    NA,                        # 8. NA CASnummer
    "7647-01-0",               # 9. Present in SSDbron
    "124-38-9",                # 10. Present in SSDbron
    "9000-00-0",               # 11. Not in SSDbron, valid format (not in SSDbron)
    "50-00-0",                 # 12. Duplicate ChemCode, valid in SSDbron
    "",                        # 13. Both blank
    NA,                        # 14. Both NA
    "999-99-9",                # 15. Valid CAS, not in SSDbron
    "1111-11-1"                # 16. Different valid, not in SSDbron
  ),
  stringsAsFactors = FALSE
)


inputwarnings <- InputWarnings$new(version_string)

# Map and validate
result <- check_substance_codes(inputData, OtherChar, SSDbron = Gross, inputwarnings, National = "English")

# Show result and warnings
list(result = result,
warnings = inputwarnings$warnings)
# ```
#
# ### convert_units_and_calculate_concentration
# This example demonstrates how the function convert_units_and_calculate_concentration()
# prepares a concentration column ready for SSDcalculations. The function takes raw measurement
# data (inputData) and performs several steps.
# The function converts all concentrations to a common unit (ug/l) using the
# AllUnitConversions table. This table defines, for each incoming unit (Unit_in), the output
# unit (here ug/l), a numeric Multiply_by factor, and an optional Mul_molmass flag indicating
# that molar mass should be applied later. Rows with unknown units are reported in a
# warning and removed from the data.
# It then calculates a Concentration column as Numeriekewaarde multiplied by the
# appropriate Multiply_by factor. For rows where Mul_molmass is TRUE, the Concentration is
# further multiplied by the substance’s molecular mass taken from the SSDbron table (using
# the MolMassName column).
# Finally, the function applies special handling for N and P forms. For Hoedanigheid.code
# equal to "N" or "Nnf", the concentration is converted from N-based units to substance-based units using the atomic mass of nitrogen and the molecular mass in SSDbron. The
# same is done for "P" or "Pnf" using the atomic mass of phosphorus. A logical column
# NaFiltering is also added to indicate whether Hoedanigheid.code ends with "nf".
# In the example below, we construct a small measurement table with different units, some
# substances present in the SSD list and one missing, and different Hoedanigheid.code
# values. After running the function, only substances with known CAS, present in SSDbron
# and with known units remain, and a Concentration column in ug/l is added.
#
# ```{r}
library(R6)
inputwarnings <- InputWarnings$new(version_string)

# --- SSD reference table ---
SSDbron <- data.frame(
  CAS = c("1111-11-1", "2222-22-2", "3333-33-3", "5555-55-5", "6666-66-6"),
  MW.g.Mol = c(100, 50, 30, 60, 40),
  stringsAsFactors = FALSE
)

# --- Input data with more variety ---
inputData <- data.frame(
  Parameter.code = c("CHEM1", "CHEM2", "CHEM3", "CHEM4", "CHEM5", "CHEM6", "CHEM7", "CHEM8"),
  CAS = c("1111-11-1", "2222-22-2", "3333-33-3", "4444-44-4", "5555-55-5", "6666-66-6", "3333-33-3", "2222-22-2"),
  Eenheid.code = c("mg/l", "ug/l", "mmol/l", "mg/l", "ng/l", "mmol/l", "mmol/l", "mg/l"),
  Numeriekewaarde = c(1.5, 3000, 0.002, 5, 1e6, 0.05, 0.1, 0.5),
  Hoedanigheid.code = c("W", "", "", "", "W", "Nnf", "N", "P"),
  stringsAsFactors = FALSE
)

# 2. Unit conversion and concentration calculation
result <- convert_units_and_calculate_concentration(inputData, SSDbron, UnitConversions, inputwarnings, "English", "MW.g.Mol")

list (result = result,
warnings = inputwarnings$warnings)
# ```
#
# ### fill_modifiers_with_defaults
# This example demonstrates how the function fill_modifiers_with_defaults() ensures that all
# required modifier columns are present in the DataSamples table and that missing values
# are replaced with sensible defaults. The function uses a fixed mapping between the
# column names in DataSamples (TSS, POC, DOC, pH, Tw, Ca, Mg, Na, Cl) and the
# corresponding default-value columns in ModifierDefaults (for example, "TSS(mg/L)" or
# "pH(units)"). For each modifier, it reads the default value from ModifierDefaults. If the
# modifier column already exists in DataSamples, all NA values in that column are replaced
# with the default. If the column does not exist, it is created and filled entirely with the
# default value for all samples.
# This mechanism allows the rest of the workflow to rely on a complete set of modifiers
# without having to check for missing columns or values. It is particularly useful after
# applying measured modifiers where not all samples have measurements for each
# modifying variable. Defaults can reflect typical background conditions or other agreed 
# values.
# In the example below, DataSamples has some modifiers present with missing values and
# other modifiers absent entirely. ModifierDefaults (data of the msPAF package) contains a single row with default values
# for all modifiers. After calling fill_modifiers_with_defaults(), every modifier column is
# present in DataSamples and contains either the original value or the default value where
# data was missing.
# ```{r}
DataSamples <- data.frame(
  SampleID = c("LOC1:2020-01-01", "LOC2:2020-01-01"),
  TSS = c(NA, 15),
  DOC = c(2.5, NA),
  pH = c(7.0, NA),
  stringsAsFactors = FALSE
)

result <- fill_modifiers_with_defaults(DataSamples, ModifierDefaults)
result

# ```

