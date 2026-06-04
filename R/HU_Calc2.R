#' @title HU_Calc2
#' @name  HU_Calc2
#' @author jaap slootweg
#' @description calculate and order Hazard Units (HU) for samples of concentrations and optional bio availability modifyers
#' @param ToHU data.frame with concentrations with at least the columns CAS, SampleID, MeasuredValue, possibly Filtered | PreTreatment
#' @param ChemData data.frame like DefChemical
#' @param SSDreplace data.frame like Defreplacements, wth at least the columns
#' @param EnvData data.frame like defmodifyers
#' @param DefaultFiltered set Filtered to, only if column "PreTreatment" is missing
#' @param DevRange Dev values outside this range will be set to 0.7 (average dev all SSDs)
#' @param aggrFUN function to aggregate the measurements, for example in case of multiple measurements
#' @param aggrParam see ...
#'
#' @return List with three elements:
#'   \itemize{
#'     \item \code{PAF}: data.frame with msPAF ready data, includes ExclusionReason column (NA for kept rows)
#'     \item \code{excluded_rows}: data.frame with all excluded rows and their ExclusionReason
#'     \item \code{warning}: list of warnings generated during processing
#'   }
#' @export
HU_Calc2 <- function (ToHU, ChemData = Gross, filter_expr = NULL, EnvData = NULL,
                      muNames = c(acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"),
                      sigmaNames = c(acute = "Acute2.0Dev10LogMassTox.ug.L", chronic = "Chronic2.0Dev10LogMassTox.ug.L"),
                      National,
                      DefaultFiltered = TRUE, aggrFUN = NULL, DevRange = c(0.2,2),
                      status_bioavailability=TRUE, TooLowLimit = NULL){

  Awarning <- list()
  excluded_rows <- data.frame()

  if (!is.null(filter_expr)){
    filter_expr <- rlang::enquo(filter_expr)
    ChemData <- ChemData %>% filter(!!filter_expr)
  }


  if (!all(c(muNames,sigmaNames) %in% names(ChemData))) {
    Awarning[["muSigmaMissing"]] <- paste("Missing mu and/or sigma;", muNames, sigmaNames)
    emptyPAF <- data.frame()
    emptyPAF$ExclusionReason <- NA_character_
    attr(emptyPAF, "warning") <- Awarning
    return(list(PAF = emptyPAF, excluded_rows = data.frame(), warning = Awarning))
  }
  # Rename columns to match expected names if they come from leesIMformat
  # ChemCode now contains the mapped CAS/identifier (from check_substance_codes)
    # For now, we only have CAS values as id for SSD, we can prepare for the near future
  if (!"substance_key" %in% colnames(ChemData)){
      ChemData$substance_key <- paste0("CAS:", ChemData$CAS)
  }

  needDataColumns <- c("NaFiltering", "SampleID", "substance_key", "Concentration")
  if(!all(needDataColumns %in% names(ToHU))){
    # TODO better to stop; clear to user; these columns ARE needed
    missingColumns <- needDataColumns [!needDataColumns %in% names(ToHU)]
    stop(paste("Missing data column;", missingColumns))
  }

  if (!is.null(aggrFUN)) {
    stop("aggrFUN is not part of HU_Calc2 anymore. Please use the aggre_HU_Calc2 function.")
  }

  # Initialize PAF with the input data (aggregation is now done separately via aggre_HU_Calc2)
  PAF <- ToHU

  ChemMatch <- match(PAF$substance_key, ChemData$substance_key)
  if (anyNA(ChemMatch)){
    stop("should have been cleaned before")
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
  if (is.null(EnvData)) {
    names(ModifierDefaults) <- sub("\\(.+\\)", "", names(ModifierDefaults))
    EnvData <- ModifierDefaults
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
  PAF$DissConc <- PAF$Concentration
  iToHUisNH4 <- which(PAF$Parameter.code == "sNH3NH4") #from specific dutch aquocode; sum of NH3 and NH4
  #NH4 / sNH3NH4 are considered the actual measured NH4 concentration, possibly replacing the calculated C[NH3]
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    PAF$DissConc[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * PAF$Concentration[iToHUisNH4]
    #remove others in samples
    iToDel <- which(PAF$SampleID %in% PAF$SampleID[iToHUisNH4] & PAF$substance_key %in% c("NH3", "NH4"))
    if (length(iToDel) > 0 ){
      excluded_NH4_a <- PAF[iToDel, ]
      excluded_NH4_a$ExclusionReason <- "NH3/NH4 duplicate removed (sNH3NH4 exists)"
      excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_NH4_a)
      PAF <- PAF[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(PAF$substance_key, ChemData$substance_key)
    }
  }
  iToHUisNH4 <- which(PAF$Parameter.code == "NH4" | PAF$Parameter.CASnummer == "1479-03-9") # == NH4
  # replaced by toxicant NH3, mind the "LeenSSD" for this!
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    PAF$DissConc[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * PAF$Concentration[iToHUisNH4]
    #remove others in samples
    iToDel <- which(PAF$SampleID %in% PAF$SampleID[iToHUisNH4] & PAF$CAS == "CAS:7664-41-7") # == NH3
    if (length(iToDel) > 0 ){
      excluded_NH4_b <- PAF[iToDel, ]
      excluded_NH4_b$ExclusionReason <- "NH3 removed (NH4 is the measured value)"
      excluded_rows <- dplyr::bind_rows(excluded_rows, excluded_NH4_b)
      PAF <- PAF[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(PAF$substance_key, ChemData$substance_key)
    }
  }

# Correction for bioavailability of metals/OC ---------------------------

  #metals / Organics differ
  metals <- !is.na(PAF$MOI) & PAF$MOI == "M"
  SpecialM <- !is.na(ChemData$CoefLogMeTot[ChemMatch]) & ChemData$CoefLogMeTot[ChemMatch] != 0
  organic <- !is.na(PAF$MOI) & PAF$MOI == "O"

  #Correction of metal concentrations for bioavailability
  if (status_bioavailability) {
    PAF$DissConc[metals] <- ifelse(PAF$NaFiltering[metals], PAF$Concentration[metals],
                                    PAF$Concentration[metals] / (1 + PAF$TSS[metals] * 10 ^ -6 * 10 ^ PAF$KocNew[metals])
    )
  }

  #for Inorganic & metals, excluding SpecialM (overwritten in the next line)
  PAF$ActConc <- PAF$DissConc

  #Correction of special metal concentrations for bioavailability
  # TODO check: should this be executed only if NOT filtered, i.e. NaFiltering == FALSE??
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

  #Correction of organic content concentrations for bioavailability
  if (status_bioavailability) {
    POC <- ModifierDefaults$`POC(mg/kg)` # TODO Check, not from EnvData ??
    PAF$ActConc[organic] <- ifelse(PAF$NaFiltering[organic], PAF$Concentration[organic],
                                    PAF$Concentration[organic] /
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
  #TooLowText <- paste("<", TooLowLimit)
  #PAF$TooLowAcute <- ifelse(PAF$HU_acute < TooLowLimit, TooLowText, "")
  #PAF$TooLowChronic <- ifelse(PAF$HU_Chronic < TooLowLimit, TooLowText, "")
  if (!is.null(aggrFUN)) {
    stop("TooLowLimit is not part of HU_Calc2 anymore. Please use the seperate function.")
  }

  #keep all relevant attributes
  AllNames <- c("substance_key","UseClass",
                "Meetobject.lokaalID", "THEdate",
                "Concentration", "ActConc", "NaFiltering",
                "HU_acute", "HU_Chronic", "PAFacute", "PAFchronic",
                "groep.fotoNL","PrimaryMoA","SampleID",
                #"TooLowAcute", "TooLowChronic",
                "Avg10Log_acute","Dev10Log_acute",
                "Avg10Log_chronic","Dev10Log_chronic")

  PAF <- PAF[,AllNames[AllNames %in% names(PAF)]]

  # Add ExclusionReason column as NA for all kept rows
  if(!("ExclusionReason" %in% names(PAF))){
    PAF$ExclusionReason <- NA_character_
  }

  return(list(PAF = PAF, excluded_rows = excluded_rows, warning = Awarning))
}
