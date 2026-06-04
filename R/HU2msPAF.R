#' @title HU2msPAF
#' @name  HU2msPAF
#' @author jaap slootweg
#' @description process Hazard Units (HU) for HU_Calc2() to msPAF values for each Sample; each UseClass ..?
#' @param HU data.frame with HU with at least the columns CAS/AquoCode, SampleID, ...
#' @param TMOAname name of column for toxic mode of action = "PrimaryMoA", NO
#' switched to AquoCode to force Response Addition
#' @return data.frame with msPAF values
#' @export
HU2msPAF <- function(HU, TMOAname = "substance_key",
                      groupName = "groep.fotoNL")
{

  if (nrow(HU) == 0) return(data.frame())
  #expected column names
  stopifnot(all(c(TMOAname, groupName, "substance_key", # yes also for substance, formerly CAS
                  "HU", "Dev10Log", "Avg10Log") %in% colnames(HU)))

  AggMsPAF <- function(df, l.AggrNames){ # call for groupName or all
    #also aggregate TMOA... which defaults to substance_key resulting in Response Addition
    AggTMOAnames <- c(l.AggrNames, TMOAname)

    #1 HU 2 msPAF, mixed model
    SomHUTMOA <- df %>%
      filter(!is.na(Dev10Log)) %>%
      group_by(across(all_of(AggTMOAnames))) %>%
      summarise(HU = sum(HU, na.rm = TRUE), .groups = "drop")

    AvgDevTMOA <- df %>%
      filter(!is.na(Dev10Log)) %>%
      group_by(across(all_of(AggTMOAnames))) %>%
      summarise(Dev10Log = mean(Dev10Log, na.rm = TRUE), .groups = "drop")

    #conc addition from sum (above) + prep effect addition
    SomHUTMOA$RemainFrac <- 1 - pnorm(log10(SomHUTMOA$HU),
                                      mean = 0,
                                      sd = AvgDevTMOA$Dev10Log)

    #effect addition; loose TMOA
    AcumsPAF <- aggregate(SomHUTMOA$RemainFrac,
                          by = SomHUTMOA[,l.AggrNames,drop = F],
                          FUN = prod)
    #names(AcumsPAF)[names(AcumsPAF)=="x"] <- "RemainFrac"
    #YrGemMsPAF <- aggregate(RemainFrac~X+Y+DataSource, data = msPAF, FUN = mean)
    AcumsPAF$msPAF <- 1 - AcumsPAF$x

    return(AcumsPAF[,-which(names(AcumsPAF) == "x")])
  }

  AllNames <- names(HU)

  #first exclude groupName to obtain the total over all groups
  msPAFall <- AggMsPAF(HU, l.AggrNames = c("SampleID")) # not: groupName,

  msPAFall[,groupName] <- "All"
  #names(AcAll)[names(AcAll)=="msPAF"] <- "Acute" # ED

  if (!is.null(groupName)) { #might be set to NULL in the call of the outer function!
    #Including aggregation by groupName
    msPAFGroup <- AggMsPAF(HU, l.AggrNames = c("SampleID", groupName))

    #names(AcGroup)[names(AcGroup)=="msPAF"] <- "Acute" # ED

    #concat "All" and Chemical groups (from GroupName) and pivot to show both columns
    msPAFgrouped <- rbind(msPAFall, msPAFGroup)
  } else { #
    msPAFgrouped <- msPAFall
  }

  return(msPAFgrouped)
}
