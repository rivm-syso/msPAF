# convert gross2023 to esf3.0 format:
#  AquoCode and CAS to
library(tidyverse)
load("data/Gross.rda")
load("data/OtherChar.rda")
# toshort <- (nchar(SSDplusListRDS[["Gross2023"]]$CAS)) < 7
# SSDplusListRDS[["Gross2023"]][toshort, c("AquoCode", "ABCquality", "KocNew", "Replace.fotoNL")]
# gives NA (to prevent errors, i guess) and       AquoCode ABCquality KocNew Replace.fotoNL
#  38821  sNH3NH4          B     NA            NH3
# Rule: NH4+ is measured or the main constituent, but HN3 is the toxicant.
# This is handled by Calc_HU; It's part of Otherchar2023
# TODO sync with download aquocodes from info-huis water

# these replaces were to other aquocode, now to substance_key
Replace2023 <- Gross %>%
  filter(!is.na(Replace.fotoNL)) %>%
  select(AquoCode, Replace.fotoNL) %>%
  left_join(Gross %>%
              select(CAS, AquoCode) %>%
              rename(Replace.fotoNL = AquoCode))
# exception
Replace2023$CAS[Replace2023$Replace.fotoNL == "'25586-43-1"] <- "25586-43-1"
Replace2023 <- mutate(Replace2023, substance_key = paste("CAS", CAS, sep = ":")) #%>%
  # Not substitute NH4, sNH3NH4 for NH3, using pKa in calc_HU
  # Replace2023[Replace2023$substance_key == "CAS:7664-41-7",]
  # distinct() %>% filter(!(substance_key == "CAS:7664-41-7"))

OtherChar <- unique(OtherChar[!OtherChar$ChemCode %in% Replace2023$AquoCode, ])
OtherChar <- OtherChar %>%
  mutate(substance_key = paste("CAS", CAS, sep = ":"))

OtherChar <- rbind(Replace2023 %>%
                     rename(ChemCode = AquoCode) %>%
                     select(ChemCode, substance_key),
                   OtherChar %>% select(ChemCode, substance_key)
                  )
# create a new list with SSD and attribute OtherChar
SSD2025 <- Gross
attr(SSD2025, which = "OtherChar") <- OtherChar
Gross2025 <- list(Gross2025 = SSD2025)
usethis::use_data(Gross2025, overwrite = T)
