library(tidyverse)
# CAS can be like xxx-xx-x, but converted to CAS:xxxxxx
load("data/dashCASGross2025.rda")
# Has a OtherChar - mapping ChemCode (like aquocode) to now substance_key
OldOtherChar <- attr(Gross2025$Gross2025, which = "OtherChar")
# update CAS, substance_key
Gross2025$Gross2025 <- Gross2025$Gross2025 |>
  rename(CASdash = CAS) |>
  mutate(substance_key = paste("CAS", gsub("-","",CASdash), sep = ":") )

# also update OtherChar accordingly, but first add CASdash like code to OtherChar
OldCASdash <- data.frame(
  ChemCode = Gross2025$Gross2025 |>
    filter(!is.na(Gross2025$Gross2025$CASdash)) |>
    pull(CASdash)
)
OldCASdash$substance_key <- paste("CAS", gsub("-","", OldCASdash$ChemCode), sep = ":")

OtherChar <- OldOtherChar |>
  mutate(substance_key = gsub("-","",substance_key)) |>
  bind_rows(OldCASdash)

attr(Gross2025$Gross2025, which = "OtherChar") <- OtherChar
# not  usethis::use_data(Gross2025, overwrite = T); gives error when making SSDplusList
saveRDS(Gross2025$Gross2025, file = "data/Gross2025.RDS")
