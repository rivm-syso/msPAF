# make SSDplusList, the list of SSD datasets available to this version of the package
Gross2025 <- readRDS("data/Gross2025.RDS")
SSD2026beta <- readRDS("data/SSD2026beta.RDS")
SSDplusList <- c(
  Gross2025,
  SSD2026beta
)
usethis::use_data(SSDplusList, overwrite = T)
