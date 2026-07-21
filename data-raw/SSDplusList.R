# make SSDplusList, the list of SSD datasets available to this version of the package
data(Gross2025)
data(SSD2026beta)
SSDplusList <- c(
  Gross2025,
  SSD2026beta
)
usethis::use_data(SSDplusList, overwrite = T)
