# SSDbeta
# init
library(tidyverse)

path2MLecoTox = path.expand("~/projects/MLecoTox")
path2stowaSSD <- file.path(path2MLecoTox, "stowaSSDupdate")
if (!"package:ToxBox" %in% search()) {
  devtools::load_all(file.path(path2MLecoTox,"ToxBox"))
}

#' include constants if not already
if (!exists("READDIRECTORY")) {
  source (file.path(path2stowaSSD,"scripts/CONST.R"))
}

path2ssd <- file.path(BASEDIR, "analysis/stowa/results/tax_class_5_II_d")
list2env(
  env_cached_source(script_path = file.path(path2stowaSSD,"scripts/graphs/processd.R"),
                    getvars = c("ssd_mu_sigma")),
  envir = .GlobalEnv
)

if(!exists("db")){
  db <- PostgreSQLdb$new(
    connection_param = list(
      host = MLecoTox_SERVER,
      dbname = MLecoTox_DATABASE
    ),
    project_schema = PROJECT_SCHEMA
  )
  message("variable db created to access postgresql")
} else {
  if("PostgreSQLdb" %in% class(db)) stop("db is not a PostgreSQLdb (R6) object")
}

# connect to the database
db$connect()

# add the Replace / OtherChar
mapTable = db$tbl_lazy("translation_tbl") |>
  filter(SSDid %in% ssd_mu_sigma$substance_key) |>
  rename(ChemCode = id_value, substance_key = SSDid) |>
  select(ChemCode, substance_key) |>
  collect()

SSD2026beta <- ssd_mu_sigma
attr(SSD2026beta, which = "OtherChar") <- mapTable

SSD2026beta <- list(SSD2026beta = SSD2026beta)
# usethis::use_data(SSD2026beta, overwrite = T)
saveRDS(SSD2026beta, file = "data/SSD2026beta.RDS")
