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
  source (file.path(path2stowaSSD,"scripts/init.R"))
  db$connect()
}

path2ssd <- file.path(BASEDIR, "analysis/stowa/results/STOWAclass_2_5")
list2env(
  env_cached_source(script_path = file.path(path2stowaSSD,"scripts/graphs/processd.R"),
                    getvars = c("ssd_mu_sigma", "ssd_mu_sigma_inclions", "AquoMissingMapping")),
  envir = .GlobalEnv
)

if (nrow(AquoMissingMapping) > 0){
  missingAquo <- do.call(paste, as.list(AquoMissingMapping$AquoCode))
  warning(paste("missing aquos present in ESF2:"))
  if (F) {
    # try match unmapped aquocodes
    QSAR_BASE_URL <- Sys.getenv("QSAR_BASE_URL", unset = "http://rivm-qsars-w01p/")
    QSAR_SWAGGER_PATH <- Sys.getenv("QSAR_SWAGGER_PATH", unset = "/swagger/v6/swagger.json")
    OECDtb <- QSARToolboxAPI$new(base_url = QSAR_BASE_URL, swagger_path = QSAR_SWAGGER_PATH)
    pb <- txtProgressBar(
      min   = 0,
      max   = nrow(AquoMissingMapping),
      style = 3
    )
    results_list_ec = list()
    idx = 1
    for (aq_i in 1:nrow(AquoMissingMapping)) {
      # aq_i = 1

      res <- tryCatch(
        OECDtb$do(
          endpoint        = "/api/v6/search/cas/{cas}/{ignoreStereo}",
          method          = "GET",
          registerUnknown = TRUE,
          ignoreStereo    = FALSE,
          cas        = gsub("-","",AquoMissingMapping$CASnummer[aq_i])
        ),
        error = function(e) {
          message("Error for EC ", aq_i, ": ", conditionMessage(e))
          NULL
        }
      )

      if (!is.null(res)) {
        res_df <- as.data.frame(res)
        results_list_ec[[idx]] <- res_df
        idx <- idx + 1
      }

      setTxtProgressBar(pb, aq_i)
    }
    close(pb)

    # TODO  reverse lookup chemId, Smiles?, test for other preferred CAS
  }
}

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
  if(!"PostgreSQLdb" %in% class(db)) stop("db is not a PostgreSQLdb (R6) object")
}

# connect to the database
db$connect()

# add the Replace / OtherChar
mapTable = db$tbl_lazy("translation_tbl") |>
  filter(!is.na(id_value) & nchar(id_value) > 1 & SSDid %in% ssd_mu_sigma$substance_key) |>
  rename(ChemCode = id_value, substance_key = SSDid) |>
  select(ChemCode, substance_key) |>
  collect()

include_ions <- TRUE
if(include_ions){
  SSD2026beta <- ssd_mu_sigma_inclions |>
    mutate(lgKoc = log10(Koc)) |> select(-Koc)
} else (
  SSD2026beta <- ssd_mu_sigma |>
    mutate(lgKoc = log10(Koc)) |> select(-Koc)
)
attr(SSD2026beta, which = "OtherChar") <- mapTable

# usethis::use_data(SSD2026beta, overwrite = T)
saveRDS(SSD2026beta, file = "data/SSD2026beta.RDS")
