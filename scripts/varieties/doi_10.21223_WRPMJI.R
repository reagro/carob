# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
    "Field book for the LBHT x LTVR clones yield evaluation in 2018 growing season at Holetta, Ethiopia. The dataset includes data about 72 potato clones. The experiment was created with an Alpha-lattice design (9*8), with 2 replication, over a 16x32 m area. The plants were fertilized with NPS (237 kg/ha) and urea (143 Kg/ha). Each plot contains 1 row, for a total of 10 plants. Row length is 3 m, spaced 0.75 m between rows and 0.3 m within rows."
  
  uri <- "doi:10.21223/WRPMJI"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=7, minor=0),
      data_institute = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  f <- ff[grep("Result", basename(ff))]
  d <- read.csv(f, sep = ";")
  d$AUDPC <- d$AUDPC / 100
  d$yield = d$TTW * 44444
  d$yield_marketable <- d$MTW * 44444
  d$N_fertilizer <-  113.8
  d$P_fertilizer <-  20.7
  d$K_fertilizer <-  0
  d <- carobiner::change_names(d,'Clone_Name','variety')
  d$is_survey <-  FALSE
  d$yield_part <- "tubers"
  d$irrigated <- NA
  d$planting_date <- "2018-01-01"
  d$location <- "Holetta"
  d$country <-  "Ethiopia"
  d$latitude <-  9.063617159041524 
  d$longitude <-  38.492574439594755
  d$geo_from_source <- TRUE
  d$trial_id <- "1"
  d$on_farm <- TRUE
  d$crop <- "potato"
  idx <- which(colnames(d) %in% c('TTW', 'MTW', 'ATW', 'AMTW', 'Selections', 'Notes'))
  d <- d[,-idx]
  
  
  carobiner::write_files(path = path,
                         metadata = meta,
                         records = d)
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
