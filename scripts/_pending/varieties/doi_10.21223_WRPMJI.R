# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:

    [copy the abstract from the repo]

"
  
  uri <- "doi:10.21223/WRPMJI"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "varieties"
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::read_metadata(uri, path, group, major=7, minor=0)
  
  
  ## process file(s)
  
  files <- list.files(file.path(paste0(path,"/data/raw/varieties/",dataset_id,"/")), pattern = "Result", full.names = T)
  cat("These are the available files for the doi link:\n\n")
  print(files)
  
  d <- read.csv(files, sep = ";")
  
  ## required variabeles for metadata
  
  metadata_requirements <- read.csv("https://raw.githubusercontent.com/reagro/terminag/main/variables/variables_metadata.csv", sep = ",")
  metadata_requirements <- metadata_requirements[which(metadata_requirements$required=="yes"),]
  
  
  ## required variables for records
  
  crop_requirements <- read.csv("https://raw.githubusercontent.com/reagro/terminag/main/variables/variables_crop.csv", sep = ",")
  crop_requirements <- crop_requirements[which(crop_requirements$required=="yes"),]
  
  ## filling metadata 
  
  missing_metadata_idx <- which(!metadata_requirements$name %in% colnames(js))
  
  missing_metadata_vars <- metadata_requirements[missing_metadata_idx,]$name
  
  print(missing_metadata_vars)
  
  fill_metadata <- c("Henry Juarez","experiment", "treatment", "AUDPC, TTW, MTW", NA,"CIP",NA,Sys.Date() |> as.character())
  
  data_vector <- setNames(fill_metadata, missing_metadata_vars)
  
  js <- cbind(js, data_vector |> 
                as.data.frame() |> 
                t())
  ## filling records
  
  d$AUDPC <- d$AUDPC / 100
  
  d$yield = d$TTW * 44444 ### rows=0.77m and plans = 0.3m = 44,444 plants/Ha.  yield = TTW (Total tuber weight (TTW) x 44,444
  
  
  d$yield_marketable <- d$MTW * 44444 ### rows=0.77m and plans = 0.3m = 44,444 plants/Ha.  yield = TTW (Total tuber weight (TTW) x 44,444
  
  d$N_fertilizer = 113.8 # Total N aplicado: 113.18 kg de N/ha (NPS = 20-20-0 y Urea = 46-0-0) 
  # Total P aplicado: 20.7 kg de P/ha (NPS = 20-20-0 y Urea = 46-0-0)  
  
  d$P_fertilizer = 20.7
  
  d$K_fertilizer = 0
  
  
  idx <- which(colnames(d) %in% c("Clone_Name"))
  
  colnames(d)[idx] <- c("variety")
  
  # TODO check the missing variables in records and do the respective code to fill those columns
  
  missing_data_idx <-  which(!crop_requirements$name %in% colnames(d))
  
  missing_data_vars <- crop_requirements[missing_data_idx,]$name
  
  print(missing_data_vars)
  
  idx_vars <- which(colnames(d) %in% crop_requirements$name)
  
  d <- d[,c(idx_vars,idx)]
  
  d$is_survey = FALSE
  
  d$yield_part <- "tubers"
  
  d$irrigated <- NA
  
  d$planting_date <- "2018-01-01"
  
  d$treatment = "Field book for the LBHT x LTVR clones yield evaluation in 2018 growing season at Holetta, Ethiopia. The dataset includes data about 72 potato clones. The experiment was created with an Alpha-lattice design (9*8), with 2 replication, over a 16x32 m area. The plants were fertilized with NPS (237 kg/ha) and urea (143 Kg/ha). Each plot contains 1 row, for a total of 10 plants. Row length is 3 m, spaced 0.75 m between rows and 0.3 m within rows."
  
  d$location <- "Holetta"
  
  d$country = "Ethiopia"
  
  d$latitude = 9.063617159041524 # Manual georeferencing near Holetta, Ethiopia. 
  
  d$longitude = 38.492574439594755 # Manual georeferencing near Holetta, Ethiopia. 
  
  d$geo_from_source <- TRUE
  
  d$trial_id <- "1"
  
  d$on_farm <- TRUE
  
  d$crop = "potato"
  
  
  
  
  carobiner::write_files(path = path,
                         metadata = js,
                         records = d)
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
