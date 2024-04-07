# R script for "carob"

carob_script <- function(path) {
  
  "Description: Survey at multiple locations in Tanzania (600 to 2100 m) to establish baseline yields at farm level. (2016)"
  
  uri <- "hdl:11529/10548220"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "crop_cuts"
  ff  <- carobiner::get_data(uri, path, group)
  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=2, minor=0),
    project=NA,
    publication=NA,
    data_institutions = "CIMMYT",
    data_type="survey", 
    carob_contributor="Fredy Chimire",
    carob_date="2024-04-06"
  )
  
  # read data
  f <- ff[basename(ff) == "TZ_TAMASA_APS_2016_Yield_MetaData.xlsx"]
  r <- readxl::read_excel(f,sheet = "Corrected-Raw-Data",n_max=1840)
  # select columns related to carob columns
  d <- data.frame(country=r$Country,crop="maize", yield_part="grain",yield= as.numeric(r$`Grain yield (kg/ha@12.5%)`), 
                  adm1 = r$Zone,adm2=r$Region,adm3=r$District,adm4=r$Ward,location=r$Village,latitude= r$Latitude,
                  longitude=r$Longitude)
  
  
  #### about the data 
  
  d$dataset_id <- dataset_id
  d$is_survey <- TRUE
  d$planting_date <- "2016"
  d$striga_trial <- FALSE
  d$borer_trial <- FALSE
  d$striga_infected <- FALSE
  d <- d[!is.na(d$yield), ]
  d$trial_id <- as.character(1:nrow(d))
  
  carobiner::write_files(dset, d, path=path)
}
