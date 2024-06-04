# R script for "carob"

## ISSUES
# 1. Planting date assigned 2014 because the cropping season was 2014/15.
# 2. 


carob_script <- function(path) {
  
  "
Replicated crop-cuts and related agronomy data for maize from 142 farmers' fields
in Southern, Eastern and Northern Zones in 2014/2015 season. The soils data from
these fields can be found here. (2017-12-17)
"
  
  #### Identifiers
  uri <- "hdl:11529/10548143"
  group <- "crop_cuts"
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ##### dataset level metadata 
  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=2, minor=1),
    data_institute = "CIMMYT",
    publication =NA,
    project = NA,
    data_type = "on-farm experiment",
    treatment_vars = "yield",
    carob_contributor = "Blessing Dzuda",
    carob_date = "2024-06-04"
  )
  
  ##### PROCESS data records
  
  # read data 
  
  f <- ff[basename(ff) == "TAMASA_TZ_CC_Yield_2015.xlsx"]
  r <- carobiner::read.excel(f, sheet = "Data")
  r <- r[1:425,1:27]
  
  ## process file(s)
  
  ## select the variables of interest and assign them to the correct name
  d <- data.frame(
    trial_id="1",
    country=r$Country,
    adm1=r$Zone,
    adm2=r$District,
    adm3=r$Village,
    adm4=r$Ward,
    latitude=r$Latitude,
    longitude=r$Longitude,
    elevation=r$Altitude,
    planting_date="2014",
    crop="maize",
    plant_density=r$`Plant Stands`,
    yield=r$`Grain yield (kg/ha@12.5%)`
  )
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$yield_part <- "grain"
  d$plant_density <- d$plant_density*400
  
  # all scripts must end like this
  carobiner::write_files(path, dset, d)
}


