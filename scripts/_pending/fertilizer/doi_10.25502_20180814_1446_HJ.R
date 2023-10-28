# R script for "carob"

## ISSUES
# ....

# yield is not the correct field (see yield and grain yield)
# fertilizer amounts should be provided



carob_script <- function(path) {
  
  "Description:
    The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across 
    sub-Saharan Africa using the Land Degradation Surveilllance framework and inlcuded also multi-location diagnostic 
    trials in selected sentiale sites to determine nutrient limitations and response to improved soil management practices 
    (soil amendments).
"
  
  uri <- "doi:10.25502/20180814/1446/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer" # This needs to go one of the groups in https://github.com/reagro/carob/blob/master/terms/groups.csv
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA, # This needs to be the project the data is comming from (AfSIS)
    uri=uri,
    data_citation="doi:10.25502/20180814/1446/HJ",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA, # This should be the simplified version of the DOI using the carobiner::simple_uri("https://doi.org/10.1016/j.agee.2016.05.012"). Then you also need to add the RIS file into the references folder of your carob repository
    data_institutions = "International Institute of Tropical Agriculture (IITA)",
    data_type="Multi-location diagnostic trials", # Here use one of the examples: "on-farm experiment", "survey", "compilation"
    carob_contributor="Haile Okuku"
    # Please add the coplete metadata fields as in https://github.com/reagro/carob/blob/master/terms/dataset.csv
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  field <- ff[basename(ff) == "Sidindi_LR2010_Field.csv"]
  plant <- ff[basename(ff) == "Sidindi_LR2010_plant.csv"]
  plot <- ff[basename(ff) == "Sidindi_LR2010_Plot.csv"]

  field <- read.csv(field)
  plant <- read.csv(plant)
  plot <- read.csv(plot)
  d <- merge(plot, field, by = "FieldID")
  
  ## process file(s)
  
  ## use a subset
  # d <- carobiner::change_names(r, from, to)
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  ## the treatment code	
  d$treatment <- NA
  
  ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- "Kenya"
  d$adm1 <- "Siaya"
  d$location <- d$Site.x
  d$site <- trimws(d$Village)
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d$longitude <- d$Flong
  d$latitude <- d$Flat
  
  ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d$crop <- trimws(tolower(d$TCrop))
  d$variety <- d$TCVariety
  
  ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  sd <- data.frame(do.call(rbind, strsplit(as.character(d$PlntDa),'/')))
  sd$day <- as.integer(ifelse(as.integer(sd$X2)>12, sd$X2, sd$X1))
  sd$month <- as.integer(ifelse(as.integer(sd$X2)>12, sd$X1, sd$X2))
  sd$year <- as.integer(sd$X3)
  d$planting_date <- as.character(as.Date(paste(sd$year, sd$month, sd$day, sep = "-")))
  hd <- data.frame(do.call(rbind, strsplit(as.character(ifelse(d$HarvDa == "", NA, d$HarvDa)),'/')))
  hd$day <- as.integer(ifelse(as.integer(hd$X2)>12, hd$X2, hd$X1))
  hd$month <- as.integer(ifelse(as.integer(hd$X2)>12, hd$X1, hd$X2))
  hd$year <- as.integer(hd$X3)
  d$harvest_date <- as.character(as.Date(paste(hd$year, hd$month, hd$day, sep = "-")))
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  d$N_fertilizer <- 0
  ## normalize names 
  d$fertilizer_type <- NA
  d$fertilizer_type[d$FType1 != ""] <- "DAP"
  d$fertilizer_type[d$FType2 != ""] <- "urea"
  d$fertilizer_type[d$FType1 != "" & d$FType2 != ""] <- paste0("DAP; urea")

  ##### in general, add comments to your script if computations are
  ##### based on information gleaned from metadata, a publication, 
  ##### or when they are not immediately obvious for other reasons
  
  ##### Yield #####
  #what plant part does yield refer to?
  d$yield_part <- NA # should be cobs
  d$biomass_total <- d$TStoverYld + d$TGrainYld
  d$yield <- d$CobFW
  
  d <- d[,c("dataset_id", "on_farm", "is_survey", "irrigated", "treatment", "country", "adm1", "location", "site", "longitude", "latitude", "crop", "variety", "planting_date", "harvest_date", "P_fertilizer", "K_fertilizer", "N_fertilizer", "fertilizer_type", "yield_part", "biomass_total", "yield")]

    
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}
