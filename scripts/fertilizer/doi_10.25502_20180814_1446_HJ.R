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
    project= "AfSIS", # This needs to be the project the data is comming from (AfSIS)
    uri=uri,
    data_citation="Huising, J. (2018). Africa Soil Information System - Phase 1, Sidindi LR [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1446/HJ",
    publication = "doi:10.1016/j.agee.2016.05.012",
    data_institutions = "International Institute of Tropical Agriculture (IITA)",
    data_type="Multi-location diagnostic trials",
    carob_contributor = "Eduardo Garcia Bendito",
    carob_date="2023-07-27",
    revised_by = "Eduardo Garcia Bendito",
    revision_date = "2024-02-28"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
  
  
  field <- ff[basename(ff) == "Sidindi_LR2010_Field.csv"]
  plant <- ff[basename(ff) == "Sidindi_LR2010_plant.csv"]
  plot <- ff[basename(ff) == "Sidindi_LR2010_Plot.csv"]

  field <- read.csv(field)
  plant <- read.csv(plant)
  plant <- aggregate(Plant.height..cm. ~ FieldID + PlotID, data = plant, 
                     FUN = function(x) mean(x, na.rm = TRUE))
  plot <- read.csv(plot)
  d <- merge(plot, field, by = "FieldID")
  d <- merge(d, plant, by = c("FieldID", "PlotID"))
  
  ## process file(s)
  
  #### about the data #####

  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  ## the treatment code	
  d$treatment <- d$TrtDesc
  d$trial_id <- gsub("Sidi2010a", "", d$FieldID)
  d$rep <- d$Rep
  
  ##### Location #####
  d$country <- "Kenya"
  d$adm1 <- "Siaya"
  d$location <- d$Site.x
  d$site <- trimws(d$Village)
  d$longitude <- d$Flong
  d$latitude <- d$Flat
  
  ##### Crop #####
  d$crop <- trimws(tolower(d$TCrop))
  d$variety <- d$TCVariety
  
  ##### Time #####
  sd <- data.frame(do.call(rbind, strsplit(as.character(d$PlntDa),'/')))
  sd$day <- as.integer(ifelse(as.integer(sd$X2)>12, sd$X2, sd$X1))
  sd$month <- as.integer(4) # Assuming planting in May is an error since all emergence dates are in April
  sd$year <- as.integer(2010) # According to the publication the year was 2010
  d$planting_date <- as.character(as.Date(paste(sd$year, sd$month, sd$day, sep = "-")))
  hd <- data.frame(do.call(rbind, strsplit(as.character(ifelse(d$HarvDa == "", NA, d$HarvDa)),'/')))
  hd$day <- as.integer(ifelse(as.integer(hd$X2)>12, hd$X2, hd$X1))
  hd$month <- as.integer(ifelse(as.integer(hd$X2)>12, hd$X1, hd$X2))
  hd$year <- as.integer(2010) # According to the publication the year was 2010
  d$harvest_date <- as.character(as.Date(paste(hd$year, hd$month, hd$day, sep = "-")))
  
  ##### Fertilizers #####
  d$P_fertilizer <- 0
  d$P_fertilizer[d$TrtDesc %in% c("NPK", "NPK+Lime", "NPK+Manure", "NPK+MN", "NP", "PK")] <- as.numeric(30)
  d$K_fertilizer <- 0
  d$K_fertilizer[d$TrtDesc %in% c("NPK", "NPK+Lime", "NPK+Manure", "NPK+MN", "PK", "NK")] <- as.numeric(60)
  d$N_fertilizer <- 0
  d$N_fertilizer[d$TrtDesc %in% c("NPK", "NPK+Lime", "NPK+Manure", "NPK+MN", "NP", "NK")] <- as.numeric(100)
  d$Ca_fertilizer <- 0
  d$Ca_fertilizer[d$TrtDesc %in% c("NPK+MN")] <- as.numeric(10)
  d$Mg_fertilizer <- 0
  d$Mg_fertilizer[d$TrtDesc %in% c("NPK+MN")] <- as.numeric(5)
  d$S_fertilizer <- 0
  d$S_fertilizer[d$TrtDesc %in% c("NPK+MN")] <- as.numeric(5)
  d$Zn_fertilizer <- 0
  d$Zn_fertilizer[d$TrtDesc %in% c("NPK+MN")] <- as.numeric(3)
  d$lime <- 0
  d$lime[d$TrtDesc == "NPK+Lime"] <- as.numeric(10)*1000 # t/ha -> kg/ha
  d$OM_amount <- 0
  d$OM_amount[d$TrtDesc == "NPK+Manure"] <- as.numeric(500)
  ## normalize names 
  d$fertilizer_type <- NA
  d$fertilizer_type[d$FType1 != ""] <- "DAP"
  d$fertilizer_type[d$FType2 != ""] <- "urea"
  d$fertilizer_type[d$FType1 != "" & d$FType2 != ""] <- paste0("DAP; urea")
  d$OM_used <- TRUE
  d$OM_type <- NA
  d$OM_type[d$TrtDesc == "NPK+Manure"] <- "farmyard manure"

  ##### Yield #####
  #what plant part does yield refer to?
  d$yield_part <- "cobs"
  d$dmy_total <- as.numeric(d$TStoverYld) + as.numeric(d$TGrainYld)
  d$yield <- as.numeric(d$CobFW)/(as.numeric(d$Harea)/10000) # kg/m2 -> kg/ha
  d$grain_weight <- d$X100GrainDW*10 # Adjusting to 1000 grains
  d$dmy_residue <- d$dmy_total - as.numeric(d$TGrainYld)
  d$crop_rotation <- trimws(tolower(d$PCrop1))
  d$crop_rotation[d$crop_rotation == "sweet potato"] <- "sweetpotato"
  d$crop_rotation[d$crop_rotation %in% c("beans", "maize, beans")] <- "common bean"
  d$crop_rotation[d$crop_rotation %in% c("beans", "maize, beans")] <- "common bean"
  
  d$EmDate <- gsub("2011", "2010", d$EmDate) # Correcting years to 2010
  d$emergence <- as.numeric(as.Date(d$EmDate, "%d/%m/%Y") - as.Date(d$planting_date))
  d$plant_height <- round(d$Plant.height..cm., 2)
  
  
  d <- d[,c("dataset_id", "trial_id", "rep", "on_farm", "is_survey",
            "country", "adm1", "location", "site", "longitude", "latitude",
            "treatment", "crop", "variety", "planting_date", "harvest_date",
            "P_fertilizer", "K_fertilizer", "N_fertilizer", "fertilizer_type",
            "Ca_fertilizer", "Mg_fertilizer", "S_fertilizer", "Zn_fertilizer",
            "OM_used", "OM_type", "OM_amount", "lime",
            "yield_part", "dmy_total", "yield", "grain_weight", "dmy_residue",
            "irrigated", "crop_rotation", "emergence", "plant_height")]
  
  d <- d[!is.na(d$yield), ]

    
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
}
