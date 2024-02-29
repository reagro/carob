# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel 
   sites across sub-Saharan Africa using the Land Degradation
   Surveillance framework and included also multi-location diagnostic
   trials in selected sentinel sites to determine nutrient limitations
   and response to improved soil management practices (soil amendments)
"
  
  uri <- "doi:10.25502/20180814/1219/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project= "AfSIS", # This needs to be the project the data is comming from (AfSIS)
    uri=uri,
    publication = "doi:10.1016/j.agee.2016.05.012",
    data_institutions = "International Institute of Tropical Agriculture (IITA)",
    data_type="Multi-location diagnostic trials",
    data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Kontela [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1219/HJ",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-02-15",
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
  
  
  f1 <- ff[basename(ff) == "Kontela_DT2009_field.csv"]# get the field data
  f2 <- ff[basename(ff) == "Kontela_DT2009_Plant.csv"] # get the plant data
  f3 <- ff[basename(ff) == "Kontela_DT2009_Plot.csv"] # get the plot data 
  
  
  field <- read.csv(f1)
  plant <- read.csv(f2)
  plant <- aggregate(Plant.height..cm. ~ Field + Plot, data = plant, 
                     FUN = function(x) mean(x, na.rm = TRUE))
  plot <- read.csv(f3)
  d <- merge(plot, field, by = "Field")
  d <- merge(d, plant, by = c("Field", "Plot"))

  # process file(s)
  
  # process field data
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  ## the treatment code	
  d$treatment <- d$TrtDesc
  d$trial_id <- gsub("Kont2010", "", d$FieldID.x)
  d$rep <- d$Rep
  
  ##### Location #####
  d$country <- "Mali"
  d$adm1 <- "Kontela"
  d$location <- trimws(gsub("Ž", "", d$Village))
  d$site <- trimws(gsub("Ž", "", d$Site.x))
  d$longitude <- as.numeric(d$Flong)
  d$latitude <- as.numeric(d$Flat)
  
  ##### Crop #####
  d$crop <- trimws(tolower(d$TCrop))
  d$variety <- d$TCVariety
  
  ##### Time #####
  sd <- data.frame(do.call(rbind, strsplit(as.character(d$PlntDa),'/')))
  sd$day <- as.integer(sd$X2)
  sd$month <- as.integer(sd$X1)
  sd$year <- as.integer(2009) # As indicated in the publication
  d$planting_date <- as.character(as.Date(paste(sd$year, sd$month, sd$day, sep = "-")))
  hd <- data.frame(do.call(rbind, strsplit(as.character(ifelse(d$HarvDa == "", NA, d$HarvDa)),'/')))
  hd$day <- as.integer(hd$X2)
  hd$month <- as.integer(hd$X1)
  hd$year <- as.integer(2009) # According to the publication the year was 2010
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
  d$yield_part <- "grain"
  d$dmy_total <- (as.numeric(d$TStoverYld) + as.numeric(d$TGrainYld))*1000
  d$yield <- as.numeric(d$TCobFW) # Seems to be in kg/ha already (??)
  d$dmy_residue <- d$dmy_total - as.numeric(d$TGrainYld)*1000
  
  d$EmDate <- gsub("2017", "2009", d$EmergDt) # Correcting years to 2009
  d$emergence <- as.numeric(as.Date(d$EmDate) - as.Date(d$planting_date))
  d$plant_height <- round(d$Plant.height..cm., 2)
  
  d <- d[,c("dataset_id", "trial_id", "rep", "on_farm", "is_survey",
            "country", "adm1", "location", "site", "longitude", "latitude",
            "treatment", "crop", "variety", "planting_date", "harvest_date",
            "P_fertilizer", "K_fertilizer", "N_fertilizer", "fertilizer_type",
            "Ca_fertilizer", "Mg_fertilizer", "S_fertilizer", "Zn_fertilizer",
            "OM_used", "OM_type", "OM_amount", "lime",
            "yield_part", "dmy_total", "yield", "dmy_residue",
            "irrigated", "emergence", "plant_height")]
 
  carobiner::write_files(dataset = dset, records = d, path = path)
}

