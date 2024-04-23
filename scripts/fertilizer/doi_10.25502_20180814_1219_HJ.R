# R script for "carob"

carob_script <- function(path) {
  
#The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)
  
  uri <- "doi:10.25502/20180814/1219/HJ"
  group <- "fertilizer"
  
  ff <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=0),
    project= "AfSIS", 
    #data_citation="Huising, J. (2018). Africa Soil Information System - Phase 1, Sidindi LR [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1446/HJ",
    publication = "doi:10.1016/j.agee.2016.05.012",
    data_institutions = "IITA",
    data_type="Multi-location trials",
    carob_contributor = "Cedric Ngakou",
    carob_date="2023-02-15"
  )

  ## read data 

  ffield <- ff[basename(ff) == "Kontela_DT2009_field.csv"]
  fplant <- ff[basename(ff) == "Kontela_DT2009_Plant.csv"]
  fplot <- ff[basename(ff) == "Kontela_DT2009_Plot.csv"]
  
  field <- read.csv(ffield, na.strings = "")
  plot <- read.csv(fplot, na.strings = "")
  plant <- read.csv(fplant, na.strings = "")
  plant <- aggregate(Plant.height..cm. ~ Field + Plot, data = plant, FUN = function(x) mean(x, na.rm = TRUE))
  r <- merge(plot, field, by = "Field")
  r <- merge(r, plant, by = c("Field", "Plot"))
  
  d <- data.frame(
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = FALSE,
    treatment = r$TrtDesc,
    rep = r$Rep,
    country = "Mali",
    adm1 = "Kontela",
    location = trimws(r$Site.x),
    site = trimws(gsub("Ž", "", r$Village)),
    longitude = r$Flong,
    latitude = r$Flat,
    crop = trimws(tolower(r$TCrop)),
    variety = r$TCVariety,
    trial_id = gsub("Kont2010", "", r$Field),
    yield_part = "grain",
    plant_height = round(r$Plant.height..cm., 2),
	# Fresh weight at 15% moisture
	yield = 1000 * r$TGrainYld / 0.85
  )

	d$residue_yield <- r$TStoverFW / r$Harea
	d$dmy_residue <- r$TStoverYld * 1000
  
  # Fertilizers according doi:10.1016/j.agee.2016.05.012
  # "NPK", "NPK+Lime", "NPK+Manure", "NPK+MN", "PK", "NK", "NP"
  d$N_fertilizer <- d$K_fertilizer <- d$P_fertilizer <- 0
  d$N_fertilizer[grep("N", r$TrtDesc)] <- 100
  d$K_fertilizer[grep("K", r$TrtDesc)] <- 60
  d$Ca_fertilizer <- d$Mg_fertilizer <- d$S_fertilizer <- d$Zn_fertilizer <- 0
  d$P_fertilizer[grep("P", r$TrtDesc)] <- 30
  d$Ca_fertilizer[r$TrtDesc == "NPK+MN"] <- 10
  d$Mg_fertilizer[r$TrtDesc == "NPK+MN"] <- 5
  d$S_fertilizer[r$TrtDesc == "NPK+MN"] <- 5
  d$Zn_fertilizer[r$TrtDesc == "NPK+MN"] <- 3
  d$lime <- 0
  d$lime[r$TrtDesc == "NPK+Lime"] <- 500
  d$OM_amount <- 0
  d$OM_amount[r$TrtDesc == "NPK+Manure"] <- 10 * 1000
  d$OM_used <- d$OM_amount == 500
  d$OM_type <- NA
  d$OM_type[r$TrtDesc == "NPK+Manure"] <- "farmyard manure"
  

  # using _mac (macronutrients?) dates as they correspond to publication information
  d$planting_date <- as.character(as.Date(r$PlntDa_mac, "%m/%d/%Y"))
  d$harvest_date <- as.character(as.Date(r$HarvDa_mac, "%m/%d/%Y"))
  # Correcting years to 2009
  r$EmDate <- gsub("2017", "2009", r$EmergDt)
  d$emergence_date <- as.character(as.Date(r$EmDate))
  
  d <- d[!is.na(d$yield), ]
  carobiner::write_files(dataset = dset, records = d, path = path)
}


