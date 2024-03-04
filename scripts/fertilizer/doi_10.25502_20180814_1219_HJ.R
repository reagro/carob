# R script for "carob"

## ISSUES
# many mistakes
# see doi_10.25502_20180814_1446_HJ.R
# for a cleaner case


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
  group <- "fertilizer"
  
  dataset_id <- carobiner::simple_uri(uri)
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  
  dset <- data.frame(
    carobiner::extract_metadata(js, uri, group=group),
    project= "AfSIS", 
    data_citation="Huising, J. (2018). Africa Soil Information System - Phase 1, Sidindi LR [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1446/HJ",
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
    dataset_id=dataset_id,
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
    dmy_storage = r$TCobFW * 1.17
  )
  
  # fresh weight
  d$grain_weight <- r$TGrainYld * 1000
  d$yield <- d$dmy_storage / 0.82 # Assuming ~17% moisture
  
  ##### Fertilizers #####
  # according to https://www.sciencedirect.com/science/article/pii/S0167880916302584
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
  
  d$residue_yield <- r$TStoverYld * 1000

  ##### Time #####
  # EGB: Planting dates and harvest dates adjusted to publication information, despite disagreement with reported data
  sd <- data.frame(do.call(rbind, strsplit(as.character(r$PlntDa),'/')))
  sd$day <- as.integer(sd$X2)
  sd$month <- as.integer(sd$X1)
  sd$year <- as.integer(2009) # As indicated in the publication
  d$planting_date <- as.character(as.Date(paste(sd$year, sd$month, sd$day, sep = "-")))
  hd <- data.frame(do.call(rbind, strsplit(as.character(ifelse(r$HarvDa == "", NA, r$HarvDa)),'/')))
  hd$day <- as.integer(hd$X2)
  hd$month <- as.integer(hd$X1)
  hd$year <- as.integer(2009) # According to the publication the year was 2010
  d$harvest_date <- as.character(as.Date(paste(hd$year, hd$month, hd$day, sep = "-")))
  r$EmDate <- gsub("2017", "2009", r$EmergDt) # Correcting years to 2009
  d$emergence <- as.numeric(as.Date(r$EmDate) - as.Date(d$planting_date))

  d <- d[,c("dataset_id", "trial_id", "rep", "on_farm", "is_survey",
            "country", "adm1", "location", "site", "longitude", "latitude",
            "treatment", "crop", "variety", "planting_date", "harvest_date",
            "P_fertilizer", "K_fertilizer", "N_fertilizer",
            "Ca_fertilizer", "Mg_fertilizer", "S_fertilizer", "Zn_fertilizer",
            "OM_used", "OM_type", "OM_amount", "lime",
            "yield_part", "yield", "grain_weight", "residue_yield",
            "dmy_storage", "irrigated", "emergence", "plant_height")]
  
  d <- d[!is.na(d$yield), ]
 
  carobiner::write_files(dataset = dset, records = d, path = path)
}

