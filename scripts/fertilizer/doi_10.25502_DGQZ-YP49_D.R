#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 african countries
#################################################################################
  

carob_script <- function(path){

  uri <- "doi.org/10.25502/DGQZ-YP49/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  #dataset level data
  
  dset <- data.frame(
  dataset_id = dataset_id,
  group = group,
  project="N2Africa",
  uri = uri,
  publication = NA,
  data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, 
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, 
  L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, 
  K., Baars, E., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - 
  Kenya, 2012 [Data set]. International Institute of Tropical Agriculture (IITA).
  https://doi.org/10.25502/DGQZ-YP49/D",
  carob_contributor = "Effie Ochieng",
  experiment_type = "fertilizer",
  has_weather =  FALSE,
  has_management = FALSE
  )
 
  # download and read data 
  
  ff <- carobiner::get_data(uri,path,group)
  js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
  dset$license <- carobiner::get_license(js) 
  
  # read the data
  f <- ff[basename(ff) == "data.csv"]
  d <- read.csv(f)
  
  f1 <- ff[basename(ff) == "general.csv"]
  d1 <- read.csv(f1)
  
  f2 <- ff[basename(ff) == "soil_properties.csv"]
  d2 <- read.csv(f2) 
  
  d$crop <- ""
  d$crop[ grep("_CB_", d$experiment_id) ] <- "common bean" # climbing
  d$crop[ grep("_BB_", d$experiment_id) ] <- "common bean" # bush
  d$crop[ grep("_SB_", d$experiment_id) ] <- "soybean"
  
  #process data sets separately identifying the variables of interest
  d$trial_id <- d$experiment_id
  d$rep <- d$replication_no
  d$on_farm <- TRUE
  d$start_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
  d$end_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
  d$treatment <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, sep = "_")
  
  #adding fertilizer information
   f <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, collapse=" ")
   f <- toupper(f)
   d$K_fertilizer <- d$P_fertilizer <- d$N_fertilizer <- 0
# this could be further simplified as the fertilizer combination is always the same.
   d$K_fertilizer[grepl("KCL", f)] <- 30
   d$P_fertilizer[grepl("TSP", f)] <- 30
   d$N_fertilizer[grepl("UREA", f)] <- 60
   d$N_splits <- 0
   d$N_splits[d$N_fertilizer > 0] <- 2
   d$inoculated <- d$sub_treatment_inoc == "Inoculated"
   d$inoculated[d$sub_treatment_inoc == "Sub-treatment (Inocu"] <- TRUE
   
   d$fertilizer_type <- ""
   d$fertilizer_type[d$N_fertilizer > 0] <- c("TSP; KCl; urea")
  
	d$yield <- d$grain_yield_ha_calc
  
  #changing the character variables to numeric using lapply
  
  i <- c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules",
         "nodule_dry_weight")
  d[, i] <- lapply(d[, i], as.numeric)
  
  
  d$biomass_roots <- d$root_dry_weight_roots_no_nodules
  d$biomass_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules +
                        d$nodule_dry_weight)
  
  #subset the processed variables
  
  d <- d[,c("trial_id","rep","on_farm","start_date","end_date","treatment","biomass_roots","biomass_total", "crop", "K_fertilizer","P_fertilizer","N_fertilizer","N_splits", "fertilizer_type", "inoculated", "yield")]
  
  
  d1$trial_id <- d1$experiment_id
  d1$country <-"Kenya"

	as <- carobiner::fix_name(sapply(strsplit(d1$action_site, "-"), \(i) i[1]), "title")
	as[as=="Marcel Olela"] <- NA
	as <- gsub(" Patrick Obwanga", "", as)
	as <- gsub(": Alice Obiero", "", as)
	ma <- carobiner::fix_name(d1$mandate_area_name, "title")
	d1$site <- paste0(as, " (", ma, ")")
	d1$longitude <- d1$gps_longitude_dec
	d1$latitude <- d1$gps_latitude_dec
	d1$elevation <- d1$gps_altitude_dec
  
  #subset the processed variables
  d1 <- d1 [, c("trial_id", "country", "site", "longitude", "latitude", "elevation")]
  
  d2$trial_id <- d2$experiment_id
  d2$soil_pH <- d2$ph
# all zero  d2$soil_K <- d2$k
  d2$soil_sand <- d2$sand
  d2$soil_clay <- d2$clay
  d2$soil_SOC <- d2$tot_carbon
  d2$soil_N <- d2$tot_nitrogen 
  
  #subset the processed variables
  d2 <- d2[, c("trial_id", "soil_pH", "soil_sand","soil_clay","soil_SOC","soil_N")]
  
  # combining the processed data sets to one
  s <- merge(d, d1, by = "trial_id")
  q <- merge(s, d2, by = "trial_id")
  
  q$dataset_id <- dataset_id
  # all scripts should end like this
  carobiner::write_files(dset, q, path, dataset_id, group)
}  
  