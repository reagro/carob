#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 african countries
#################################################################################

carob_script <- function(path){

uri <- "doi.org/10.25502/a7ex-ea51/d"
dataset_id <- carobiner::simple_uri(uri)
group <- "fertilizer"

#dataset level data

dset <- data.frame(
  dataset_id = dataset_id,
  group = group,
  project="N2Africa",
  uri = uri,
  publication = "doi.org/10.1016/j.agee.2017.08.015",
  data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, 
  E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, 
  P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, 
  T., Ronner, E., Kanampiu, F., Giller, K., Baars, E., 
  & Heerwaarden, J. van. (2020). N2Africa agronomy 
  trials - Rwanda, 2012 [Data set]. International Institute 
  of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/A7EX-EA51/D",
  carob_contributor = "Effie Ochieng",
  experiment_type = "on farm",
  has_weather =  FALSE,
  has_management = FALSE
)


## download and read data 

 ff <- carobiner::get_data(uri,path,group)
 js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
 dset$license <- carobiner::get_license(js) 
 
# read the data
 f <- ff[basename(ff) == "data.csv"]
 d <- data.frame(read.csv2(f, sep = ","))
 
 f1 <- ff[basename(ff) == "general.csv"]
 d1 <- data.frame(read.csv2(f1, sep = ","))
 
# f2 <- ff[basename(ff) == "soil_properties.csv"]
# d2 <- data.frame(read.csv2(f2, sep = ","))

 d1$trial_id <- d1$experiment_id
 d1$country <-"Rwanda"
 d1$adm2 <- d1$mandate_area_name
 d1$site <- d1$action_site
 d1 <- d1 [, c("trial_id", "country", "site","adm2")]

## Fix 
#  d1$latitude <- 
#  d1$longitude <- 
 
 
 ##d2$trial_id <- d2$experiment_id
 # EGB: pH is NULL
 # d2$soil_pH <- as.numeric(d2$ph)
 ## RH these are also all zero!
 ##d2$soil_K <- as.numeric(d2$k)
 ##d2$soil_sand <- as.numeric(d2$sand)
 ##d2$soil_clay <- as.numeric(d2$clay)
 ##d2$soil_SOC <- as.numeric(d2$tot_carbon)
 ##d2$soil_N <- as.numeric(d2$tot_nitrogen)
 
 #subset the processed variables
 ##d2 <- d2[, c("trial_id", "soil_K", "soil_sand","soil_clay","soil_SOC","soil_N")]
 
 
#create a "variable 'crop' and fill it using the varieties, data set had no crop variable
 e <- data.frame(variety = trimws(d$variety))
 e$variety[e$variety==""] <- NA

## RH: there are many records for which the crop is not know (and neither is the variety)
## That needs to be resolved
 e$crop <- NA
 e$crop[d$variety %in% c("Peka 6", "Peka6", "Sc. Saga", "Sc. Square", "Sc. Squille", "SB8", "SB24")] <- "soybean"
 e$crop[d$variety %in% c("RWA 1668", "Gasirida", "RWR 1668")] <- "common bean"
 
 #process data sets separately identifying the variables of interest
 e$trial_id <- d$experiment_id
 e$rep <- d$replication_no
 e$on_farm <-TRUE
 e$start_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
 e$end_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
 e$treatment <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, sep = "-")
 e$biomass_roots <- as.numeric(d$root_dry_weight_roots_no_nodules)
 e$biomass_total <- as.numeric((d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules + d$nodule_dry_weight))
 e$yield <- as.numeric(d$grain_yield_ha_calc)

 # adding the fertilizer information
 fert <- trimws(d$sub_treatment_inoc)

 e$P_fertilizer[fert %in% c("DAP", "TSP", "TSP/KCL")] <- 30
 e$K_fertilizer[fert == "TSP/KCL"]<- 30
 ## fix
 ## d$N_fertilizer[fert %in% c("DAP")] <- ????
 
 #subset the processed variables
# d <- d[, c("trial_id", "rep", "on_farm","start_date","end_date", "treatment", "biomass_roots","biomass_total", "yield", "crop","P_fertilizer","K_fertilizer")]
 
 # combining the processed data sets to one
 e <- merge(e, d1, by = "trial_id")
 
 e$dataset_id <- dataset_id
 
 # all scripts should end like this
 carobiner::write_files(dset, e, path, dataset_id, group)
}



