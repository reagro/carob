#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

uri <- "doi.org/10.25502/q4wa-ap97/d"
dataset_id <- carobiner::simple_uri(uri)
group <- "fertilizer"

#dataset level data

dset <- data.frame(
  dataset_id = dataset_id,
  group = group,
  uri = uri,
  publication = NA,
  data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, 
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., 
  Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Baars, E., & Heerwaarden,
  J. van. (2020). N2Africa agronomy trials - Rwanda, 2011 [Data set]. 
  International Institute of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/Q4WA-AP97/D",
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
                           
#process data sets separately identifying the variables of interest 

d$trial_id <- d$experiment_id
d$rep <- d$replication_no
d$on_farm <- TRUE
d$start_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
d$end_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
d$yield <- d$grain_yield_ha_calc


#adding fertilizer information
## RH. Initialize to zero. I assume that all the other cases 
## that are not changed below are zero, not NA!
d$P_fertilizer <- 0
d$K_fertilizer <- 0
d$N_fertilizer <- 0
d$N_splits <- 0

d$P_fertilizer[d$sub_treatment_inoc %in% c("TSP", "TSP/KCL+UREA", "TSP/KCL", "TSP/KCL +UREA")]<- 30
d$K_fertilizer[d$sub_treatment_inoc %in% c("TSP/KCL+UREA", "TSP/KCL", "TSP/KCL +UREA")] <- 30
d$N_fertilizer[d$sub_treatment_inoc %in% c("TSP/KCL+UREA","TSP/KCL +UREA","PK6+Urea","SB24+Urea")] <- 60
d$N_splits[d$N_fertilizer > 0] <- 2

# subsetting the processed variables
d <- d [, c("trial_id","rep","on_farm","start_date","end_date","yield","P_fertilizer","K_fertilizer","N_fertilizer","N_splits")]

d1$trial_id <- d1$experiment_id
d1$country <- "Rwanda"

## RH These do not seem to be ADM1 or ADM2, but rather location names?
d1$adm1 <- carobiner::fix_name(d1$action_site, "title")
d1$adm2 <- carobiner::fix_name(d1$mandate_area_name, "title")
## RH also more name fixing needed see 
## sort(unique(d1$adm1))
## sort(unique(d1$adm2))


d1$crop[grepl("SOY", d1$crop)] <- "soybean"
d1$crop[d1$crop %in% c("Bush BEANS INPUT", "Climbing BEANS INPUT")] <- "common bean"

d1 <- d1 [, c("trial_id","country","adm1","adm2","crop")]

#d2$trial_id <- d2$experiment_id
## these are all zero!! that cannot be, do not include
##d2$soil_pH <- d2$ph
##d2$soil_K <- d2$k
##d2$soil_sand <- d2$sand
##d2$soil_clay <-d2$clay
##d2$soil_SOC <- d2$tot_carbon
##d2$soil_N <- d2$tot_nitrogen
##d2 <- d2 [, c("trial_id","soil_pH","soil_K","soil_sand","soil_clay","soil_SOC","soil_N")]


# combining the processed data sets to one
s <- merge(d, d1, by = "trial_id")
#q <- merge(s, d2, by = "trial_id")

#add the gps information

##RH: how can you have a single coordinate pair for many locations??
s$latitude <-  -1.94028
s$longitude <- 29.87389

s$dataset_id <- dataset_id

# all scripts should end like this
carobiner::write_files(dset, s, path, dataset_id, group)

}
