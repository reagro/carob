#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

uri <- "doi.org/10.25502/q4wa-ap97/d"
dataset_id <- carobiner::simple_uri(uri)
group <- "variety_trials"

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
  experiment_type = "variety_trials",
  has_weather =  FALSE,
  has_management = FALSE
)


# download and read data 

ff <- carobiner::get_data(uri,path,group)
js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
dset$license <- carobiner::get_license(js) 

# read the data
f <- ff[basename(ff) == "data.csv"]
d <- data.frame(read.csv2(f, sep = ","))

f1 <- ff[basename(ff) == "general.csv"]
d1 <- data.frame(read.csv2(f1, sep = ","))

f2 <- ff[basename(ff) == "soil_properties.csv"]
d2 <- data.frame(read.csv2(f2, sep = ","))
                           
#process data sets separately identifying the variables of interest 

d$trial_id <- d$experiment_id
d$rep <- d$replication_no
d$on_farm <-"yes"
d$start_date <-paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")
d$end_date <- paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")
d$yield <- d$grain_yield_ha_calc

#adding fertilizer information
d$P_fertilizer[d$sub_treatment_inoc == "TSP"|d$sub_treatment_inoc == "TSP/KCL+UREA"|d$sub_treatment_inoc =="TSP/KCL"|d$sub_treatment_inoc =="TSP/KCL +UREA"]<- 30
d$K_fertilizer[d$sub_treatment_inoc == "TSP/KCL+UREA"|d$sub_treatment_inoc =="TSP/KCL"|d$sub_treatment_inoc == "TSP/KCL +UREA"]<- 30
d$N_fertilizer[d$sub_treatment_inoc =="TSP/KCL+UREA"|d$sub_treatment_inoc == "TSP/KCL +UREA"|d$sub_treatment_inoc =="PK6+Urea"|d$sub_treatment_inoc =="SB24+Urea"]<-60
d$N_splits <- 2

# subsetting the processed variables
d <- d [, c("trial_id","rep","on_farm","start_date","end_date","yield","P_fertilizer","K_fertilizer","N_fertilizer","N_splits")]

d1$trial_id <- d1$experiment_id
d1$country <- "Rwanda"
d1$adm1 <- d1$action_site
d1$adm2 <- d1$mandate_area_name
d1$crop <- ifelse(d1$crop ==  "SOYBEAN"|d1$crop == "SOYBEANS "|d1$crop == "SOY BEANS VARIETY"|
                    d1$crop == "Bush BEANS INPUT"|d1$crop == "Climbing BEANS INPUT","soybean","common bean")

d1 <- d1 [, c("trial_id","country","adm1","adm2","crop")]

d2$trial_id <- d2$experiment_id
d2$soil_pH <- d2$ph
d2$soil_K <- d2$k
d2$soil_sand <- d2$sand
d2$soil_clay <-d2$clay
d2$soil_SOC <- d2$tot_carbon
d2$soil_N <- d2$tot_nitrogen

d2 <- d2 [, c("trial_id","soil_pH","soil_K","soil_sand","soil_clay","soil_SOC","soil_N")]

# combining the processed data sets to one
s <- merge(d, d1, by = "trial_id")
q <- merge(s, d2, by = "trial_id")

#add the gps information

q$latitude <-  -1.94028
q$longitude <- 29.87389

q$dataset_id <- dataset_id

# all scripts should end like this
carobiner::write_files(dset, q, path, dataset_id, group)
TRUE
}
