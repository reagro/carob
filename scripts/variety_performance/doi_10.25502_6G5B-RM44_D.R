carob_script <- function(path){

  "
 Title: N2Africa agronomy trials - Kenya, 2011
 
 Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity 
 of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
 improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, 
 N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit 
 from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants 
 and fertilizers adapted to local settings. A strong national expertise in grain legume production and 
 N2-fixation research and development will be the legacy of the project.The project is implemented in 
 five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, 
 Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
  "
  
uri <- "doi:10.25502/6G5B-RM44/D"
dataset_id <- agro::get_simple_URI(uri)
group <- "variety_performance"

## dataset level data

dset <- data.frame(
  dataset_id = dataset_id,
  group=group,
  uri=uri,
  publication= NA,
  data_citation = "Vanlauwe, Bernard, Adjei-Nsiah, S., Woldemeskel, E.,
  Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer,P., Chikowo, R., Phiphira,
  L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K.,Baars,
  E., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - Kenya, 2010
  [Data set].International Institute of Tropical Agriculture (IITA).
  https://doi.org/10.25502/6G5B-RM44/D",
  data_institutions = "IITA",
  carob_contributor="Rachel Mukami and Effie Ochieng",
  experiment_type="Symbiotic N2 fixation",
  has_weather=FALSE,
  has_management=FALSE)

## download and read data

ff <- carobiner::get_data(uri, path, group)
js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
dset$license <- carobiner::get_license(js)


###download and read data

f <- ff[basename(ff) == "data.csv"]
d <- data.frame(read.csv2(f, sep = "," ))
drop <- c("SN","instanceid")
d = d[,!(names(d) %in% drop)]


f1 <- ff[basename(ff) == "soil_properties.csv"]
d1 <- data.frame(read.csv2(f1, sep = "," ))
drop <- c("SN","instanceid")
d1 = d1[,!(names(d1) %in% drop)]


f2 <- ff[basename(ff) == "general.csv"]
d2 <- data.frame(read.csv2(f2, sep = "," ))
drop <- c("SN","instanceid")
d2 = d2[,!(names(d2) %in% drop)]


## Merging the dataframes to one, I use a full outer join

d3 <- merge(d, d1, by = c("experiment_id","id"), all = TRUE)

d4 <- merge(d3, d2, by = c("experiment_id","id"), all = TRUE)


## Getting the total biomass

d4[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules",
       "nodule_dry_weight")] <- lapply(d4[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules",
                                              "nodule_dry_weight")], as.numeric)
d4$total_biomass <- d4$above_ground_dry_biomass+d4$root_dry_weight_roots_no_nodules+d4$nodule_dry_weight
d4$dataset_id <- dataset_id
d4$trial_id <- d4$experiment_id
d4$rep <- d4$replication_no
d4$treatment <- paste("main treatment: ",d4$main_treatment," | ","inoculant treatment: " ,d4$sub_treatment_inoc," | ","fertilizer treatment: " ,d4$sub_treatment_fert)
d4$inoculated <- ifelse(d4$main_treatment %in% c("Inoculated","inoculated","Inocualted"),"yes",
                        ifelse(d4$main_treatment %in% c("Not noculated","Not Inoculated","Not inoculated","Not inoculted"),"no",NA))
d4$start_date <- as.Date(paste(d4$planting_date_mm,d4$planting_date_dd,d4$planting_date_yyyy,sep = "/"),"%m/%d/%Y")
d4$end_date <- as.Date(paste(d4$date_harvest_mm,d4$date_harvest_dd,d4$date_harvest_yyyy,sep = "/"),"%m/%d/%Y")
d4$yield <- d4$grain_yield_ha_calc
d4$residue_yield <- d4$tot_stover_yield_haulm_husks_calc
d4$biomass_total <- d4$total_biomass
d4$biomass_roots <- d4$root_dry_weight_roots_no_nodules
  
d4$fertilizer_type <- ifelse(d4$sub_treatment_inoc %in% c("DAP","MRP","TSP/KCL/urea","TSP/KCL UREA","TSP/kCL/urea","TSP/KCL/Urea",
                                                          "TSP","TSP/KCL","TSP/KCL/UREA"),d4$sub_treatment_inoc,
                             ifelse(d4$sub_treatment_fert %in% c("TSP/KCL","TSP/KCL/UREA","DAP","MRP","TSP","KCl","TSP/KCl","KCL"),d4$sub_treatment_fert,NA))
d4$soil_pH <- d4$ph
d4$soil_K <- d4$k
d4$soil_sand <- d4$sand
d4$soil_clay <- d4$clay
d4$soil_SOC <- d4$tot_carbon
d4$soil_N <- d4$tot_nitrogen
d4$country <- replace(d4$country,1:nrow(d4),"Kenya")
d4$location <-d4$mandate_area_name
d4$crop <- ifelse(d4$crop == "SOYBEAN "|d4$crop == "SOY BEANS "|d4$crop == "SOY BEANS VARIETY"|
                    d4$crop == "SOY BEANS ROT"|d4$crop == "SOYBEAN","soybean","common bean")

d4$crop[is.na(d4$crop)] = "common bean" # assuming all NA values are common beans
d4$on_farm <- "yes"
d4$latitude<- -0.02356
d4$longitude <- 37.90619

# Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
# and Urea split (50-50) applied at a rate of 60 kg N/ha in Kenya and Rwanda trials
             
d4$N_fertilizer <- ifelse(d4$fertilizer_type %in% c("TSP/KCL/UREA","TSP/KCL/Urea","TSP/kCL/urea","TSP/KCL UREA","TSP/KCL/urea"),60,NA)
d4$P_fertilizer <- ifelse(d4$fertilizer_type %in% c("TSP/KCl","TSP/KCL","TSP","DAP","TSP/KCL/UREA","TSP/KCL/Urea","TSP/kCL/urea","TSP/KCL UREA","TSP/KCL/urea"),30,NA)
d4$K_fertilizer <- ifelse(d4$fertilizer_type %in% c("TSP/KCl","TSP/KCL","TSP/KCL/UREA","TSP/KCL/Urea","TSP/kCL/urea","TSP/KCL UREA","TSP/KCL/urea","KCL","KCl"),30,NA)

d4 <- d4[, c("dataset_id","country","trial_id","location","rep", "treatment", "variety", "start_date",
             "end_date", "yield", "residue_yield", "biomass_total", "biomass_roots", "fertilizer_type", "N_fertilizer","P_fertilizer","K_fertilizer","crop",
             "soil_pH", "soil_K", "soil_sand", "soil_clay", "soil_SOC", "soil_N", "on_farm",
             "latitude", "longitude")]

carobiner::write_files(dset, d4, path, dataset_id, group)
TRUE
}
