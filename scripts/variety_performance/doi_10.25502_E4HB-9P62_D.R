
"
Title: N2Africa agronomy trials - Rwanda, 2010
  
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity 
of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
improving household nutrition and increasing income levels of smallholder farmers. As a vision of success,
N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit 
from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants
and fertilizers adapted to local settings. A strong national expertise in grain legume production and 
N2-fixation research and development will be the legacy of the project. The project is implemented in 
five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, 
Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

"
carob_script <- function(path) {
  
  uri <- "doi:10.25502/E4HB-9P62/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "variety_performance"
  
  ## data set level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M.,
    Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller,
    K., Baars, E., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - Rwanda, 2010 [Data set]. 
    International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/E4HB-9P62/D",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    experiment_type="variety_performance",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  # reading the data.csv data
  f <- ff[basename(ff) == "data.csv"]
  d <- read.csv(f)
  
  d$trial_id <- d$experiment_id
  d$rep <- d$replication_no
  
  # Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
  # and Urea split (50-50) applied at a rate of 60 kg N/ha
  
  d$treatments <- paste("main treatment: ",d$main_treatment," |","subtreatment inoculation : ",d$sub_treatment_inoc)
  d$treatment <- d$treatments
  d$planting_date <- as.Date(paste(d$planting_date_yyyy,d$planting_date_mm,d$planting_date_dd, sep = "-"))
  d$start_date <- d$planting_date
   
## RH: first fix the names; that saves a lot of effort later on
## RH:  d$fertilizer_type <- d$sub_treatment_inoc   

	f <- carobiner::fix_name(d$sub_treatment_inoc, "upper")
	f <- gsub(" ", "", f)
	f <- gsub("\\+", "/", f)
## RH: inspect
#	sort(unique(f))
	d$fertilizer_type = f
	
	
## RH: use %in% or "grep" instead of endless "OR OR OR"	
## RH: ifelse is also a bit difficult to read with long clauses
## RH: no P fertilizer means that it is zero, not NA!
	d$P_fertilizer <- 0 
	i <- d$fertilizer_type %in% c("DAP", "TSP", "TSP/KCL", "TSP/KCL/UREA", "TSP/KCl")
    d$P_fertilizer[i] <- 30 

 	d$K_fertilizer <- 0 
	i <- grep("KCL", d$fertilizer_type) 
    d$K_fertilizer[i] <- 30 
 
	d$N_fertilizer <- 0 
	i <- grep("UREA", d$fertilizer_type) 
	d$N_fertilizer[i] <- 60 
#RH: check if the DAP application is indeed 60 kg/ha. 
#RH: it was not included but it is a N fertilizer
	d$N_fertilizer[d$fertilizer_type == "DAP"] <- 60 	

#RH: what is PK6? Does that have N? The original code suggests it does.
#RH: then what about P? Or is it an innoculant?

#RH innoculants need to go to a separate column. They are not fertilizers. 

##RH: always check like this 
unique(d[,c("fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer")]  )

##RH : original code  
##RH  d$P_fertilizer <- ifelse(d$fertilizer_type == "DAP"|d$fertilizer_type == "TSP"|d$fertilizer_type == "TSP/KCL"|
##RH                             d$fertilizer_type == "TSP/KCL+UREA"|d$fertilizer_type == "TSP/KCL +UREA"|d$fertilizer_type == "TSP/KCl"|
##RH                             d$fertilizer_type == "TSP/KCl+UREA"|d$fertilizer_type == "TSP/KCL/UREA"|
##RH                             d$fertilizer_type == "TSP/KCl/Urea"|d$fertilizer_type == "TSP/KCl+urea",30,NA)
  
##RH  d$K_fertilizer <- ifelse(d$fertilizer_type == "TSP/KCL"|d$fertilizer_type == "TSP/KCL+UREA"|
##RH                             d$fertilizer_type == "TSP/KCL +UREA"|d$fertilizer_type == "TSP/KCl"|
##RH                             d$fertilizer_type == "TSP/KCl+UREA"|d$fertilizer_type == "TSP/KCL/UREA"|
##RH                             d$fertilizer_type == "TSP/KCl/Urea"|d$fertilizer_type == "TSP/KCl+urea",30,NA)
  
##RH  d$N_fertilizer <- ifelse(d$fertilizer_type == "PK6+Urea"|d$fertilizer_type == "PK6 +Urea"|d$fertilizer_type == "TSP/KCL+UREA"|
##RH                             d$fertilizer_type == "TSP/KCL +UREA"|d$fertilizer_type == "TSP/KCl+UREA"|d$fertilizer_type == "TSP/KCL/UREA"|
##RH                             d$fertilizer_type == "TSP/KCl/Urea"|d$fertilizer_type == "TSP/KCl+urea",60,NA)
  
  
  d$harvest_date <- as.Date(paste(d$date_harvest_yyyy,d$date_harvest_mm,d$date_harvest_dd, sep = "-"))
  d$end_date <-d$harvest_date
  d$grain_weight <- as.numeric(d$dry_weight_100_seeds)*10
  d$yield <- d$grain_yield_ha_calc
  d$residue_yield <- d$tot_stover_yield_haulm_husks_calc
  
  d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules","nodule_dry_weight")] <- 
    lapply(d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules","nodule_dry_weight")], as.numeric)
  
  d$biomass_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules+d$nodule_dry_weight)

  x <- d[,c("trial_id","rep","treatment","variety","start_date","end_date","grain_weight","yield","residue_yield",
            "biomass_total", "fertilizer_type","N_fertilizer","K_fertilizer","P_fertilizer")]
  
  # reading the general.csv data
  f <- ff[basename(ff) == "general.csv"]
  d1 <- read.csv(f)
  d1$trial_id <- d1$experiment_id
  d1$location <- d1$action_site
  d1$adm1 <- d1$mandate_area_name
  x1 <- d1[,c("trial_id","location","adm1","country","crop")]
  
  # reading the soil_properties.csv data
  f <- ff[basename(ff) == "soil_properties.csv"]
  d2 <- read.csv(f)
  d2$trial_id <- d2$experiment_id
  d2$soil_pH <- d2$ph
  d2$soil_sand <- d2$sand
  d2$soil_clay <- d2$clay
  d2$soil_N <- d2$tot_nitrogen
  d2$soil_K <- d2$k
  x2 <- d2[,c("trial_id","soil_pH","soil_sand","soil_clay","soil_N","soil_K")]
  
  
  # combining into 1 data set
  y <- merge(x,x1,by = "trial_id",all = TRUE)
  w <- merge(y,x2,by = "trial_id",all = TRUE)
  
  w$dataset_id <- dataset_id
  w$on_farm <- "yes"
  w$latitude <- -1.94028
  w$longitude <- 29.87389
  w$crop <- ifelse(w$crop %in% c("Bush Beans","Climbing Beans","Bush bean","Bush BEAN","Bush BEANS ","Bush BEANS",
                                  "Climbing BEANS ","Climbing bean"),"common bean",
                    ifelse(w$crop %in% c("Soybeans","SOY BEANS INPUT","SOYBEAN","Climbing bean","SOYBEAN "),"soybean","common bean")) # filled all NA values with common bean crop
  
  
	v <- carobiner::fix_name(w$variety)
	v <- carobiner::replace_values(v, 
		c("GASILIDA", "GASIRIDA", "MAMASA", "MAMESA", "SOPROSOY", "YEZUMUTIMA"),
		c("Gasirida", "Gasirida", "Mamesa", "Mamesa", "Soprosoy", "Yezumitima"))
  
  	v <- gsub("^RWA", "RWA ", v)
	v <- gsub("^RWR", "RWR ", v)
	v <- gsub("  ", " ", v)

	sort(unique(v))
	message("   'variety' contains innoculant names and fertilizers.\n   Perhaps an error in the original data. This must be fixed?\n")	
 
	# all scripts must end like this
	carobiner::write_files(dset, w, path, dataset_id, group)
}

