
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
  
  # registering the dataset
  uri <- "doi:10.25502/VMVB-SN23/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "variety_performance"
  
  # The metadata at the dataset level
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation = "Vanlauwe, B. et al. (2020) ‘N2Africa agronomy trials - Kenya, 2011’. International Institute of Tropical Agriculture (IITA). doi:10.25502/VMVB-SN23/D.",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami and Effie Ochieng",
    experiment_type="N2 fixation",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## downloading data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  # reading the data.csv data
  f <- ff[basename(ff) == "data.csv"]
  d <- read.csv(f)

  # Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
  # and Urea split (50-50) applied at a rate of 60 kg N/ha in Kenya and Rwanda trials
  
  d$fertilizer_type <- d$sub_treatment_fert
  d$N_fertilizer[d$fertilizer_type == "TSP/KCL/Urea"] <- 60
  
  d$P_fertilizer[d$fertilizer_type == "TSP/KCL"|d$fertilizer_type == "DAP"|d$fertilizer_type == "TSP/KCL/Urea"
                 |d$fertilizer_type == "TSP"] <- 30
  
  d$K_fertilizer[d$fertilizer_type == "TSP/KCL"|d$fertilizer_type == "TSP/KCL/Urea"] <- 30
  
  d$trial_id <- d$experiment_id
  d$rep <- d$replication_no
  d$treatments <- paste0("main treatment: ",d$main_treatment," |","subtreatment inoculation : ",d$sub_treatment_inoc, 
                         "|","subtreatment fertilizer : ",d$sub_treatment_fert)
  
  d$treatment <- d$treatments
  d$planting_date <- as.Date(paste(d$planting_date_yyyy,d$planting_date_mm,d$planting_date_dd, sep = "-"))
  d$start_date <- d$planting_date
  d$harvest_date <- as.Date(paste(d$date_harvest_yyyy,d$date_harvest_mm,d$date_harvest_dd,sep = "-"))
  d$end_date <- d$harvest_date
  
  d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules","nodule_dry_weight")] <- 
    lapply(d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules","nodule_dry_weight")], as.numeric)
  
  d$biomass_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules+d$nodule_dry_weight)
  d$yield <- d$grain_yield_ha_calc
  d$residue_yield <- d$tot_stover_yield_haulm_husks_calc
  d$grain_weight <- as.numeric(d$dry_weight_100_seeds)*10
  x <- d[,c("trial_id","rep","treatment","variety","start_date","end_date","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","biomass_total","yield","residue_yield","grain_weight")]
  
  # reading the rust_score.csv data
  
  f <- ff[basename(ff) == "rust_score.csv"]
  d1 <- read.csv(f)
  
  d1$trial_id <- d1$experiment_id
  d1$rep <- d1$replication_no
  d1$sub_treatment_inoc <- replace(d1$sub_treatment_inoc,1:nrow(d1),"Inoculated")
  d1$treatments <- paste0("main treatment: ",d1$main_treatment," |","subtreatment inoculation : ",d1$sub_treatment_inoc, 
                          " |","subtreatment fertilizer : ",d1$sub_treatment_fert)
  d1$treatment <- d1$treatments
  d1$on_farm <- "yes"
  d1$latitude <-	-0.02356
  d1$longitude <-	37.90619
  d1$fertilizer_type <- d1$sub_treatment_fertiliser
  d1$N_fertilizer[d1$fertilizer_type == "TSP/KCL/Urea"] <- 60
  d1$P_fertilizer[d1$fertilizer_type == "TSP/KCL/Urea"] <- 30
  d1$K_fertilizer[d1$fertilizer_type == "TSP/KCL/Urea"] <- 30
  x1 <- d1[,c("trial_id","rep","on_farm","variety","latitude","longitude","treatment",
              "fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer")]
  
  # reading the soil_properties.csv data
  f <- ff[basename(ff) == "soil_properties.csv"]
  d2 <- read.csv(f)
  d2$trial_id <- d2$experiment_id
  d2$adm1 <- d2$mandate_area_name
  d2$dates <- as.Date(paste(d2$date_checked_yyyy,d2$date_checked_mm,d2$date_checked_dd,sep = "-")) #date in "yyyy-mm-dd"
  d2$adm1 <- d2$mandate_area_name
  d2$location <- d2$action_site
  d2$observation_date <- d2$dates
  
  # assumption is that there are only two bean types,soybeans and common beans
  d2$crop <- ifelse(d2$crop == "SOY BEANS INPUT"|d2$crop == "SOY BEAN-RUST EVALUATION"|d2$crop == "SOY BEANS ROT"|d2$crop == "RUST EVALUATION -MUMIAS","soybean","common bean")
  x2 <- d2[,c("trial_id", "country","location","adm1","observation_date","crop")]
  
  # combining into 1 final dataset
  y <- merge(x,x1,by = c("trial_id","rep","treatment", "variety",
                         "fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer"),all = TRUE)
  z <- merge(y,x2,by = c("trial_id"),all = TRUE)
  z$dataset_id <- dataset_id
  z$on_farm <- replace(z$on_farm,1:nrow(z),"yes")
  z$latitude <- replace(z$latitude,1:nrow(z),"-0.02356")
  z$longitude <- replace(z$longitude,1:nrow(z),"37.90619")
  
 
	#sort(unique(z$variety))
	v <- carobiner::fix_name(z$variety)
	i <- grepl("Kenya", v, ignore.case=TRUE)
	v[i] <- fix_name(v[i], "title")
	i <- grepl("qui", v, ignore.case=TRUE)
	v[i] <- "Sequel"

	v <- carobiner::replace_values(v, 
		c("GASIRIDA", "UMUBANO", "NEWROSCOCO",  "OKWODHO", "MAMESA", "EAI3600", "SB19" ),
		c("Gasirida", "Umubano", "New Roscoco", "Okwodho", "Mamesa", "EAI 3600","SB 19"))
	v <- carobiner::replace_values(v, 
		c("MAC 44", "Mac 49", "Mac 9", "Mac44",  "saga", "TGX 1740-2F", "TGx-1987-62 F", "Variety"),
		c("MAC 44", "MAC 49", "MAC 9", "MAC 44", "Saga", "TGx1740-2F",  "TGx1987-62-F",  NA))

	v <- gsub("^TGx", "TGx ", v)
	v <- gsub("^KK", "KK ", v)
	v <- gsub("^RWV", "RWV ", v)
	v <- gsub("  ", " ", v)

	# this is a bit speculative 
	v <- carobiner::replace_values(v, 
		c("SC Saga", "SC Sequel"),
		c("Saga", "Sequel"))
		
	#sort(unique(v))
	z$variety <- v 
 
	z$fertilizer_type <- carobiner::fix_name(z$fertilizer_type)
	#unique(z$fertilizer_type)
	
	
	message("   'fertilizer_type' contains variety names and numbers.\n   Perhaps an error in the original data? Needs to be fixed\n")	
 
    # all scripts must end like this
	carobiner::write_files(dset, z, path, dataset_id, group)
}

