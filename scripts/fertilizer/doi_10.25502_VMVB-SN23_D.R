
 ## The original data has a problem as some rows are placed in the wrong columns



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
  group <- "fertilizer"
  
  # The metadata at the dataset level
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
	project="N2Africa",
    uri=uri,
    publication="doi.org/10.21955/gatesopenres.1115299.1",
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
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  d$N_fertilizer <- 0
  d$S_fertilizer <- 0
  d$N_splits <- 0
  
  d$P_fertilizer[d$sub_treatment_fert%in% c("TSP/KCL", "TSP", "TSP/KCL/Urea","DAP")]<- 30
  d$K_fertilizer[d$sub_treatment_fert %in% c("TSP/KCL", "MRP-PALLETS","MRP-DUST","MRP-PALLET", "TSP/KCL/Urea" )] <- 30
  d$N_fertilizer[d$sub_treatment_fert %in% c("TSP/KCL/Urea")] <- 60
  d$N_splits[d$N_fertilizer > 0] <- 2
  d$P_fertilizer[d$sub_treatment_fert == "SYMPAL"] <- 23
  d$K_fertilizer[d$sub_treatment_fert == "SYMPAL"] <- 15
  d$S_fertilizer[d$sub_treatment_fert == "SYMPAL"] <- 4
  
  
  
  # standardizing the all the fertilizer types
  ##MRP is muriate of potash 
  ft <- ifelse(d$sub_treatment_inoc %in% c("TSP/KCL","KCL"), d$sub_treatment_inoc,
               ifelse(d$sub_treatment_fert %in% c("None","MRP-PALLETS","DAP","SYMPAL","MRP-DUST","TSP/KCL","TSP","NONE","MRP-PALLET","TSP/KCL/Urea"),d$sub_treatment_fert,NA))
  
   ft <- toupper(ft)
  ft <- gsub("KCL", "KCl", ft)
  ft <- gsub("UREA", "urea", ft)
  ft <- gsub("None","none",ft)
  ft <- gsub("NONE","none",ft)
  ft <- gsub("MRP-PALLETS","KCl",ft) 
  ft <- gsub("MRP-PALLET","KCl",ft) 
  ft <- gsub("MRP-DUST", "KCl", ft)
  ft <- gsub( "SYMPAL","sympal", ft)
  ft <- gsub(" ", "; ", ft)
  
  
  d$fertilizer_type <- gsub("/", "; ", ft)
  d$trial_id <- d$experiment_id
  d$rep <- d$replication_no
  d$start_date <- as.character(as.Date(paste(d$planting_date_yyyy,d$planting_date_mm,d$planting_date_dd, sep = "-")))
  d$end_date <-as.character(as.Date(paste(d$date_harvest_yyyy,d$date_harvest_mm,d$date_harvest_dd, sep = "-")))
  d <-replace(d,d=="0000-08-04","2011-08-04")# correcting the dates
  d$yield <- d$grain_yield_ha_calc
  d$residue_yield <- d$tot_stover_yield_haulm_husks_calc
  d$biomass_roots <- d$root_dry_weight_roots_no_nodules
  d$biomass_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules + d$nodule_dry_weight)
  d$yield <- d$grain_yield_ha_calc
  d$residue_yield <- d$tot_stover_yield_haulm_husks_calc
  d$grain_weight <- as.numeric(d$dry_weight_100_seeds)*10 # to get the 1000 grain weight
  d$treatment <- carobiner::fix_name(d$main_treatment, "title")
  d$treatment <- gsub("Withou Lime", "Without Lime", d$treatment)
  d$treatment <- gsub("Minus Lime", "Without Lime", d$treatment )
  d$inoculated <- ifelse(d$sub_treatment_inoc %in% c("Inoculated","inoculated" ,"Inoculation"),TRUE,FALSE)
  
  x <- d[,c("trial_id","rep","variety","treatment","inoculated","start_date","end_date","fertilizer_type","N_fertilizer","N_splits","P_fertilizer","K_fertilizer","S_fertilizer","yield","residue_yield","grain_weight","biomass_roots","biomass_total")]
  
  # reading the rust_score.csv data
  
  f <- ff[basename(ff) == "rust_score.csv"]
  d1 <- read.csv(f)
  
  d1$trial_id <- d1$experiment_id
  d1$rep <- d1$replication_no
  d1$treatment <- d1$main_treatment
  d1$inoculated <-d1$sub_treatment_inoculation
  d1$fertilizer_type <- gsub( "/","; ",d1$sub_treatment_fertiliser)
  
  # Standardizing the fertilizer types
  d1$N_splits <- 0
  d1$P_fertilizer <- 0
  d1$K_fertilizer <- 0
  d1$N_fertilizer <- 0
  d1$S_fertilizer <- 0
  d1$N_splits[d1$N_fertilizer > 0] <- 2
  d1$N_fertilizer[d1$fertilizer_type == "TSP; KCL; Urea"] <- 60
  d1$P_fertilizer[d1$fertilizer_type == "TSP; KCL; Urea"] <- 30
  d1$K_fertilizer[d1$fertilizer_type == "TSP; KCL; Urea"] <- 30
  d1$P_fertilizer[d1$fertilizer_type == "sympal"] <- 23
  d1$K_fertilizer[d1$fertilizer_type == "sympal"] <- 15
  d1$S_fertilizer[d1$fertilizer_type == "sympal"] <- 4
  
  x1 <- d1[,c("trial_id","rep","variety","treatment","inoculated","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","S_fertilizer","N_splits")]
  
  # reading the soil_properties.csv data
  f <- ff[basename(ff) == "soil_properties.csv"]
  d2 <- read.csv(f)
  d2$trial_id <- d2$experiment_id
  d2$adm1 <-gsub("-.*","",d2$mandate_area_name)
  
  d2$adm1 <- carobiner::replace_values(d2$adm1, c("Western","WESTERN","Nyanza","South Nyanza","Western ","RANGENYA"),
                                               c("Western","Western","Nyanza","South Nyanza","Western","Rangenya"))
  d2$location <- gsub("-.*","",d2$action_site)
  d2$location <- carobiner::replace_values(d2$location, c("Bungoma","Butere","KAKAMEGA","Migori","Butere ","Mumias","Kakamega south","Kisumu west","MIGORI","BUTULA","BUNGOMA","Rarieda","BUTERE"),
                                                        c("Bungoma","Butere","Kakamega","Migori","Butere","Mumias","Kakamega South","Kisumu West","Migori","Butula","Bungoma","Rarieda","Butere"))
  
  d2$observation_date <- as.character(as.Date(paste(d2$date_checked_yyyy,d2$date_checked_mm,d2$date_checked_dd, sep = "-")))
  d2$country <- "Kenya"
  
  
  x2 <- d2[,c("trial_id", "country","adm1","location","observation_date")]
  
  # combining into 1 final dataset
  y <- merge(x,x1,by = c("trial_id","rep","treatment","inoculated", "variety","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","N_splits","S_fertilizer"),all.x = TRUE)
  z <- merge(y,x2,by = "trial_id", all.x = TRUE)
  z$dataset_id <- dataset_id
  z$on_farm <- TRUE
  
  # lats and longs extracted from geonames.org
  z$latitude <- ifelse(z$location == "Bungoma", 0.5635,
                       ifelse(z$location == "Butere",0.20694,
                              ifelse(z$location == "Kakamega",0.28422,
                                     ifelse(z$location =="Migori",-0.982,
                                            ifelse(z$location =="Mumias",0.33474,
                                                   ifelse(z$location =="Kakamega South",0.17124,
                                                          ifelse(z$location == "Kisumu West",-0.091702,
                                                                 ifelse(z$location == "Butula",0.33796,  -2.218))))))))
    
  z$longitude <- ifelse(z$location == "Bungoma",34.56055,
                        ifelse(z$location == "Butere",34.49006,
                               ifelse(z$location == "Kakamega",34.75229,
                                      ifelse(z$location =="Migori",34.409,
                                             ifelse(z$location =="Mumias",34.48796,
                                                    ifelse(z$location =="Kakamega South",34.59466,
                                                           ifelse(z$location == "Kisumu West",34.767956,
                                                                  ifelse(z$location == "Butula",34.3355,37.316))))))))
  
  
 
	#sort(unique(z$variety))
	v <- carobiner::fix_name(z$variety)
	i <- grepl("Kenya", v, ignore.case=TRUE)
	v[i] <- carobiner::fix_name(v[i], "title")
	i <- grepl("qui", v, ignore.case=TRUE)
	v[i] <- "Sequel"

	v <- carobiner::replace_values(v, 
		c("GASIRIDA", "UMUBANO", "NEWROSCOCO",  "OKWODHO", "MAMESA", "EAI3600", "SB19"),
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
 
	#filling in the crop column using the varieties
	z$crop <-""
	z$crop[z$variety %in% c("Kenya Umoja","Kenya Tamu","Kenya Mavuno","New Roscoco","Umubano","Mamesa","Gasirida","KK 071","KK 072","KK 15",
	                        "KK 8","Local check (Tsimbindi)","MAC 44","MAC 49","MAC 9","RWV 1129","RWV 2070","RWV 51348","Okwodho")]<- "common bean"
	
	z$crop[z$variety %in% c("TGx 1740-2F","SB 19","SB 25","SB 3","SB 8","Maksoy","Namsoy","TGx 1987-62F","TGx 1987-6F","Sequel","Nyala","Samba",
	                        "Saga","S823-6-16","EAI 3600","Maksoy 4M","Namsoy 2N","TGx 1987-62-F")]<- "soybean"
	
	# removing rows that have no entries, problem is with the original data
	z<- z[-c(1:2, 87,205,290,375,460,525,629,678,748,784,794,834,868,902:903,988,1073,1158,1243:1244,
	         1329,1414:1415,1500:1501,1586:1587,1672,1730:1731,1786:1787), ]
	
	# z$fertilizer_type <- carobiner::fix_name(z$fertilizer_type)
	#unique(z$fertilizer_type)
	message("   'fertilizer_type'  contains variety names and numbers, 'variety' has numbers too hence 'crop'
	        contains white space.\n  Due to error in the original data. Needs to be fixed\n")
	
	#rearranging the data 
	
	z <- z[,c("dataset_id","trial_id","country","adm1","location","crop","variety","rep","treatment","inoculated","fertilizer_type","N_fertilizer","N_splits",
	          "P_fertilizer","K_fertilizer","S_fertilizer","start_date","end_date","yield","residue_yield",
	          "grain_weight","biomass_roots","biomass_total","on_farm","observation_date","latitude","longitude")] 
	

 
    # all scripts must end like this
	carobiner::write_files(dset, z, path, dataset_id, group)
}

