

carob_script <- function(path) {
  
  "
  This dataset comprises data from on farm trials conducted in sub-humid Zimbabwe during two 
  cropping seasons (2017/18-2018/19). The study investigated the effect of maize-cowpea
  intercropping on productivity, biological N2-fixation and grain mineral content compared 
  to sole crops. The effect of soil characteristics (homefield vs outfield) and of cowpea 
  varieties landrace) was also studied (2020-08-17)
"
  
	uri <- "doi:10.18167/DVN1/IJOA5J" 
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		data_citation="Namatsheve, Talent; Chikowo, Regis; Corbeels, Marc; Mouquet-Rivier, Claire; Icard-Vernière, Christèle; Cardinael, Rémi, 2020, Data for: Maize-cowpea intercropping as an ecological intensification option for low input systems in sub-humid Zimbabwe: productivity, biological N2-fixation and grain mineral content, https://doi.org/10.18167/DVN1/IJOA5J, CIRAD Dataverse, V2",
		publication= "doi:10.1016/j.fcr.2020.108052",
		data_institutions = "CIRAD",
		data_type="experiment", 
		carob_contributor="Effie Ochieng'",
		carob_date="2023-10-23",
		project=NA
	)
  
  ## download and read data 
  
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
  
  
	f <- ff[basename(ff) == "Namatsheve_et_al_Dataset.xlsx"]
	r <- readxl::read_excel(f, sheet = 2) 
	r1 <- readxl::read_excel(f)
                           
  # subset for intercrop, subset again, one to have cowpea info another maize info then bindr() 
	d <- r[r$System == "Intercrop",]
	d1 <- carobiner::change_names(d, c("100_seed_mass_cowpeas_9.7", "Cowpea_yield_kg_ha"), c("grain_weight", "yield"))
	d1 <- d1[,c("Site","Field","Treatment","Treatment_desc","System","Cowpea_variety","Fertilizer" ,"Year","Net_plot_m2","grain_weight", "yield")]
	d1$crop <- "cowpea"
	d1$yield_part <- "seed"
	d2 <- carobiner::change_names(d, c("100_seed_mass_maize_12.5","Maize_yield_kg_ha"), c("grain_weight", "yield"))
	d2 <- d2[,c("Site","Field","Treatment","Treatment_desc","System","Cowpea_variety","Fertilizer" ,"Year","Net_plot_m2","grain_weight", "yield")]
	d2$crop <- "maize"
	d2$yield_part <- "grain"
	d <- carobiner::bindr(d1,d2)
  
  #subset for monocrop and replace the NAs 
	c <- r[r$System == "Monocrop",]
	c <- c[, c("Site","Field","Treatment","Treatment_desc","System","Cowpea_variety","Fertilizer","Year","Net_plot_m2","100_seed_mass_maize_12.5","100_seed_mass_cowpeas_9.7","Maize_yield_kg_ha","Cowpea_yield_kg_ha")]
	c <- carobiner::change_names(c,c("100_seed_mass_maize_12.5","100_seed_mass_cowpeas_9.7"), c("grain_weight1", "grain_weight2"))
	c$grain_weight1[c$grain_weight1 == "NA"] <- NA
	c$grain_weight1 <- as.numeric(c$grain_weight1)
	c$grain_weight2[c$grain_weight2 == "NA"] <- NA
	c$grain_weight2 <- as.numeric(c$grain_weight2)
	c$Maize_yield_kg_ha[c$Maize_yield_kg_ha == "NA"] <- NA
	c$Maize_yield_kg_ha <- as.numeric(c$Maize_yield_kg_ha)
	c$Cowpea_yield_kg_ha[c$Cowpea_yield_kg_ha == "NA"] <- NA
	c$Cowpea_yield_kg_ha <- as.numeric( c$Cowpea_yield_kg_ha)
  
    i <- is.na(c$grain_weight1) & (!is.na(c$grain_weight2))
    c$grain_weight1[i] <- c$grain_weight2[i]
    c$Maize_yield_kg_ha[i] <- c$Cowpea_yield_kg_ha[i]
   
	c$grain_weight2 <- NULL
	c$Cowpea_yield_kg_ha <- NULL
	c$crop <- ifelse(grepl("cowpea",c$Treatment_desc), "cowpea", "maize")
	c$yield_part <- ifelse(grepl("cowpea",c$crop), "seed", "grain")
	c <- carobiner::change_names(c, c("grain_weight1", "Maize_yield_kg_ha"), c("grain_weight", "yield"))

	d <- carobiner::bindr(d, c)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$country <- "Zimbabwe"
	d$adm1 <- "Mashonaland East"
	d$adm2 <- "Goromonzi"
  
  # efyrouwa:  dates not clear, do we use last date of planting and harvest?
  # Maize planting was 28 November and 01 December during the 2017/18
  # 30 November and 04 December during the 2018/19 cropping season.
  # cowpea 2017/18 ,from 28 December to 01 January.
  # 2018/19, cowpea was sown 14–18 December
  
  # In the 2017/18 season, maize, improved cowpea and landrace cowpea were harvested 30 April and 05 May 2018.
  # In the 2018/19 season improved cowpea was harvested earlier,26 March to 31 March 2019
  # followed by landrace cowpea and maize which were harvested from 15 April to 22 April 2019 
  
	d$planting_date <- ""
	d$harvest_date <- ""
  
	i <- (d$Year == 1 & d$crop == "maize")
	d$planting_date[i] <- "2017-12-01"
	d$harvest_date[i] <- "2018-05-05"
	i <- (d$Year == 2 & d$crop == "maize")
	d$planting_date[i] <- "2018-12-04"
	d$harvest_date[i] <- "2019-04-22"
	i <- (d$Year == 1 & d$crop == "cowpea")
	d$planting_date[i] <- "2018-01-01"
	d$harvest_date[i] <- "2018-05-05"
	i <- d$Year == 2 & d$crop == "cowpea" & d$Cowpea_variety == "Landrace"
	d$planting_date[i] <- "2018-12-18"
	d$harvest_date[i] <- "2019-04-22"
	i <- d$Year == 2 & d$crop == "cowpea" & d$Cowpea_variety == "Improved"
	d$planting_date[i] <- "2018-12-18"
	d$harvest_date[i] <- "2019-03-31"
  
  
	# efyrouwa: from publication. At sowing, 15 kg P ha−1 and 30 kg K ha−1 were applied
	# as single super phosphate and muriate of potash, respectively, in all treatments. 
	# In the treatments receiving N fertilizer (+N), 15 kg N ha−1 was applied 
	# as ammonium nitrate at sowing and at four and six weeks after planting.
	d$K_fertilizer <- 30
	d$P_fertilizer <- 15
	d$N_fertilizer <- ifelse(d$Fertilizer == "+N", 30, 0)
	d$N_splits <- 2
	d$fertilizer_type <- ifelse(d$Fertilizer == "+N","AN; KCl; SSP", "KCl; SSP")

  # efyrouwa : spatial info provided is only for Goromonzi
	d$latitude <- -17.80695
	d$longitude <- 31.36372
  
  #processing the second data set
	d1 <- r1[, c("Site","Field","Treatment","Treatment_desc","System","Cowpea_variety","Fertilizer","Year","Previous_crop","Clay_perc","Silt_perc","Total_sand_perc","pH_CaCl2","Available_P","Total_Nitrogen","SOC_perc","Zn_ICP_mg_kg","Ca_ICP_mg_kg" ,"Mg_ICP_mg_kg", "K_ICP_mg_kg","P_ICP_mg_kg","Cu_ICP_mg_kg","Fe_ICP_mg_kg","Mn_ICP_mg_kg","Al_ICP_mg_kg")]
	d1 <- carobiner::change_names(d1, c("Previous_crop","Clay_perc","Silt_perc","Total_sand_perc","pH_CaCl2","Available_P","Total_Nitrogen","SOC_perc","Zn_ICP_mg_kg","Ca_ICP_mg_kg" ,"Mg_ICP_mg_kg", "K_ICP_mg_kg","P_ICP_mg_kg","Cu_ICP_mg_kg","Fe_ICP_mg_kg","Mn_ICP_mg_kg","Al_ICP_mg_kg"),
     c("previous_crop","soil_clay","soil_silt","soil_sand","soil_pH_CaCl2","soil_P_total","soil_N","soil_SOC","grain_Zn","grain_Ca","grain_Mg","grain_K","grain_P", "grain_Cu", "grain_Fe", "grain_Mn", "grain_Al"))
  
	f <- merge(d,d1, by = c("Site","Field","Treatment","Treatment_desc","System","Cowpea_variety","Fertilizer","Year"), all.x = T)
	f$variety <- f$Cowpea_variety
	f$trial_id <- paste(1:nrow(f),f$Treatment_desc)
	f$dataset_id <- dataset_id
	f$grain_weight <- as.numeric(f$grain_weight)
	f$yield <- as.numeric(f$yield)
	f$N_splits <- as.integer(f$N_splits)
	f$variety <- f$Cowpea_variety
	f$previous_crop <- carobiner::replace_values(f$previous_crop,c("sweet_potatoes", "groundnuts","velvet_beans","fallow"), c("sweetpotato","groundnut","velvet bean",NA))
	f <- f[-1:-9]
  
	# all scripts must end like this
    carobiner::write_files(dset, f, path=path)
}

