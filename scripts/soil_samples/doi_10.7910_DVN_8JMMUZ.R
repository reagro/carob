
carob_script <- function(path) {
  
"The aim of this project component was to conduct a landscape assessment of soil and land health in near Hallu, Tanzania for the CIAT-Soils activity for the CGIAR Research Program on Climate Change, Agriculture and Food Security (CCAFS). The selection of the LDSF site near Hallu was informed by Selian Agriculture Research Institute (SARI) scientists and chosen to compliment existing CIAT AfricaRISING LDSF sites (near Long and Matufa). This activity was part of a CIAT-SARI activity to access eco-efficiency of farming systems for CCAFS. The Land Degradation Surveillance Framework (LDSF) was employed to conduct a systematic biophysical baseline of key land and soil health metrics. The LDSF is designed to provide a biophysical baseline at landscape level, and a monitoring and evaluation framework for assessing processes of land degradation and the effectiveness of rehabilitation measures (recovery) over time. (2015-11-17)"
  
	uri <- "doi:10.7910/DVN/8JMMUZ"
	group <- "soil_samples"

	ff <- carobiner::get_data(uri, path, group)
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		project= 'CCAFS',
		publication= NA,
		data_institutions = "CIAT",
		data_type = "soil properties", 
		carob_contributor="Andrew Sila", 
		carob_date="2023-09-28"
	)
  
  # No need to read the table with MIR data
	f1 <- ff[basename(ff) == "2 Hallu_Crop_Nutrition_Results.xls"]
	r1 <- data.frame(carobiner::read.excel(f1))
	
	f2 <- ff[basename(ff) == "3 Hallu_Carbon_and_Nitrogen.csv"]
	r2 <- data.frame(read.csv(f2))
	
	f3 <- ff[basename(ff) == "7 Hallu_LDSF_data_raw.csv"]
	r3 <- data.frame(read.csv(f3))
	r3 <- r3[,c('Country', 'Site', 'Cluster', 'Plot', 'Latitude', 'Longitude')]
	r3$trial_id <- paste0(r3$Cluster, '.', r3$Plot)
	
	r1 <- r1[-c(1:5),]
	cnm <-  as.vector(t(r1[1,]))
	colnames(r1) <- cnm
	
	r2[1:5,1:4]
	r2$CIAT_Kenya_Laboratory.ID <- gsub(' ','',r2$CIAT_Kenya_Laboratory.ID)
	
	r <- merge(r2, r1, by.x = 'CIAT_Kenya_Laboratory.ID', by.y = 'Field')
	r$trial_id <- paste0(r$Cluster, '.', r$Plot)
	r <- merge(r3, r)
	
	r$S = ifelse(r$S == '< 0.50', 0.001,r$S)
	
	
	## process file(s)
	
	## select the variables of interest and assign them to the correct name
	# EC to be converted from uS/cm to mS/cm divide by 1000
	d <- data.frame(
	  trial_id = r$trial_id,
	  soil_pH = as.numeric(r$pH),
	  soil_EC = as.numeric(r$`EC(S)`)/1000,
	  soil_Al = as.numeric(r$Al),
	  soil_B = as.numeric(r$B),
	  soil_Ca = as.numeric(r$Ca),
	  soil_Cu = as.numeric(r$Cu),
	  soil_Fe = as.numeric(r$Fe),
	  soil_K = as.numeric(r$K),
	  soil_Mg = as.numeric(r$Mg),
	  soil_Mn = as.numeric(r$Mn),
	  soil_Na = as.numeric(r$Mn),
	  soil_P_total = as.numeric(r$P),
	  soil_S = as.numeric(r$S),
	  soil_Zn = as.numeric(r$Zn),
	  #soil_Acidity = r$Hp,
	  #soil_PSI = r$PSI,
	  #soil_Ex_Na = r$ExNa,
	  #soil_Ex_Ca = r$ExCa,
	  #soil_Ex_K = r$ExK,
	  #soil_ExBas = r$ExBas,
	  #soil_clay = r$psa.c4clay,
	  #soil_silt = r$psa.c4silt,
	  #soil_sand = r$psa.c4sand,
	  soil_C = as.numeric(r$Total.C),
	  soil_N = as.numeric(r$Tota.N)
	)
	
	
	#### about the data #####
	## (TRUE/FALSE)
	#d$on_farm <- TRUE
	# d$is_survey <- FALSE
	#d$irrigated <- FALSE
	## the treatment code	
	# d$treatment <- 
	
	##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d$country <- r$Country
	d$site <- r$Site
	## each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d$longitude <- r$Longitude
	d$latitude <- r$Latitude
	
	d$soil_sample_top <- as.numeric(ifelse(r$`Soil Depth` == 'Top Soil', '0','20'))
	d$soil_sample_bottom <- as.numeric(ifelse(r$`Soil Depth` == 'Sub Soil', '20','50'))
	
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	# d$crop <- 
	#   d$variety <- 
	#   
	##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	# d$planting_date <- as.character(as.Date(   ))
	# d$harvest_date  <- as.character(as.Date(    ))
	# 
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	## P <- P2O5 / 2.29
	## K <- K2O / 1.2051
	# d$P_fertilizer <- 
	#   d$K_fertilizer <-
	#   d$N_fertilizer <- 
	#   d$S_fertilizer <- 
	#   d$lime <- 
	#   ## normalize names 
	#   d$fertlizer_type <- 
	#   
	#   d$inoculated <- TRUE or FALSE
	# d$inoculant <- 
	
	##### in general, add comments to your script if computations are
	##### based on information gleaned from metadata, a publication, 
	##### or when they are not immediately obvious for other reasons
	
	##### Yield #####
	# d$yield <- 
	#   #what plant part does yield refer to?
	#   d$yield_part <- 
	#   
	# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)


	