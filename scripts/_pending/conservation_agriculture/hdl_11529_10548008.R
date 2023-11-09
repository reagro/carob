# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers
"

	uri <- "hdl:11529/10548008"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Ghosh, Anup K.; Islam, Rashadul; Anwar, Mazharul; Molla, Samim H.; Akhter-Ul-Alam, Md., 2018, '6.2- Rabi (winter) crops-all nodes- Validation trials -Rangpur-Bangladesh', https://hdl.handle.net/11529/10548008, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Michelle Njukuya",
		carob_date="2023-09-05"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	
	ff <- ff[grep("xlsx$", ff)]

	# process all ff files, below only the first one 


	f <- ff[basename(ff) == "Rabi 2016-17-validation trials-all nodes-Rangpur.xlsx"]

	r0 <- carobiner::read.excel(f,sheet = "Rabi-Wheat")
	r1 <- carobiner::read.excel(f,sheet = "Rabi-Maize")
	r2 <- carobiner::read.excel(f,sheet = "Kharif I-Jute")
	r3 <- carobiner::read.excel(f,sheet = "Kharif I-Maize")
	r4 <- carobiner::read.excel(f,sheet = "Boro rice")
	
## process file(s)

################################### Rabi-Wheat sheet ##############################
	
#### about the data #####
## (TRUE/FALSE)

	d0 <- data.frame(dataset_id=dataset_id, country="Bangladesh", 
		longitude=r0$Longitude, latitude=r0$Latitude)

	d0$on_farm <- TRUE
	d0$is_survey <- FALSE
	d0$irrigated <- TRUE
## the treatment code	
	d0$treatment <- r0$Tillage

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d0$adm2 <- r0$District
	d0$location <- r0$`Site/Location/Node`
## each site must have corresponding longitude and latitude
## see carobiner::geocode

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d0$crop <- r0$Crop
	d0$variety <- r0$Variety

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d0$planting_date <- r0$`Date of sowing (mm/dd/yryr)`
	d0$harvest_date  <- r0$`Date of harvesting`

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d0$P_fertilizer <- 15
   d0$K_fertilizer <- 10
   d0$N_fertilizer <- 30
   d0$S_fertilizer <- 10
   d0$Zn_fertilizer <- 1
   d0$lime <- NA
## normalize names 
   d0$fertilizer_type <- NA
   d0$inoculated <- FALSE
   d0$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d0$biomass_total <- r0$`Biomass (t/ha)`
	d0$residue_yield <- r0$`Straw yield (t/ha)`
	d0$yield <- r0$`Grain yield (t/ha)`
	#what plant part does yield refer to?
	d0$yield_part <- "grain" 

	d0$season <- "Rabi" #RH fill in 
	
############### END OF Rabi-Wheat sheet ####################################
	
################### Rabi-Maize #########################################
	
	## process file(s)
	
	d1 <- data.frame(dataset_id=dataset_id, country="Bangladesh", 
		longitude=r1$Longitude, latitude=r1$Latitude)
	
	
	#### about the data #####
	## (TRUE/FALSE)
	
	d1$on_farm <- TRUE
	  d1$is_survey <- FALSE
	  d1$irrigated <- TRUE
	  ## the treatment code	
	  d1$treatment <- r1$Tillage
	  
	  ##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	  d1$adm2 <- r1$District
	  d1$location <- r1$`Site/Location/Node`
	  d1$elevation <- NA
	  
	  ##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d1$crop <- r1$Crop
	  d1$variety <- r1$Variety
	  
	  ##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d1$planting_date <- r1$`Date of sowing (mm/dd/yryr)`
	d1$harvest_date  <- r1$`Date of harvesting`
	
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	## P <- P2O5 / 2.29
	## K <- K2O / 1.2051
	d1$P_fertilizer <- 25
	  d1$K_fertilizer <- 30
	  d1$N_fertilizer <- 60
	  d1$S_fertilizer <- 20
	  d1$Zn_fertilizer <- 1
	  d1$lime <- NA
	  ## normalize names 
	  d1$fertilizer_type <- NA
	  d1$inoculated <- FALSE
	d1$inoculant <- NA 

	d1$season <- "Rabi" 
	  
	  ##### in general, add comments to your script if computations are
	  ##### based on information gleaned from metadata, a publication, 
	  ##### or when they are not immediately obvious for other reasons
	  
	  ##### Yield #####
	d1$biomass_total <- r1$`Biomass (t/ha)`
	  d1$residue_yield <- r1$`Straw yield (t/ha)`
	  d1$yield <- r1$`Grain yield (t/ha)`
	  #what plant part does yield refer to?
	  d1$yield_part <- "grain"
	  
############################################ END OF Rabi-Maize ###############################################################################
	  
##############################################	Kharif I-Jute ######################################################################################  
	  ## process file(s)
   
	d2 <- data.frame(dataset_id=dataset_id, country="Bangladesh", 
		longitude=r2$Longitude, latitude=r2$Latitude)
	  
	  #### about the data #####
	  ## (TRUE/FALSE)
	  
	  d2$on_farm <- TRUE
	    d2$is_survey <- FALSE
	    d2$irrigated <- TRUE
	    ## the treatment code	
	    d2$treatment <- r2$Tillage
	    
	    ##### Location #####
	  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	  ## you can use carobiner::fix_name()
	    d2$adm2 <- r2$District
	    d2$location <- r2$`Site/Location/Node`
	    
	    ##### Crop #####
	  ## normalize variety names
	  ## see carobiner::fix_name
	  d2$crop <- r2$Crop 
	    d2$variety <- r2$Variety
	    d2$season <- r2$Season
	    
	    ##### Time #####
	  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	  ## use 	as.character(as.Date()) for dates to assure the correct format.
	  d2$planting_date <- r2$`Date of sowing (mm/dd/yryr)`
	  d2$harvest_date  <- r2$`Date of harvesting`
	  
	  ##### Fertilizers #####
	  ## note that we use P and K, not P2O5 and K2O
	  ## P <- P2O5 / 2.29
	  ## K <- K2O / 1.2051
	  d2$P_fertilizer <- 15
	    d2$K_fertilizer <- 10
	    d2$N_fertilizer <- 20
	    d2$S_fertilizer <- NA # RH should be zero?
		d2$Zn_fertilizer <- NA  # RH should be zero?
	    d2$lime <- NA  # RH should be zero?
	    ## normalize names 
	    d2$fertilizer_type <- NA
	    d2$inoculated <- FALSE
	  d2$inoculant <- NA
	    
	    ##### in general, add comments to your script if computations are
	    ##### based on information gleaned from metadata, a publication, 
	    ##### or when they are not immediately obvious for other reasons
	    
	    ##### Yield #####
	  d2$biomass_total <- r2$`Biomass (t/ha)`
	    
	    d2$yield <- r2$`Jute fibre yield (t/ha)`
	    d2$residue_yield <- r2$`Sun dry Jute stick yield (kg/smpling area)`
	    #what plant part does yield refer to?
	    d2$yield_part <- "stems"
	    
	  
#################END OF Kharif I-Jute ############################
	 
####################### Kharif I-Maize ###########################
	 
	 
	d3 <- data.frame(dataset_id=dataset_id, country="Bangladesh", 
		longitude=r3$Longitude, latitude=r3$Latitude)
	 
	 d3$on_farm <- TRUE
	   d3$is_survey <- FALSE
	   d3$irrigated <- TRUE
	   ## the treatment code	
	   d3$treatment <- r3$Tillage
	   
	   ##### Location #####
	 ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	 ## you can use carobiner::fix_name()
	   d3$adm2 <- r3$District
	   d3$location <- r3$`Site/Location/Node`
	   
	   ##### Crop #####
	 ## normalize variety names
	 ## see carobiner::fix_name
	 d3$crop <- r3$Crop
	   d3$variety <- r3$Variety
	   d3$yield_part <- "grain"
	   
	   ##### Time #####
	 ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	 ## use 	as.character(as.Date()) for dates to assure the correct format.
	 d3$planting_date <- r3$`Date of sowing (mm/dd/yryr)`
	 d3$harvest_date  <- r3$`Date of harvesting`
	 
	 ##### Fertilizers #####
	 ## note that we use P and K, not P2O5 and K2O
	 ## P <- P2O5 / 2.29
	 ## K <- K2O / 1.2051
	 d3$P_fertilizer <- 15
	   d3$K_fertilizer <- 10
	   d3$N_fertilizer <- 30
	   d3$S_fertilizer <- NA # RH zero?
		d3$Zn_fertilizer <- NA  # RH should be zero?
	   d3$lime <- NA  # RH zero?
	   ## normalize names 
	   d3$fertilizer_type <- NA
	   d3$inoculated <- FALSE
	 d3$inoculant <- NA
	   
	   ##### in general, add comments to your script if computations are
	   ##### based on information gleaned from metadata, a publication, 
	   ##### or when they are not immediately obvious for other reasons
	   
	   ##### Yield #####
	 d3$biomass_total <- r3$`Biomass (t/ha)`
	 d3$residue_yield <- r3$`Straw yield (t/ha)`  
	   d3$yield <- r3$`Grain yield (t/ha)`
	 
	 d3$season <- "kharif I"

################# END OF Kharif I-Maize############
	 
############### Boro rice ###########################
	 
	d4 <- data.frame(dataset_id=dataset_id, country="Bangladesh", 
		longitude=r4$Longitude, latitude=r4$Latitude)
	 
	 #### about the data #####
	 d4$on_farm <- TRUE
	   d4$is_survey <- FALSE
	   d4$irrigated <- TRUE
	   ## the treatment code	
	   d4$treatment <- r4$Tillage
	   
	   ##### Location #####
	 ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	 ## you can use carobiner::fix_name()
	   d4$adm2 <- r4$District
	   d4$location <- r4$`Site/Location/Node`
	 ## each site must have corresponding longitude and latitude
	 ## see carobiner::geocode
	   
	   ##### Crop #####
	 ## normalize variety names
	 ## see carobiner::fix_name
	 d4$crop <- r4$Crop
	   d4$variety <- r4$Variety
	   d4$yield_part <- "grain"
	   
	   ##### Time #####
	 ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	 ## use 	as.character(as.Date()) for dates to assure the correct format.
	 d4$planting_date <- r4$`Date of sowing (mm/dd/yryr)`
	 d4$harvest_date  <- r4$`Date of harvesting`
	 
	 ##### Fertilizers #####
	 ## note that we use P and K, not P2O5 and K2O
	 ## P <- P2O5 / 2.29
	 ## K <- K2O / 1.2051
	 d4$P_fertilizer <- 20 
	   d4$K_fertilizer <- 15
	   d4$N_fertilizer <- 30
	   d4$S_fertilizer <- NA # RH 0?
		d4$Zn_fertilizer <- NA  # RH should be zero?
	   d4$lime <- NA # RH 0?
	   ## normalize names 
	   d4$fertilizer_type <- NA
	   d4$inoculated <- FALSE
	 d4$inoculant <- NA
	 d4$season <- r4$Season
	   
	   ##### in general, add comments to your script if computations are
	   ##### based on information gleaned from metadata, a publication, 
	   ##### or when they are not immediately obvious for other reasons
	   
	   ##### Yield #####
	 d4$biomass_total <- r4$`Biomass (t/ha)`
	 d4$residue_yield <- r4$`Straw yield (t/ha)`  
	   d4$yield <- r4$`Grain yield (t/ha)` 
	   

	x <- rbind(d0, d1, d2, d3, d4)
	x$crop <- tolower(x$crop)
	x$planting_date <- as.character(as.Date(x$planting_date))
	x$harvest_date <- as.character(as.Date(x$harvest_date))
	x$adm1 <- x$adm3 <- as.character(NA)
	x$elevation <- as.numeric(NA)
	
	  	# all scripts must end like this
	carobiner::write_files(dset, x, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

