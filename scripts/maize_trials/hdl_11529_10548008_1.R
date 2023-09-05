# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers
    

"

	uri <- "hdl:11529/10548008"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trial"
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
		carob_contributor="Michelle Njukuya"
	)

## download and read data 
 path <- "C:/Users/user/Documents/DataAnalysis/carob-BangladashDataset"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- "C:/Users/user/Documents/DataAnalysis/carob-BangladashDataset/data/raw/maize_trial/hdl_11529_10548008/Rabi 2016-17-validation trials-all nodes-Rangpur.xlsx"
	library("readxl")
	r <- read_excel(f)
	r <- readxl::read_excel(f,sheet = "Rabi-Wheat") |> as.data.frame()
	r1 <- readxl::read_excel(f,sheet = "Rabi-Maize") |> as.data.frame()
	r2 <- readxl::read_excel(f,sheet = "Kharif I-Jute") |> as.data.frame()
	r3 <- readxl::read_excel(f,sheet = "Kharif I-Maize") |> as.data.frame()
	r4 <- readxl::read_excel(f,sheet = "Boro rice") |> as.data.frame()
	
## process file(s)

################################### Rabi-Wheat sheet ##############################
	r <- readxl::read_excel(f,sheet = "Rabi-Wheat") |> as.data.frame()
		d <- r
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- d$Tillage

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Bangladesh"
	d$site <- d$District
	d$adm1 <- d$`Site/Location/Node`
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- d$Longitude
	d$latitude <- d$Latitude

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- d$Crop
	d$variety <- d$Variety

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- d$`Date of sowing (mm/dd/yryr)`
	d$harvest_date  <- d$`Date of harvesting`

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- "15"
   d$K_fertilizer <- "10"
   d$N_fertilizer <- "30"
   d$S_fertilizer <- "10"
   d$Zn_fertilizer <- "1"
   d$lime <- NA
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- d$`Biomass (t/ha)`
  d$residue_yield <- d$`Straw yield (t/ha)`
	d$yield <- d$`Grain yield (t/ha)`
	#what plant part does yield refer to?
	d$yield_part <- "grain" 
	
	d <- d[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1", "longitude","latitude","treatment","crop","variety","planting_date","harvest_date","P_fertilizer","K_fertilizer","N_fertilizer","S_fertilizer","Zn_fertilizer","inoculated","inoculant","biomass_total","residue_yield","yield","yield_part")]
	################################### END OF Rabi-Wheat sheet #############################################################################################################################################################################################################################################	

	################################### Rabi-Maize #################################################################
	
	## process file(s)
	r1 <- readxl::read_excel(f,sheet = "Rabi-Maize") |> as.data.frame()
	
	d1 <- r1
	
	
	#### about the data #####
	## (TRUE/FALSE)
	
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	  d1$is_survey <- FALSE
	  d1$is_experiment <- TRUE
	  d1$irrigated <- TRUE
	  ## the treatment code	
	  d1$treatment <- d1$Tillage
	  
	  ##### Location #####
	## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	## you can use carobiner::fix_name()
	d1$country <- "Bangladesh"
	  d1$site <- d1$District
	  d1$adm1 <- d1$`Site/Location/Node`
	  d1$adm2 <- NA
	  d1$adm3 <- NA
	  d1$elevation <- NA
	## each site must have corresponding longitude and latitude
	## see carobiner::geocode
	d1$longitude <- d1$Longitude
	  d1$latitude <- d1$Longitude
	  
	  ##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d1$crop <- d1$Crop
	  d1$variety <- d1$Variety
	  
	  ##### Time #####
	## time can be year (four characters), year-month (7 characters) or date (10 characters).
	## use 	as.character(as.Date()) for dates to assure the correct format.
	d1$planting_date <- d1$`Date of sowing (mm/dd/yryr)`
	d1$harvest_date  <- d1$`Date of harvesting`
	
	##### Fertilizers #####
	## note that we use P and K, not P2O5 and K2O
	## P <- P2O5 / 2.29
	## K <- K2O / 1.2051
	d1$P_fertilizer <- "25"
	  d1$K_fertilizer <- "30"
	  d1$N_fertilizer <- "60"
	  d1$S_fertilizer <- "20"
	  d1$Zn_fertilizer <- "1"
	  d1$lime <- NA
	  ## normalize names 
	  d1$fertlizer_type <- NA
	  d1$inoculated <- FALSE
	d1$inoculant <- NA 
	  
	  ##### in general, add comments to your script if computations are
	  ##### based on information gleaned from metadata, a publication, 
	  ##### or when they are not immediately obvious for other reasons
	  
	  ##### Yield #####
	d1$biomass_total <- d1$`Biomass (t/ha)`
	  d1$residue_yield <- d1$`Straw yield (t/ha)`
	  d1$yield <- d1$`Grain yield (t/ha)`
	  #what plant part does yield refer to?
	  d1$yield_part <- "grain"
	  
	  d1 <- d1[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","crop","variety","planting_date","harvest_date","P_fertilizer","K_fertilizer","N_fertilizer","S_fertilizer","Zn_fertilizer","inoculated","inoculant","biomass_total","residue_yield", "yield","yield_part")]
############################################ END OF Rabi-Maize ###############################################################################
	  
##############################################	Kharif I-Jute ######################################################################################  
	  ## process file(s)
	  r2 <- readxl::read_excel(f,sheet = "Kharif I-Jute") |> as.data.frame() 
	   
	  d2 <- r2

	  
	  #### about the data #####
	  ## (TRUE/FALSE)
	  
	  d2$dataset_id <- dataset_id
	  d2$on_farm <- TRUE
	    d2$is_survey <- FALSE
	    d2$is_experiment <- TRUE
	    d2$irrigated <- TRUE
	    ## the treatment code	
	    d2$treatment <- d2$Tillage
	    
	    ##### Location #####
	  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	  ## you can use carobiner::fix_name()
	  d2$country <- "Bangladash"
	    d2$site <- d2$District
	    d2$adm1 <- d2$`Site/Location/Node`
	    d2$adm2 <- NA
	    d2$adm3 <- NA
	    d2$elevation <- NA
	  ## each site must have corresponding longitude and latitude
	  ## see carobiner::geocode
	  d2$longitude <- d2$Longitude
	    d2$latitude <- d2$Latitude
	    
	    ##### Crop #####
	  ## normalize variety names
	  ## see carobiner::fix_name
	  d2$crop <- d2$Crop 
	    d2$variety <- d2$Variety
	    d2$season <- d2$Season
	    
	    ##### Time #####
	  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	  ## use 	as.character(as.Date()) for dates to assure the correct format.
	  d2$planting_date <- d2$`Date of sowing (mm/dd/yryr)`
	  d2$harvest_date  <- d2$`Date of harvesting`
	  
	  ##### Fertilizers #####
	  ## note that we use P and K, not P2O5 and K2O
	  ## P <- P2O5 / 2.29
	  ## K <- K2O / 1.2051
	  d2$P_fertilizer <- "15"
	    d2$K_fertilizer <- "10"
	    d2$N_fertilizer <- "20"
	    d2$S_fertilizer <- NA
	    d2$lime <- NA
	    ## normalize names 
	    d2$fertlizer_type <- NA
	    d2$inoculated <- TRUE/FALSE
	  d2$inoculant <- NA
	    
	    ##### in general, add comments to your script if computations are
	    ##### based on information gleaned from metadata, a publication, 
	    ##### or when they are not immediately obvious for other reasons
	    
	    ##### Yield #####
	  d2$biomass_total <- d2$`Biomass (t/ha)`
	    
	    d2$yield <- d2$`Jute fibre yield (t/ha)`
	    d2$residue_yield <- d2$`Sun dry Jute stick yield (kg/smpling area)`
	    #what plant part does yield refer to?
	    d2$yield_part <- "stem"
	    
	 d2 <- d2[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","season","country","season","site","adm1","longitude","latitude","treatment","crop","variety","planting_date","harvest_date","P_fertilizer","K_fertilizer","N_fertilizer","inoculated","inoculant","biomass_total","yield_part","residue_yield","yield")]
	  
###################################END OF Kharif I-Jute ##############################################################################################################################################################################################################################################################################
	 
#################################### Kharif I-Maize ###################################################################################################################################################################################################################################################################################
	 
	 r3 <- readxl::read_excel(f,sheet = "Kharif I-Maize") |> as.data.frame()	
	 
	 ## use a subset
	 d3 <- r3
	 
	 #### about the data #####
	 ## (TRUE/FALSE)
	 
	 d3$dataset_id <- dataset_id
	 d3$on_farm <- TRUE
	   d3$is_survey <- FALSE
	   d3$is_experiment <- TRUE
	   d3$irrigated <- TRUE
	   ## the treatment code	
	   d3$treatment <- d3$Tillage
	   
	   ##### Location #####
	 ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	 ## you can use carobiner::fix_name()
	 d3$country <- "Bangladash"
	   d3$site <- d3$District
	   d3$adm1 <- d3$`Site/Location/Node`
	   d3$adm2 <- NA
	   d3$adm3 <- NA
	   d3$elevation <- NA
	 ## each site must have corresponding longitude and latitude
	 ## see carobiner::geocode
	 d3$longitude <- d3$Longitude
	   d3$latitude <- d3$Latitude
	   
	   ##### Crop #####
	 ## normalize variety names
	 ## see carobiner::fix_name
	 d3$crop <- d3$Crop
	   d3$variety <- d3$Variety
	   d3$yield_part <- "maize"
	   
	   ##### Time #####
	 ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	 ## use 	as.character(as.Date()) for dates to assure the correct format.
	 d3$planting_date <- d3$`Date of sowing (mm/dd/yryr)`
	 d3$harvest_date  <- d3$`Date of harvesting`
	 
	 ##### Fertilizers #####
	 ## note that we use P and K, not P2O5 and K2O
	 ## P <- P2O5 / 2.29
	 ## K <- K2O / 1.2051
	 d3$P_fertilizer <- "15"
	   d3$K_fertilizer <-"10"
	   d3$N_fertilizer <- "30"
	   d3$S_fertilizer <- NA
	   d3$lime <- NA
	   ## normalize names 
	   d3$fertlizer_type <- NA
	   d3$inoculated <- FALSE
	 d3$inoculant <- NA
	   
	   ##### in general, add comments to your script if computations are
	   ##### based on information gleaned from metadata, a publication, 
	   ##### or when they are not immediately obvious for other reasons
	   
	   ##### Yield #####
	 d3$biomass_total <- d3$`Biomass (t/ha)`
	 d3$residue_yield <- d3$`Straw yield (t/ha)`  
	   d3$yield <- d3$`Grain yield (t/ha)`
	 
	 d3 <- d3[,c("dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","crop","variety","yield_part","planting_date","harvest_date","P_fertilizer","K_fertilizer","N_fertilizer","fertlizer_type","inoculated","inoculant","biomass_total","residue_yield","yield")]

################################ END OF Kharif I-Maize################################################################################################################################################################################################################################################################################
	 
################################### Boro rice ########################################################################################################################################################################################################################################################################################
	 
	 r4 <- readxl::read_excel(f,sheet = "Boro rice") |> as.data.frame()	 
	 
	 ## use a subset
	 d4 <- r4
	 
	 
	 #### about the data #####
	 ## (TRUE/FALSE)
	 
	 d4$dataset_id <- dataset_id
	 d4$on_farm <- TRUE
	   d4$is_survey <- FALSE
	   d4$is_experiment <-TRUE 
	   d4$irrigated <- TRUE
	   ## the treatment code	
	   d4$treatment <- d4$Tillage
	   
	   ##### Location #####
	 ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
	 ## you can use carobiner::fix_name()
	 d4$country <- "Bangladash"
	   d4$site <- d4$District
	   d4$adm1 <- d4$`Site/Location/Node`
	   d4$adm2 <- NA
	   d4$adm3 <- NA
	   d4$elevation <- NA
	 ## each site must have corresponding longitude and latitude
	 ## see carobiner::geocode
	 d4$longitude <- d4$Longitude
	   d4$latitude <- d4$Latitude
	   
	   ##### Crop #####
	 ## normalize variety names
	 ## see carobiner::fix_name
	 d4$crop <- d4$Crop
	   d4$variety <- d4$Variety
	   d4$yield_part <- "grain"
	   
	   ##### Time #####
	 ## time can be year (four characters), year-month (7 characters) or date (10 characters).
	 ## use 	as.character(as.Date()) for dates to assure the correct format.
	 d4$planting_date <- d4$`Date of sowing (mm/dd/yryr)`
	 d4$harvest_date  <- d4$`Date of harvesting`
	 
	 ##### Fertilizers #####
	 ## note that we use P and K, not P2O5 and K2O
	 ## P <- P2O5 / 2.29
	 ## K <- K2O / 1.2051
	 d4$P_fertilizer <- "20"
	   d4$K_fertilizer <-"15"
	   d4$N_fertilizer <- "30"
	   d4$S_fertilizer <- NA
	   d4$lime <- NA
	   ## normalize names 
	   d4$fertlizer_type <- NA
	   d4$inoculated <- FALSE
	 d4$inoculant <- NA
	 d4$season <- d4$Season
	   
	   ##### in general, add comments to your script if computations are
	   ##### based on information gleaned from metadata, a publication, 
	   ##### or when they are not immediately obvious for other reasons
	   
	   ##### Yield #####
	 d4$biomass_total <- d4$`Biomass (t/ha)`
	 d4$residue_yield <- d4$`Straw yield (t/ha)`  
	   d4$yield <-d4$`Grain yield (t/ha)` 
	   
	 d4 <- d4[,c( "dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","adm1","longitude","latitude","treatment","crop","variety","yield_part","planting_date","harvest_date","N_fertilizer","P_fertilizer","K_fertilizer","fertlizer_type","inoculated","inoculant","biomass_total","residue_yield","yield")]

	  	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

