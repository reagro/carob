# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)]

"

	uri <- "hdl:11529/10547969"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Chowdhury, Apurba K.; Bhattacharya, Prateek M.; Das, K.K.; Dhar, Tapamay; Pradhan, K.; Sinha, A.K.; Ghosh, Arunava; Mitra, B.; Chattopadhyay, C., 2018, 2.7-Rabi (winter) crops-all nodes-Long term trial (LT)-Choochbehar-West Bengal, https://hdl.handle.net/11529/10547969, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "2.7-Rabi (winter) crops-all nodes-Long term trial (LT)-Choochbehar-West Bengal",
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda" 
	)

## download and read data 
  path <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	library(readxl)
	Maize_2014_15_5_LT_All_nodes_Coochbehar_data <- read_excel("data/raw/maize_trials/hdl_11529_10547969/Maize 2014-15-5-LT-All nodes-Coochbehar.data.xlsx", 
	                                                           +     sheet = "14 - Grain Harvest ")
	

	
## process file(s)

## use a subset
	d <- Maize_2014_15_5_LT_All_nodes_Coochbehar_data

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- d$Tmnt
	d$trial_id <- d$`Trial Code`
  d$residue_yield <- d$`Straw yield (t/ha)`
  d$season <- d$Season
  d$plant_spacing <- d$`Distance between 4 adjacent rows (m)`
  
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "India"
	d$site <- "Choochbehar"
	d$adm1 <- "West Bengal"
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 89.451088
	d$latitude <- 26.323921

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "Maize"
	d$variety <- NA

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- NA
   d$K_fertilizer <-NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- 
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- d$`Biomass (t/ha)`

	d$yield <- d$`Grain yield (t/ha)`
	#what plant part does yield refer to?
	d$yield_part <- "Grain"
	
	d <- d[, c("country", "adm1","site","longitude","latitude","season","trial_id","treatment","crop","plant_spacing","yield_part","biomass_total","residue_yield","yield")]
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

