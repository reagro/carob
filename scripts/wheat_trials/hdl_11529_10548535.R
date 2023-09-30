# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [The Semi-Arid Wheat Yield Trial (SAWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to low rainfall, drought prone environments typically receiving less than 500 mm of water available during the cropping cycle. The combination of water-use efficiency and water responsive broad adaptation plus yield potential is important in drought environments where rainfall is frequently erratic across and within years. Stripe rust, leaf rust and stem rust, root rots, nematodes, and bunts are the key biotic constraints. Typical target environments include winter rain or Mediterranean-type drought associated with post-flowering moisture stress and heat stress such as those found at Aleppo (Syria), Settat (Morocco) and Marcos Juarez (Argentina), all classified by CIMMYT within Wheat Mega Environment 4 (Low rainfall, semi-arid environment; ME4: SA). It is distributed to 150 locations, and contains 50 entries. (2019)]

"

	uri <- "hdl:11529/10548535"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2020, 27th Semi-Arid Wheat Yield Trial, https://hdl.handle.net/11529/10548535, CIMMYT Research Data & Software Repository Network, V4",
		publication= "27th Semi-Arid Wheat Yield Trial",
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda"  
	)

## download and read data 

	path <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)
	dset$license <- carobiner::get_license(js)

	library("readxl")
	X27TH_SAWYT_RawData <- read_excel("data/raw/wheat_trials/hdl_11529_10548535/27TH SAWYT_RawData.xlsx")

	
## process file(s)

## use a subset
	d <- X27TH_SAWYT_RawData

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE 
	d$irrigated <- FALSE
## the treatment code	
	d$country <- d$Country
	d$rep <- d$Rep
	d$harvest_date <- d$Cycle
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <-NA 
	d$site <- NA
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- NA
	d$latitude <- NA

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "wheat"
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
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- NA
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- TRUE/FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- NA 

	d$yield <- d$Value
	#what plant part does yield refer to?
	d$yield_part <- "grain" 
	
	d <- d[, c("country","harvest_date","crop","rep","yield_part","yield")]
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

