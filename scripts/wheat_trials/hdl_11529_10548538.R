# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [The Elite Selection Wheat Yield Trial (ESWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents the optimally irrigated, low rainfall areas. Major stresses include leaf, stem and yellow rusts, Karnal bunt, and lodging. Representative areas include the Gangetic Valley (India), the Indus Valley (Pakistan), the Nile Valley (Egypt), irrigated river valleys in parts of China (e.g. Chengdu), and the Yaqui Valley (Mexico). This ME encompasses 36 million hectares spread primarily over Asia and Africa between 350S -350N latitudes. White (amber)-grained types are preferred by consumers of wheat in the vast majority of the areas. It is distributed to upto 200 locations and contains 50 entries. (2019)]"

	uri <- "hdl:11529/10548538"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id, 
		group=group,
		project=NA,
		uri=uri,
		data_citation="Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2020, 40th Elite Selection Wheat Yield Trial, https://hdl.handle.net/11529/10548538, CIMMYT Research Data & Software Repository Network, V3",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "40th Elite Selection Wheat Yield Trial",
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda"  
	)

## download and read data 
	
  path <- "C:/Users/Cimmyt/Documents/WORKING DIRECTORY/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)
	dset$license <- carobiner::get_license(js)

	library(readxl)
	X40TH_ESWYT_GrnYld_xls <- read_excel("data/raw/wheat_trials/hdl_11529_10548538/40TH ESWYT_GrnYld.xls.xlsx")

	
## process file(s)

## use a subset
	d <- X40TH_ESWYT_GrnYld_xls

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- FALSE
	d$irrigated <- FALSE
	
## the treatment code	
	d$country <- d$Country
	d$planting_date <- d$Cycle
	d$record_id <- d$Sid
	

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- NA
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
   d$K_fertilizer <-NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- NA
## normalize names 
   d$fertlizer_type <-NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <-NA

	d$yield <- d$Value
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	
	d <- d[,c("country","planting_date","record_id","crop","yield_part","yield")]
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)#
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

