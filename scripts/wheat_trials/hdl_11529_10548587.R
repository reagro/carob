# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2020)]

"

	uri <- "hdl:11529/10548587"
	path <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2021, 28th High Rainfall Wheat Yield Trial, https://hdl.handle.net/11529/10548587, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "28th High Rainfall Wheat Yield Trial",
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)


	library("readxl")
	X28TH_HRWYT_GrnYld_xls <- read_excel("data/raw/wheat_trials/hdl_11529_10548587/28TH HRWYT_GrnYld.xls.xlsx")

	
## process file(s)

## use a subset
	d <- X28TH_HRWYT_GrnYld_xls

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$country <- d$Country
	d$planting_date <- d$Cycle
	d$yield <- d$Value

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
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####

	#what plant part does yield refer to?
	d$yield_part <- "grain" 
   
   d <- d[, c("country","planting_date","crop","yield_part","yield")]
   
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

