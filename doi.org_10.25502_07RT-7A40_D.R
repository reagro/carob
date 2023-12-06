# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy the abstract from the repo]

"

	uri <- "doi.org/10.25502/07RT-7A40/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "",
		data_institutions = "International Institute of Tropical Agriculture (IITA).",
		# e.g. "on-farm experiment", "survey", "compilation"
   		data_type="on-farm experiment", 
		carob_contributor="Shumirai Manzvera",
		# date of first submission to carob
		carob_date="2023-12-05",
		# name(s) of others who made significant improvements
		revised_by=""
	)

## download and read data 
	path <- "C:/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)

	
## process file(s)
	d <- n_and_p_maize_trial_2015_16

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$year

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Nigeria"
	d$site <- d$loc
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 8.675277000000051
	d$latitude <-  9.081999 

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d$variety <- d$variety

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- d$prate
   d$N_fertilizer <- d$nrate 
   
   
##### Yield #####
   
	d$yield <- d$yield
  d$asi <- d$ASI
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

