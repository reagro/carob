# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy the abstract from the repo]

"

	uri <- "doi.org_10.25502_fbgw-1m42_d"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
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
		data_institutions = "International Institute of Tropical Agriculture (IITA)",
   		data_type= "compilation",
		carob_contributor="Shumirai Manzvera"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	path <- "C:/carob"

	f <-"C:/carob/data/raw/conservation_agriculture/doi.org_10.25502_fbgw-1m42_d"
	
## process file(s)

## use a subset
	d <- sorghum_biomass_sampling_csv
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- Uganda
	 
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 32.29028
	d$latitude <- 1.37333

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "sorghum"

##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_stems <- d$Yield_straw_dry_ton_ha_1

	d$yield <- d$Yield_grain_dry_ton_ha_1
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	d$plant_height <-d$Avg_Hgt_in_quadrant_m
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
 path <- "C:/carob"

