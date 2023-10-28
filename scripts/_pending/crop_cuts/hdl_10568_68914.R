# R script for "carob"

## ISSUES
# ....

## RH: are these field data?
## RH: there is no file at that hdl


carob_script <- function(path) {

"Description:

    The Global Yield Gap Atlas project (GYGA - http://yieldgap.org ) has undertaken a yield gap assessment following the protocol recommended by van Ittersum et. al. (van Ittersum et. al., 2013). This datafile holds the results for rainfed millet.
"

	uri <- "hdl:10568/68914"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="The Global Yield Gap Atlas",
		uri=uri,
		data_citation="NA",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "NA",
		data_institutions = "The Global Yield Gap Atlas ",
   		data_type="experiment",
		carob_contributor="Shumirai Manzvera",
		carob_date="2023-09-19"
	)

## download and read data 

	f <- "C:/carob/data/raw/crop_cuts/hdl:10568/68914"
	path<- "C:/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)
	

	r <- GygaRainfedMillet
	

	
## process file(s)

## use a subset
	d <- r

	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <-TRUE 
	d$irrigated <- FALSE


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$COUNTRY
	d$site <- d$STATIONNAME
	
	d$elevation <- d$ELEVATION_METER
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- d$LONGITUDE
	d$latitude <- d$LATITUDE

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <-d$CROP 
 





##### Yield #####


	d$yield <- d$YA
	
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

