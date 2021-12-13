#################################################################################
# Project name: Grain yield and other agronomic traits of international maize trials-Benin-2016
# Description: This is an international study that contains data on yield and 
# other agronomic traits of maize including striga attacks on maize in Africa. 
# The study was carried out by the International 
# Institute of Tropical Agriculture in 2016 in eight African countries and one asian country
#################################################################################

carob_script <- function(path) {
	uri <- "doi:10.25502/20180830/1745/MA"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "international_maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		uri = uri,
		group=group,
 	    publication="doi:10.2135/cropsci2018.03.0168",
		carob_contributor = "Camila Bonilla",
		experiment_type = "varieties",
		has_weather = FALSE,
		has_management = FALSE
	)

	cat("download failed\n")
	return(TRUE)
	
	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	if (is.null(ff)) {
		cat("download failed\n")
		return(TRUE)
	}
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	mzfun <- carobiner::get_function("intmztrial", path, group)
	TRUE
}

