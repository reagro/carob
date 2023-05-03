#################################################################################
# Project name: Grain yield and other agronomic traits of international maize trials-Benin-2016
# Description: This is an international study that contains data on yield and 
# other agronomic traits of maize including striga attacks on maize in Africa. 
# The study was carried out by the International 
# Institute of Tropical Agriculture in 2016 in eight African countries and one asian country
#################################################################################

carob_script <- function(path) {


	uri <- "doi:10.25502/20180730/1354/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "international_maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		uri = uri,
		group=group,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		experiment_type = "varieties",
		has_weather = FALSE,
		has_management = FALSE
	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)
	
	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	# d <- mzfun(ff, "international_maize_trial_sierra leone_borer.csv", TRUE)
	cat("  borer file to be done\n")
	e <- mzfun(ff, "international_maize_trial_sierra leone_regular.csv")


	#x <- carobiner::bindr(d, e)
	x <- e
	x$description[x$description == ""] <- NA
	x$yield <- suppressWarnings(as.numeric(x$yield))
	
	x$striga_trial <- FALSE
	x$striga_infected <- NA
	x$dataset_id <- dataset_id
	x$yield2 <- as.numeric(x$yield2)
	x$grain_weight <- as.numeric(x$grain_weight)


# all scripts must end like this
	carobiner::write_files(dset, x, path, dataset_id, group)
}



