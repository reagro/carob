#################################################################################
# Project name: Grain yield and other agronomic traits of international maize trials-Zimbabwe-1989-2008
# Description: This is an international study that contains data on yield and 
# other agronomic traits of maize including striga attacks on maize in Africa. 
# The study was carried out by the International 
# Institute of Tropical Agriculture in 2016 in eight African countries and one asian country
#################################################################################


carob_script <- function(path) {

	uri <- "doi:10.25502/20180730/0930/MA"
	dataset_id <- agro::get_simple_URI(uri)
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

	cat("to be done\n") 
	return(TRUE)
	
	mzfun <- carobiner::get_function("intmztrial_borer", path, group)

	d <- mzfun(ff, "international_maize_trial_guinea_borer.csv", borer=TRUE)
	d$striga_trial <- "yes"
	d$dataset_id <- dataset_id
	
	e <- mzfun(ff, "international_maize_trial_guinea_regular.csv")
	e$striga_trial <- "no"
	e$dataset_id <- dataset_id

	x <- carobiner::bindr(d, e)

	x$location[x$location=='Harare1'] <- "Harare"
	x$longitude[x$location=='Harare'] <- 31.05
	x$latitude[x$location=='Harare'] <- -17.83

# all scripts must end like this
	carobiner::write_files(dset, x, path, dataset_id, group)
}

