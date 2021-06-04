#################################################################################
# Grain yield and other agronomic traits of international maize trials-Camerron 1989-2002
# Description: This is an international study that contains data on yield and other Agronomic
# traits of maize including borer and striga attacks on maize in Africa. 
# The study was carried out by the International Institute of Tropical Agriculture between 1989
# and 2015 in over thirty African countries. This dataset contains output of the research for Cameroon.
#################################################################################

carob_script <- function(path) {
	uri <- "doi:10.25502/20180716/0907/MA"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "international_maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		uri = uri,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		contributor = "Camila Bonilla",
		experiment_type = "varieties",
		has_weather = FALSE,
		has_management = FALSE
	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	mzfun <- carobiner::get_function("intmztrial", path, group)

	d <- mzfun(ff, "international_maize_trial_cameroon_striga.csv", TRUE)
	d$striga_trial <- "yes"
	d$dataset_id <- dataset_id
	
	e <- mzfun(ff, "international_maize_trial_cameroon_regular.csv")
	e$striga_trial <- "no"
	e$dataset_id <- dataset_id

	x <- carobiner::bindr(d, e)
	x$location[	x$location == "Cameroon"] <- ""

# all scripts must end like this
	carobiner::write_files(dset, x, path, dataset_id, group)
}