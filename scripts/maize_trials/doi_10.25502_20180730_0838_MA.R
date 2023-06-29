#################################################################################
# Project name: Grain yield and other agronomic traits of international maize trials-Gambia-1993-2015
# Description: This is an international study that contains data on yield and 
# other agronomic traits of maize including striga attacks on maize in Africa. 
# The study was carried out by the International 
# Institute of Tropical Agriculture in 2016 in eight African countries and one Asian country
#################################################################################

carob_script <- function(path) {

	uri <- "doi:10.25502/20180730/0838/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		data_citation="Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Gambia, 1993 - 2015 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180730/0838/MA",
		data_institutions = "IITA",
		group=group,		
		uri = uri,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		experiment_type = "varieties",
		has_weather = FALSE,
		has_management = FALSE
	)
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff, "international_maize_trial_gambia_striga.csv", TRUE)
	d$striga_trial <- TRUE
	d$dataset_id <- dataset_id
	d$planting_date[d$planting_date == 215] <- 2015
	
	e <- mzfun(ff, "international_maize_trial_gambia_regular.csv")
	e$striga_trial <- FALSE
	e$dataset_id <- dataset_id

	x <- carobiner::bindr(d, e)

	suppressWarnings(x$sl <- as.numeric(x$sl))
	suppressWarnings(x$rl <- as.numeric(x$rl))

	x$description <- as.character(x$description)
# all scripts must end like this
	carobiner::write_files(dset, x, path=path)
}