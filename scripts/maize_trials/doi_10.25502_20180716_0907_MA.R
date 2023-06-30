#################################################################################
# Grain yield and other agronomic traits of international maize trials-Camerron 1989-2002
# Description: This is an international study that contains data on yield and other Agronomic
# traits of maize including borer and striga attacks on maize in Africa. 
# The study was carried out by the International Institute of Tropical Agriculture between 1989
# and 2015 in over thirty African countries. This dataset contains output of the research for Cameroon.
#################################################################################

carob_script <- function(path) {

	uri <- "doi:10.25502/20180716/0907/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Menkir, A. & Olufisola Oladipo, 2018. Grain Yield and Other Agronomic Traits of International Maize Trials - Borer [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180716/0907/MA",
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		experiment_type = "varieties",
		has_weather = FALSE

	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff, "international_maize_trial_cameroon_striga.csv", TRUE)
	d$striga_trial <- TRUE
	d$dataset_id <- dataset_id
	
	e <- mzfun(ff, "international_maize_trial_cameroon_regular.csv")
	e$striga_trial <- FALSE
	e$dataset_id <- dataset_id

	x <- carobiner::bindr(d, e)
	x$location[	x$location == "Cameroon"] <- NA
	x$description <- as.character(x$description)

	i <- which(x$location == "Mayo-Galke") 
	x$longitude[i] <- 14.2421
	x$latitude[i] <- 8.3868
	x$location[i] <- "Mayo-Galké"

	i <- which(x$location == "Nfonta") 
	x$longitude[i] <- 10.271
	x$latitude[i] <- 6.0178

	i <- which(x$location == "Djallingo") 
	x$longitude[i] <- 13.45833
	x$latitude[i] <- 9.245833


# all scripts must end like this
	carobiner::write_files(dset, x, path=path)
}