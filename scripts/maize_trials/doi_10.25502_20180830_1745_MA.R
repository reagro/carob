#################################################################################
# Project name: Grain yield and other agronomic traits of international maize trials-Benin-2016
# Description: This is an international study that contains data on yield and 
# other agronomic traits of maize including striga attacks on maize in Africa. 
# The study was carried out by the International 
# Institute of Tropical Agriculture in 2016 in eight African countries and one asian country
#################################################################################

carob_script <- function(path) {

	uri <- "doi:10.25502/20180830/1745/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		data_citation = "Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Republic of Benin, 2016 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180830/1745/MA",
		uri = uri,
		group=group,
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)

	
	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff, "Republic_of_benin_international_maize_trials_Regular2016.csv", FALSE)
	d$striga_trial <- TRUE
	d$dataset_id <- dataset_id
	d$country <- "Benin"
	d$yield <- 1000 * d$yield

	d$x_1000gwt <- NULL
	d$description <- as.character(d$description)
	d$yield2 <- as.numeric(d$yield2)

#	e <- mzfun(ff, "international_maize_trial_tanzania_regular.csv")
#	e$striga_trial <- "no"
#	e$dataset_id <- dataset_id
#	x <- carobiner::bindr(d, e)
#	x$longitude[x$localition=='Mlingano'] <- 38.86
#	x$latitude[x$localition=='Katrin'] <- -4.03


# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}



