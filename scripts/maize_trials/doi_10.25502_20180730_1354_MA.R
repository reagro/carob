
carob_script <- function(path) {
"
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Sierra Leone.
"


	uri <- "doi:10.25502/20180730/1354/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		uri = uri,
		data_citation = "Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Sierra Leone, 1990 - 2013 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180730/1354/MA",
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

	# d <- mzfun(ff, "international_maize_trial_sierra leone_borer.csv", TRUE)
	cat("  borer file to be done\n")
	e <- mzfun(ff, FALSE)

	#x <- carobiner::bindr(d, e)
	x <- e
	x$description[x$description == ""] <- NA
	x$yield <- suppressWarnings(as.numeric(x$yield))
	
	x$striga_infected <- NA
	x$dataset_id <- dataset_id
	x$grain_weight <- as.numeric(x$grain_weight)

	x$latitude[x$location == "Freetown"] <- 8.4542
	x$latitude[x$location == "Masita"] <- 8.29
	x$longitude[x$location == "Masita"] <- -13

# all scripts must end like this
	carobiner::write_files(dset, x, path=path)
}



