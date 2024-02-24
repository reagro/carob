
carob_script <- function(path) {
"
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Sierra Leone.
"


	uri <- "doi:10.25502/20180730/1354/MA"
	group <- "maize_trials"	

	dataset_id <- carobiner::simple_uri(uri)
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
		
	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		data_citation = "Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Sierra Leone, 1990 - 2013 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180730/1354/MA",
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		carob_date="2021-06-03",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)
	d <- mzfun(ff, id=dataset_id)

	d$latitude[d$location == "Freetown"] <- 8.4542
	d$latitude[d$location == "Masita"] <- 8.29
	d$longitude[d$location == "Masita"] <- -13

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}



