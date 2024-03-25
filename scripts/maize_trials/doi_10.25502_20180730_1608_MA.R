
carob_script <- function(path) {
"This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Zimbabwe.
"

	uri <- "doi:10.25502/20180730/1608/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
		
	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		carob_date="2021-06-03",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)


	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	d$dataset_id <- dataset_id
	

	d$location[d$location=='Harare1'] <- "Harare"
	d$longitude[d$location=='Harare'] <- 31.05
	d$latitude[d$location=='Harare'] <- -17.83
	d$description <- as.character(d$description)

	carobiner::write_files(dset, d, path=path)
}

