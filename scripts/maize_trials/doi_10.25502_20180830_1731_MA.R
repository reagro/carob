
carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"

	uri <- "doi:10.25502/20180830/1731/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Nigeria, 2016 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180830/1731/MA",
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)

	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	d$dataset_id <- dataset_id
	
	#lonlat reversed
	i <- d$location == "Eruwa"
	d$longitude[i] <- 3.42
	d$latitude[i] <- 7.53
	# presumably also reversed
	i <- d$location == "Agwan-Millan"
	d$longitude[i] <- 4.48
	d$latitude[i] <-  12.54
	
	i <- d$location == "Angaradebou"
	d$country[i] <- "Benin"
	
	i <- d$location == "Ikorodu"
	d$longitude[i] <- 3.5
	d$latitude[i] <-  6.59
	
	carobiner::write_files(dset, d, path=path)
}