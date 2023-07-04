
# border tbd

carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Mali."
				
	uri <- "doi:10.25502/20180727/1709/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – DR Congo, 1996 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180727/1709/MA",
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

	d <- mzfun(ff, FALSE)
	e <- mzfun(ff, TRUE)
	d <- carobiner::bindr(d, e)
	
	i <- d$location == "Gandajika"
	d$longitude[i] <- 23.95
	d$latitude[i] <- -6.750
	
	i <- d$location == "Kipopo"
	d$longitude[i] <- 17.83
	d$latitude[i] <- -4.970
	
	i <- d$location == "Mangongo"
	d$longitude[i] <- 23.744
	d$latitude[i] <- 0.9257

	i <- d$location == "Kalemba"
	d$longitude[i] <- 22.217
	d$latitude[i] <- -6.867
	
	d$country <- "Democratic Republic of the Congo"
	d$dataset_id <- dataset_id
	carobiner::write_files(dset, d, path=path)

}