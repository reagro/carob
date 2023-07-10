

carob_script <- function(path) {
"Description:
This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries.
"

	uri <- "doi:10.25502/20180925/1812/BB"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		## the source says "Gambia" but it is "Ghana", fixed
		data_citation="Badu-Apraku, B. (2018). International Maize Trials – Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) - Ghana, 2008 - 2012 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180925/1812/BB",
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

	d <- mzfun(ff, sf="Ghana.csv")
	d$dataset_id <- dataset_id	
	
	i <- d$location == "Babile"
	d$longitude[i] <- -2.83
	i <- d$location == "Wa"
	d$longitude[i] <- -2.5
	i <- d$location == "Manga"
	d$longitude[i] <- -0.16
	
	carobiner::write_files(dset, d, path=path)

}