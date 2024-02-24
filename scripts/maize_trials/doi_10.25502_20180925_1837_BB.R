

carob_script <- function(path) {
"Description:
This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries.
"

	uri <- "doi:10.25502/20180925/1837/BB"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Badu-Apraku, B. (2018). International Maize Trials – Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) - Mali, 2008 - 2013 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180925/1837/BB",
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_date="2023-07-03",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)


	## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)
	d <- mzfun(ff, sf="Mali.csv")
	d$dataset_id <- dataset_id	
	
	i <- d$location == "Sapu"
	d$country[i] <- "Gambia"

	i <- d$location == "N'tarla"
	d$longitude[i] <- -5.69
	d$latitude[i] <- 12.621
	
	carobiner::write_files(dset, d, path=path)
}
