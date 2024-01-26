

carob_script <- function(path) {
"Description:
This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries.
"

	uri <- "doi:10.25502/20180925/1906/BB"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Badu-Apraku, B. (2018). International Maize Trials – Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) - Nigeria, 2008 - 2016 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180925/1906/BB",
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

	d <- mzfun(ff, sf="Nigeria.csv")
	d$dataset_id <- dataset_id	
	d$yield[d$yield > 20000] <- NA

	#lonlat reversed
	i <- d$location == "Eruwa"
	d$longitude[i] <- 3.42
	d$latitude[i] <- 7.53
	
	i <- d$location == "Zaria"
	d$longitude[i] <- 7.717
	d$latitude[i] <- 11.131

	i <- d$location == "Tofa"
	d$longitude[i] <- 8.27
	d$latitude[i] <- 12.06 
	
	i <- d$location == "Mokwa"
	d$longitude[i] <- 5.06
	d$latitude[i] <- 9.29

	i <- d$location == "Zuru"
	d$longitude[i] <- 5.219
	d$latitude[i] <- 11.432

	i <- d$location=="Ikenne"
	d$longitude[i] <- 3.698
	d$latitude[i] <- 6.901

	i <- d$location == "Bagauda"
	d$longitude[i] <- 8.385
	d$latitude[i] <- 11.570
	
	i <- d$location == "Ejiba"
	d$longitude[i] <- 5.640
	d$latitude[i] <- 8.298

	i <- d$location == "Tamba Daura"
	d$longitude[i] <- 8.311
	d$latitude[i] <- 13.016
		
	i <- d$location == "Angaradebou"
	d$country[i] <- "Benin"
	
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(dset, d, path=path)

}
