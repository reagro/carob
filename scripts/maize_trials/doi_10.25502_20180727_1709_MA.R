
# border tbd

carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Mali."
				
	uri <- "doi:10.25502/20180727/1709/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1, group)
		
	## dataset level data 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_date="2023-07-03",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)


	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	d$country <- "Democratic Republic of the Congo"
	d$dataset_id <- dataset_id
	
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
	
	i <- d$location == "Kashila"
	#Kashile
	d$longitude[i] <- 21.88 
	d$latitude[i] <- -5.17
	
	i <- d$location == "M'vuazi"
	d$longitude[i] <- 14.886
	d$latitude[i] <- -5.439
	
	# Mankewa, (alluvial area of M'vuazi river) IITA 1978 annual report
	i <- d$location == "Mankewa"
	d$longitude[i] <- 14.886
	d$latitude[i] <- -5.439


	carobiner::write_files(dset, d, path=path)
}
