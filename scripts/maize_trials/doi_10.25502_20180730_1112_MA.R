
# borer to be done

carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"
	uri <- "doi:10.25502/20180730/1112/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Nigeria, 1989 - 2015 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180730/1112/MA",
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
	d$dataset_id <- dataset_id
	
	i <- d$location == "Mokwa"
	d$longitude[i] <- 5.06
	d$latitude[i] <- 9.29

	i <- d$location == "Kaduna"
	d$longitude[i] <- 7.45
	d$latitude[i] <- 10.6

	i <- d$location == "Dansadau"
	d$longitude[i] <- 8
	d$latitude[i] <- 10

	i <- d$location == "Ikorodu"
	d$longitude[i] <- 3.50
	d$latitude[i] <- 6.59

	i <- grep("Gusau", d$location)
	d$longitude[i] <- 7.08
	d$latitude[i] <- 12.03

	i <- d$location == "Kilissi"
	d$country[i] <- "Guinea"
	d$longitude[i] <- -12.6 
	d$latitude[i] <- 9.64 

	i <- d$location == "Sinematiali"
	d$country[i] <- "Côte d'Ivoire"
	
	i <- d$location == "Niaouli"
	d$country[i] <- "Benin"
	
	i <- d$location == "Touboro"
	d$country[i] <- "Cameroon"
	
	carobiner::write_files(dset, d, path=path)

}