
carob_script <- function(path) {
"
Grain yield and other agronomic traits of international maize trials-Camerron 1989-2002
Description: This is an international study that contains data on yield and other Agronomic
traits of maize including borer and striga attacks on maize in Africa. 
The study was carried out by the International Institute of Tropical Agriculture between 1989
and 2015 in over thirty African countries. This dataset contains output of the research for Cameroon.
"

	uri <- "doi:10.25502/20180716/0907/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Menkir, A. & Olufisola Oladipo, 2018. Grain Yield and Other Agronomic Traits of International Maize Trials - Borer [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180716/0907/MA",
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		carob_date="2021-06-02",
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
	
	d$location[	d$location == "Cameroon"] <- NA
	d$description <- as.character(d$description)

	i <- which(d$location == "Mayo-Galke") 
	d$longitude[i] <- 14.2421
	d$latitude[i] <- 8.3868
	d$location[i] <- "Mayo-Galké"

	i <- which(d$location == "Nfonta") 
	d$longitude[i] <- 10.271
	d$latitude[i] <- 6.0178

	i <- which(d$location == "Djallingo") 
	d$longitude[i] <- 13.45833
	d$latitude[i] <- 9.245833


# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}
