
# border tbd

carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Kenya."
				
	uri <- "doi:10.25502/20180730/0942/MA"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "maize_trials"	
		
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri = uri,
		data_citation="Menkir, A. (2018). Grain Yield and Other Agronomic Traits of International Maize Trials – Kenya, 1989 - 2013 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180730/0942/MA",
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

	d$trial_name[d$trial_name == ""] <- "not specified"
	
	carobiner::write_files(dset, d, path=path)

}