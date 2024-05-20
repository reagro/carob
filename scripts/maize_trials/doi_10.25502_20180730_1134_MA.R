
carob_script <- function(path) {
"
This is an international study that contains data on yield and other Agronomic traits of maize including borer and striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 1989 and 2015 in over thirty African countries.

This dataset contains output of the research for Republic of Benin.
"

	uri <- "doi:10.25502/20180730/1134/MA"
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)
		

	dset <- data.frame(
		carobiner::read_metadata(uri, path, major=2, minor=1, group),
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_date="2023-07-03",
		data_type = "experiment",
		treatment_vars = "variety;location",
		project="International Maize Trials",
		data_institutions="IITA"
	)

	

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	
	d$country <- "Benin"
	
	i <- d$location == "Bagou"
	d$latitude[i] <- 10.70
	
	carobiner::write_files(dset, d, path=path)

}



