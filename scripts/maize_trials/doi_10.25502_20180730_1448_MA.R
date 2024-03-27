

carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"

	uri <- "doi:10.25502/20180730/1448/MA"
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)
		
	## dataset level data 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, major=2, minor=1, group),
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_date="2023-07-03",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"
	)


	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	
	
	# lon/lat reversed
	i <- d$location == "Wad-Medan"
	d$longitude[i] <- 33.48
	d$latitude[i] <- 14.4

	
	carobiner::write_files(dset, d, path=path)
}
