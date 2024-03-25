

carob_script <- function(path) {
"Description:
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"
				
	uri <- "doi:10.25502/20180730/0912/MA"
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

	d$dataset_id <- dataset_id

	d$country[d$location == "Oyo"] <- "Nigeria" #?!
	i <- d$location == "Babile"
	d$longitude[i] <- -2.83
	i <- d$location == "Wa"
	d$longitude[i] <- -2.5
	i <- d$location == "Manga"
	d$longitude[i] <- -0.16
	i <- d$location == "Yendi"
	d$longitude[i] <- 0

	carobiner::write_files(dset, d, path=path)

}
