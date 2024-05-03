
carob_script <- function(path) {

"Grain yield and other agronomic traits of international maize trials-Gambia-1993-2015
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa. The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one Asian country"

	uri <- "doi:10.25502/20180730/0838/MA"
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, major=2, minor=1, group),
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Camila Bonilla",
		carob_date="2021-06-03",
		data_type = "experiment",
		project="International Maize Trials",
		data_institutions="IITA"

	)

	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	
	d$planting_date[d$planting_date == 215] <- 2015
	
#	suppressWarnings(x$sl <- as.numeric(x$sl))
#	suppressWarnings(x$rl <- as.numeric(x$rl))

#	x$description <- as.character(x$description)
	carobiner::write_files(dset, d, path=path)
}
