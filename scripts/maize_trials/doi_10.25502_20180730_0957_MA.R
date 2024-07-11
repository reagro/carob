# R script for "carob"



carob_script <- function(path) {
"
This is an international study that contains data on yield and other agronomic traits of maize including striga attacks on maize in Africa.

The study was carried out by the International Institute of Tropical Agriculture in 2016 in eight African countries and one asian country.
"

	uri <- "doi:10.25502/20180730/0957/MA"
	group <- "maize_trials"	
	ff <- carobiner::get_data(uri, path, group)
		

	meta <- data.frame(
		carobiner::read_metadata(uri, path, major=2, minor=1, group),
 	    publication="doi:10.1016/j.jenvman.2017.06.058",
		carob_contributor = "Robert Hijmans",
		carob_date="2023-07-03",
		data_type = "experiment",
		treatment_vars = "variety",
		project="International Maize Trials",
		data_institute="IITA"
	)


	mzfun <- carobiner::get_function("intmztrial_striga", path, group)

	d <- mzfun(ff)
	
	
	carobiner::write_files(meta, d, path=path)

}
