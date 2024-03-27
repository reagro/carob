

carob_script <- function(path) {
"Description:
This is an international study that contains data on Grain Yield and other Agronomic Traits of Extra-Early, Early Maturing Variety (White & Yellow) under Stress (Drought, Striga) in Africa.

The study was carried out by the International Institute of Tropical Agriculture between 2008 and 2016 in twelve African countries.
"

	uri <- "doi:10.25502/20180926/1840/BB"
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

	d <- mzfun(ff, sf="Sierra-leone.csv")
		
	carobiner::write_files(dset, d, path=path)

}
