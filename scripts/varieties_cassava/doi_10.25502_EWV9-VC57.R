# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Assessment of White root varieties of Cassava for high yield, high dry matter and disease resistance using NCRP Trial (16 clones) in Abuja 2017/2018 Breeding Season from 2015uyt30whtroot &2015ncrp yrt ikenne AB."
  
	uri <- "doi:10.25502/EWV9-VC57"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)
		
	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=3),
		data_organization = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		carob_date = "2024-09-18",
		notes = NA
	)

	process_cassava <- carobiner::get_function("process_cassava", path, group)
	d <- process_cassava(ff)
	carobiner::write_files(path = path, metadata = meta, records = d$records, timerecs=d$timerecs)
}

