# R script for "carob"
# license: GPLv3

carob_script <- function(path) {
  
"Assessment of Varieties (Polyploid) of Cassava for high yield, disease resistance in a Uniform Yield Trial (19 clones) in Adeta, Togo 1996/97 Breeding Season"
  
	uri <- "doi:10.25502/8DRG-6A47"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)
		
	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=3),
		data_institute = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		carob_date = "2024-09-16",
		notes = NA
	)

	process_cassava <- carobiner::get_function("process_cassava", path, group)
	d <- process_cassava(ff)
	carobiner::write_files(path = path, metadata = meta, records = d$records, timerecs=d$timerecs)
}

