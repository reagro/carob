# R script for "carob"
# license: GPLv3

carob_script <- function(path) {

"Assessment of Yellow root Varieties of Cassava for high yield and disease resistance  using 126 clones at ibadan in  1995/96 Breeding Season"
  
	uri <- "doi:10.25502/JJTX-8W04"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)
		
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=3),
		data_institute = "IITA",
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

