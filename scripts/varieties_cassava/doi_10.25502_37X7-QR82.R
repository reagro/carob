# R script for "carob"
# license: GPLv3

carob_script <- function(path) {
  
"Assessment of Varieties (Polyploid) of Cassava for high yield, disease resistance in a Uniform Yield Trial"
  
	uri <- "doi:10.25502/37X7-QR82"
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
		carob_date = "2024-09-16",
		notes = NA
	)

	process_cassava <- carobiner::get_function("process_cassava", path, group)
	f <- grep(".csv$", ff, value=TRUE)
	
	d <- process_cassava(f, "Nigeria")

	carobiner::write_files(path = path, metadata = meta, records = d)
}

