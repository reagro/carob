# R script for "carob"
# license: GPLv3

## Not included because there is no yield data or other response variable


carob_script <- function(path) {
  
"Assessment of Varieties (Polyploid) of Cassava for high yield, disease resistance in a Uniform Yield Trial"
  
	uri <- "doi:10.25502/T0MA-YP66"
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

	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")
	carobiner::write_files(path, meta)
}

