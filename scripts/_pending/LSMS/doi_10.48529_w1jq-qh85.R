# R script for "carob"


carob_script <- function(path) {

"Malawi: Third Integrated Household Survey 2010-2011"

	uri <- "doi:10.48529/w1jq-qh85"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		carob_contributor = "Robert Hijmans",
		completion = 0,
		carob_date = "2025-05-14"
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



