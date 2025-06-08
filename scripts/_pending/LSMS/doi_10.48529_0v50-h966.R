# R script for "carob"


carob_script <- function(path) {

"Mali: Enquête Agricole de Conjoncture Intégrée aux Conditions de Vie des Ménages 2017"

	uri <- "doi:10.48529/0v50-h966"
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



