# R script for "carob"


carob_script <- function(path) {

"Uganda: National Panel Survey 2005-2009"

	uri <- "doi:10.48529/pnd4-7d68"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=0),
		publication = NA,
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-05-14"
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



