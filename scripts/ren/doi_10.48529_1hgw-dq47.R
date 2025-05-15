# R script for "carob"


carob_script <- function(path) {

"Nigeria: General Household Survey, Panel 2018-2019, Wave 4"

	uri <- "doi:10.48529/1hgw-dq47"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		publication = NA,
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-05-14"
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



