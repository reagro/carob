# R script for "carob"


carob_script <- function(path) {

"Niger: National Survey on Household Living Conditions and Agriculture 2011"

	uri <- "doi:10.48529/bp16-s524"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=0),
		publication = NA,
		carob_contributor = "Robert Hijmans",
		completion = 0,
		carob_date = "2025-05-14"
	)

	return(TRUE)
	carobiner::write_files(path, meta, d)
}



