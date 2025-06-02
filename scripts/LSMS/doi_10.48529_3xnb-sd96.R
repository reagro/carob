# R script for "carob"


carob_script <- function(path) {

"Niger: National Survey on Household Living Conditions and Agriculture 2014, Wave 2 Panel Data"

	uri <- "doi:10.48529/3xnb-sd96"
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



