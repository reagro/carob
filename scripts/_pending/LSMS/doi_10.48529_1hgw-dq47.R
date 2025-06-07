# R script for "carob"


carob_script <- function(path) {

"Nigeria: General Household Survey, Panel 2018-2019, Wave 4"

	uri <- "doi:10.48529/1hgw-dq47"
	group <- "LSMS"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::get_metadata(uri, path, group, major=1, minor=0),
		publication = NA,
		carob_contributor = "Robert Hijmans",
		completion = 0,
		carob_date = "2025-05-14"
	)
	
	
	hg <- read.csv(ff[basename(ff) == "nga_householdgeovars_y4.csv"])

	geo <- data.frame(
		hhid = hg$hhid,
		latitude = hg$lat_dd_mod,
		longitude = hg$lon_dd_mod
	)

	p1 <- read.csv(ff[basename(ff) == "sect1_plantingw4.csv"])




	return(TRUE)
	carobiner::write_files(path, meta, d)
}



