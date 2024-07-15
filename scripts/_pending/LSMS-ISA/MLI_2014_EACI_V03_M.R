# R script for "carob"


carob_script <- function(path) {

return( TRUE)


"
	https://microdata.worldbank.org/index.php/catalog/2583/download/47803
	Description:


"

	ht  <- httr::GET(url)

	uri <- "doi:______"
	group <- ""
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
	   publication="publication doi",
	   contributor="Your name",
	   data_type="___"
 	)




	f <- ff[basename(ff) == "_____________"]

	d <- read.csv(f)
	d <- readxl::read_excel(f) |> as.data.frame()
	
	# process file(s)
	d <- carobiner::change_names(d, from, to)
	

	carobiner::write_files(path, meta, d)
	TRUE
}
