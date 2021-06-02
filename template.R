# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:


"

	uri <- "doi:______"
	dataset_id <- carobiner::clean_uri(uri)
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   uri=uri,
	   publication="publication doi",
	   contributor="Your name",
	   experiment_type="___",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- agro::get_data_from_uri(uri, path=file.path(path, "data/raw"))
	js <- carobiner::get_metadata(dataset_id, path, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "_____________"]

	d <- read.csv(f)
	d <- readxl::read_excel(f) |> as.data.frame()
	
	# process file(s)
	d <- carobiner::change_names(d, from, to)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id)
}
