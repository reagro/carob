# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "doi:______"
	dataset_id <- agro::get_simple_URI(uri)
	group <- ""
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication= "",
	   data_institutions = "",
	   carob_contributor="Your name",
	   
	   ## something like randomized control...
	   experiment_type="___",
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "_____________"]

	d <- read.csv(f)
	d <- readxl::read_excel(f) |> as.data.frame()
	
	# process file(s)
	d <- carobiner::change_names(d, from, to)
	d$dataset_id <- dataset_id

#   d$longitude <- 
#   d$latitude <- 


# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}

## now test your function in a clean environment 
# path <- _____
# carob_script(path)

