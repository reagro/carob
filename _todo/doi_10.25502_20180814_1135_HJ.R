# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "doi:10.25502/20180814/1135/HJ"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="https://doi.org/10.25502/20180814/1135/HJ",
	   data_citation = "",
	   data_institutions = "",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="___",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group)
	dset$license <- carobiner::get_license(js)

  # 1st get field and location data
	f <- ff[basename(ff) == "Kiberashi_DT2010_field.csv"]

	d <- read.csv(f)
	
	# 2nd get agronomic data
	f1 <- ff[basename(ff) == "Kiberashi_DT2010_plot.csv"]
	
	d1 <- read.csv(f1)
	
	# process file(s)
	d <- carobiner::change_names(d, from, to)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
