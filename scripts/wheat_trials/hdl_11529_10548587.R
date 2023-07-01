# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2020)

"

	uri <- "hdl:11529/10548587"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   project="CIMMYT High Rainfall Wheat Yield Trial",	   
	   publication="doi:10.1016/j.fcr.2020.107742",
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2021, '28th High Rainfall Wheat Yield Trial', hdl:11529/10548587, CIMMYT Research Data & Software Repository Network, V1",
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   data_type="on-station experiment",
	   has_weather=FALSE
 	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)

## process file(s)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

