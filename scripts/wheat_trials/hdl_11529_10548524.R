# R script for "carob"

## ISSUES
# ....
# specify path parameter

carob_script <- function(path) {

"
Description:
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2019)"
	
  uri <- "hdl:11529/10548524"
  dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication = NA,
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2020, '27th High Rainfall Wheat Yield Trial', https://hdl.handle.net/11529/10548524, CIMMYT Research Data & Software Repository Network, V4",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   
	   ## something like randomized control...
	   experiment_type="On-station experiment",
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=TRUE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)
	dset$license <- carobiner::get_license(js)

## process file(s)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}
