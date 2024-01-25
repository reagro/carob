# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
The International Bread Wheat Screening Nursery (IBWSN) is designed to rapidly assess a large number of advanced generation (F3-F7) lines of spring bread wheat under Mega-environment 1 (ME1) which represents diversity for a wide range of latitudes, climates, daylengths, fertility conditions, water management, and (most importantly) disease conditions. The distribution of these nurseries is deliberately biased toward the major spring wheat regions of the world where the diseases of wheat are of high incidence. It is distributed to 180 locations and contains 300-450 entries. (2020)"

	uri <- "hdl:11529/10548590"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   project="International Bread Wheat Screening Nursery",	   
	   publication=NA,
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2021. 53rd International Bread Wheat Screening Nursery. https://hdl.handle.net/11529/10548590, CIMMYT Research Data & Software Repository Network, V4",
	   data_institutions = "CIMMYT",
	   carob_contributor="Robert Hijmans",
	   carob_date="2023-10-02",
	   data_type="on-station experiment"
 	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)

## process file(s)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

