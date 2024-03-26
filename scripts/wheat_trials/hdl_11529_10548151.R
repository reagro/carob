# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [International Durum Yield Nurseries are replicated yield trials designed to measure the yiel d potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico. These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries. (2018)]

"
#### Identifiers
	uri <- "hdl:11529/10548151"
	group <- "wheat_trials"

# the script filename should be paste0(dataset_id, ".R")
	dataset_id <- carobiner::simple_uri(uri)

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		data_citation="Global Wheat Program; IWIN Collaborators; Ammar, Karim; Payne, Thomas, 2018, 50th International Durum Yield Nursery, https://hdl.handle.net/11529/10548151, CIMMYT Research Data & Software Repository Network, V4",
		data_institutions = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-03-14"
	)
	
##### PROCESS data records
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}


