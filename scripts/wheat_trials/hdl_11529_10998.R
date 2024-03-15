# R script for "carob"

carob_script <- function(path) {

"International Durum Yield Nurseries are replicated yield trials designed to measure the yiel d potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico. These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries. (2014)"

	uri <- "hdl:11529/10998"
	group <- "wheat_trials"
	dataset_id <- carobiner::simple_uri(uri)

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=4, minor=0)

	dset <- data.frame(
	  carobiner::extract_metadata(js, uri, group=group),
	  data_citation="Global Wheat Program; IWIN Collaborators; Ammar, Karim; Payne, Thomas, 2017, 46th International Durum Yield Nursery, https://hdl.handle.net/11529/10998, CIMMYT Research Data & Software Repository Network, V4",
	  data_institutions = "CIMMYT",
	  publication=NA,
	  project=NA,
	  data_type= "experiment",
	  carob_contributor= "Blessing Dzuda",
	  carob_date="2024-02-22"
	)
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id
	
	carobiner::write_files(dset, d, path=path)
}
