# R script for "carob"


carob_script <- function(path) {
  
"International Durum Yield Nurseries are replicated yield trials designed to measure the yield potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico. These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries.(2016)"
   
	uri <- "hdl:11529/10548035"
	group <- "wheat_trials"
	dataset_id <- carobiner::simple_uri(uri)

	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=6, minor=1)

	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group=group),
		project="International Durum Yield Nursery",
		publication=NA,
		data_institutions = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Blessing Dzuda",
		carob_date="2024-01-11"
	)
	 
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	d$crop <- "durum wheat"
	
	carobiner::write_files(path, dset, d)
}

