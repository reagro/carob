# R script for "carob"

carob_script <- function(path) {

"

International Durum Yield Nurseries (IDYN) are replicated yield trials designed to measure the yield potential and adaptation of superior CIMMYT-bred spring durum wheat germplasm that have been developed from tests conducted under irrigation and induced stressed cropping conditions in northwest Mexico.   These materials have been subjected to numerous diseases (leaf, stem and yellow rust; Septoria tritici blotch) and varied growing environments. It is distributed to 70 locations, and contains 50 entries. (2019)"

	uri <- "hdl:11529/10548522"
	group <- "wheat_trials"

	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=4, minor=0),
		data_institutions = "CIMMYT",
		publication=NA,
		project="International Durum Yield Nursery",
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-02-22"
	)
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$crop <- "durum wheat"
	
	carobiner::write_files(path, dset, d)
}


