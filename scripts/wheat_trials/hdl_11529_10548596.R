# R script for "carob"

carob_script <- function(path) {

"The Helminthium Leaf Blight Screening Nursery is a single replicate nursery that contains diverse spring bread wheat (Triticum aestivum) germplasm with total 50-100 entries and 2 REPs. (2020)" 
  
	uri <- "hdl:11529/10548596"
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		project="Helminthium Leaf Blight Screening Nursery",
		publication = NA,
		data_institute = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_date="2024-06-26",   
		data_type="on-station experiment",
		treatment_vars = "variety_code"
	)
  
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, meta, d)
}
