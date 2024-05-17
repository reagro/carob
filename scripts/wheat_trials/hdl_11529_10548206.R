# R script for "carob"


carob_script <- function(path) {

"	CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2017)"
	
	uri <- "hdl:11529/10548206"
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
	   project="High Rainfall Wheat Yield Trial",
	   publication = NA,
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   carob_date="2023-05-03",
	   
	   data_type="on-station experiment"
		exp_treatments = "variety;location",
	    
	    
	)



	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, dset, d)
}
