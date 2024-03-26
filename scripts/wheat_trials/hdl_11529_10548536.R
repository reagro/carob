# R script for "carob"


carob_script <- function(path) {

"
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2019)
"
  uri <- "hdl:11529/10548536"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
	   project="High Temperature Wheat Yield Trial",
	   publication = NA,
	   #data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2020, '18th High Temperature Wheat Yield Trial', hdl:11529/10548536, CIMMYT Research Data & Software Repository Network, V3",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   carob_date="2023-05-03",
	   
	   data_type="on-station experiment"
	    
	    
	)



	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	carobiner::write_files(path, dset, d)
}
