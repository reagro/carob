# R script for "carob"


carob_script <- function(path) {

"	CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2003)"
	
	uri <- "hdl:11529/10548204"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
	   project="High Rainfall Wheat Yield Trial",
	   publication = "doi:10.1016/j.fcr.2020.107742",
	   #data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019, '11th High Rainfall Wheat Yield Trial', hdl:11529/10548202, CIMMYT Research Data & Software Repository Network, V2, UNF:6:8B2lZ7f/GAnAmOz9N/I9GA== [fileUNF]",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   carob_date="2023-05-03",
	   
	   data_type="on-station experiment"
	    
	    
	)



	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	carobiner::write_files(path, dset, d)

}
