# R script for "carob"



carob_script <- function(path) {

"
	
    CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2020)

"

	uri <- "hdl:11529/10548587"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
	   project="High Rainfall Wheat Yield Trial",	   
	   publication="doi:10.1016/j.fcr.2020.107742",
	   #data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2021, '28th High Rainfall Wheat Yield Trial', hdl:11529/10548587, CIMMYT Research Data & Software Repository Network, V1",
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   carob_date="2023-02-06",
	   data_type="on-station experiment"
 	)



	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	carobiner::write_files(path, dset, d)
}

