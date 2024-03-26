# R script for "carob"


carob_script <- function(path) {

"
	
    CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2021)
"
	uri <- "hdl:11529/10548702"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
	   project="High Temperature Wheat Yield Trial",
	   publication = NA,
	   #data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Saint Pierre, Carolina, 2022, '20th High Temperature Wheat Yield Trial', hdl:11529/10548702, CIMMYT Research Data & Software Repository Network, V2",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   carob_date="2023-05-03",
	   
	   data_type="on-station experiment"
	    
 	)



	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	i <- which(d$location == "Aicrp On Wheat")
	d$location[i] == "AICRP on wheat"
	tmp <- d$longitude[i]
	d$longitude[i] <- d$latitude[i] 
	d$latitude[i] <- tmp

	carobiner::write_files(path, dset, d)

}
