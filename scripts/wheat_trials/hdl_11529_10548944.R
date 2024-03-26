
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2022)"

carob_script <- function(path) {

	uri <- "hdl:11529/10548944"
	group <- "wheat_trials"
	dataset_id <- carobiner::simple_uri(uri)
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		data_institutions = "CIMMYT",
		publication=NA,
		project="High Temperature Wheat Yield Trial",
		#data_citation="Global Wheat Program; IWIN Collaborators; Singh, Ravi; Saint Pierre, Carolina, 2023. 21st High Temperature Wheat Yield Trial, https://hdl.handle.net/11529/10548944, CIMMYT Research Data & Software Repository Network, V1",
		data_type= "experiment",
		carob_contributor= "Mary Njogu",
		carob_date="2024-03-15"
	)
  
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	d$previous_crop <- as.character(d$previous_crop)
	carobiner::write_files(path, dset, d)
}  

