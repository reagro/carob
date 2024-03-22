# R script for "carob"

carob_script <- function(path) {

"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2011)"

	uri <- "hdl:11529/10548193"
	group <- "wheat_trials"

	dataset_id <- carobiner::simple_uri(uri)
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)

	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group=group),
		project="High Rainfall Wheat Yield Trial",
		publication = NA,
		data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019. 13th High Temperature Wheat Yield Trial. hdl:11529/10548193, CIMMYT Research Data & Software Repository Network, V3, UNF:6:Vxdy7e0lWhMPXFSREyGA5A== [fileUNF]",
		data_institutions = "CIMMYT",
		carob_contributor="Andrew Sila",
		carob_date="2023-05-03",
		data_type="on-station experiment"
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	carobiner::write_files(path, dset, d)

}
