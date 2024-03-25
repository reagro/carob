# R script for "carob"

carob_script <- function(path) {

"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2009)"

	uri <- "hdl:11529/10548209"
	group <- "wheat_trials"

	dataset_id <- carobiner::simple_uri(uri)
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)

	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project="High Rainfall Wheat Yield Trial",
		publication = "doi:10.1016/j.fcr.2020.107742",
		#data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019. 17th High Rainfall Wheat Yield Trial. https://hdl.handle.net/11529/10548209, CIMMYT Research Data & Software Repository Network, V2",
		data_institutions = "CIMMYT",
		carob_contributor="Andrew Sila",
		carob_date="2023-05-03",
		data_type="on-station experiment"
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)

#Note:  ATHIENOY is in the UN buffer zone between Cyprus and N Cyprus. GADM needs to updated for that.
	carobiner::write_files(path, dset, d)
}
