# R script for "carob"


carob_script <- function(path) {

"The Elite Selection Wheat Yield Trial (ESWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents the optimally irrigated, low rainfall areas. Major stresses include leaf, stem and yellow rusts, Karnal bunt, and lodging. Representative areas include the Gangetic Valley (India), the Indus Valley (Pakistan), the Nile Valley (Egypt), irrigated river valleys in parts of China (e.g. Chengdu), and the Yaqui Valley (Mexico). This ME encompasses 36 million hectares spread primarily over Asia and Africa between 350S -350N latitudes. White (amber)-grained types are preferred by consumers of wheat in the vast majority of the areas. It is distributed to upto 200 locations and contains 50 entries. (2022)"

	uri <- "hdl:11529/10548938"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project="Elite Selection Wheat Yield Trial",
		publication = NA,
		data_institutions = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_date="2024-03-26",
		data_type="on-station experiment"
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff, dataset_id)
	d$previous_crop <- as.character(d$previous_crop)
	
	carobiner::write_files(path, dset, d)
}
