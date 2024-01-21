# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [The Elite Selection Wheat Yield Trial (ESWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents the optimally irrigated, low rainfall areas. Major stresses include leaf, stem and yellow rusts, Karnal bunt, and lodging. Representative areas include the Gangetic Valley (India), the Indus Valley (Pakistan), the Nile Valley (Egypt), irrigated river valleys in parts of China (e.g. Chengdu), and the Yaqui Valley (Mexico). This ME encompasses 36 million hectares spread primarily over Asia and Africa between 350S -350N latitudes. White (amber)-grained types are preferred by consumers of wheat in the vast majority of the areas. It is distributed to upto 200 locations and contains 50 entries. (2019)]"

	uri <- "hdl:11529/10548538"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id, 
		group=group,
		project=NA,
		uri=uri,
		data_citation="Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2020, 40th Elite Selection Wheat Yield Trial, https://hdl.handle.net/11529/10548538, CIMMYT Research Data & Software Repository Network, V3",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="experiment",
		carob_contributor="Blessing Dzuda",
		carob_date = "2023-11-02"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)

## process file(s)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id

	carobiner::write_files(dset, d, path=path)
}

