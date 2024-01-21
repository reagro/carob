# R script for "carob"

## ISSUES
# ....
# specify path parameter

carob_script <- function(path) {

"
Description:
The Elite Selection Wheat Yield Trial (ESWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents the optimally irrigated, low rainfall areas. Major stresses include leaf, stem and yellow rusts, Karnal bunt, and lodging. Representative areas include the Gangetic Valley (India), the Indus Valley (Pakistan), the Nile Valley (Egypt), irrigated river valleys in parts of China (e.g. Chengdu), and the Yaqui Valley (Mexico). This ME encompasses 36 million hectares spread primarily over Asia and Africa between 350S -350N latitudes. White (amber)-grained types are preferred by consumers of wheat in the vast majority of the areas. It is distributed to upto 200 locations and contains 50 entries. (2017)
"
  uri <- "hdl:11529/10548343"
  dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project="CIMMYT Elite Selection Wheat Yield Trial",
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication = NA,
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019, '38th Elite Selection Wheat Yield Trial', https://hdl.handle.net/11529/10548343, CIMMYT Research Data & Software Repository Network, V2",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   carob_date="2023-05-03",
	   
	   ## something like randomized control...
	   data_type="on-station experiment"
	    
	    
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)

## process file(s)
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id
	
	d$heading[d$heading == 0] <- NA

# all scripts must end like this
	carobiner::write_files(path, dset, d)
}
