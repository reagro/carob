# R script for "carob"


carob_script <- function(path) {
  
  "
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2003)
"
	uri <- "hdl:11529/10548246"
	group <- "wheat_trials"
	ff <- carobiner::get_data(uri, path, group)
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=3, minor=1),
		project="High Temperature Wheat Yield Trial",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication = NA,
		data_institutions = "CIMMYT",
		carob_contributor="Fredy Chimire",
    
    ## something like randomized control...
		data_type="on-station experiment",
		exp_treatments = "variety;location",
		carob_date="2023-10-23"
    
  )
 
  
  
  	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
  
	carobiner::write_files(path, dset, d)
}
