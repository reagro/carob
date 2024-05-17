
carob_script <- function(path) {
  
"CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Screening Nursery (HRWSN) contains spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall areas (Mega-environment 2). (2013)"

  uri <- "hdl:11529/10548261"
  group <- "wheat_trials"
  ff  <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=1, minor=0),
    data_institutions = "CIMMYT",
    publication= NA,
    project="High Rainfall Wheat Yield Trial",
    data_type= "experiment",
		exp_treatments = "variety;location"
    carob_contributor= "Fredy Chimire",
    carob_date="2024-04-29"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)	

  carobiner::write_files(path, dset, d)
}



