# R script for "carob"


carob_script <- function(path) {
  
  "CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Screening Nursery (HRWSN) contains spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall areas (Mega-environment 2). (2017)"
  
  uri <- "hdl:11529/10548048"
  group <- "wheat_trials"
  ff <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=2, minor=0),
    project="High Rainfall Wheat Screening Nursery",
    publication = NA,
    data_institute = "CIMMYT",
    carob_contributor="Mitchelle Njukuya",
    carob_date="2024-06-04",   
    data_type="on-station experiment",
    treatment_vars = "variety_code;longitude;latitude"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
  carobiner::write_files(path, dset, d)
}
