# R script for "carob"


carob_script <- function(path) {
  
  "CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Screening Nursery (HRWSN) contains spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall areas (Mega-environment 2). (2016)"
  uri <- "hdl:11529/10548047"
  group <- "varieties_wheat"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- data.frame(
    carobiner::read_metadata(uri, path, group, major=2, minor=0),
    data_institution = "CIMMYT",
    publication= NA,
    project="International Bread Wheat Screening Nursery",
    data_type= "experiment",
    carob_contributor= "Fredy Chimire",
    carob_date="2024-07-12",
    treatment_vars = "variety_code"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)	
  
  carobiner::write_files(path, meta, d)
}
