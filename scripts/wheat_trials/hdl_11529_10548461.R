# R script for "carob"


carob_script <- function(path) {
  
  "The International Septoria Observation Nursery (earlier Septoria Monitoring Nursery – SMN) is a single replicate nursery that contains diverse spring bread wheat (Triticum aestivum) germplasm adapted to ME2 (High rainfall environment) and ME4 (Low rainfall, semi-arid environment) with total 50-100 entries and white/red grain color. (2013)"
  
  uri <- "hdl:11529/10548461"
  group <- "wheat_trials"
  ff <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=1, minor=0),
    project="23rd International Septoria Observation Nursery",
    publication = NA,
    data_institute = "CIMMYT",
    carob_contributor="Mitchelle Njukuya",
    carob_date="2024-05-30",   
    data_type="on-station experiment",
    treatment_vars = "variety_code;longitude;latitude"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
  carobiner::write_files(path, dset, d)
}
