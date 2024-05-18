# R script for "carob"


carob_script <- function(path) {
  
"The Karnal Bunt Screening Nursery is a single replicate nursery that contains diverse spring bread wheat
(Triticum aestivum) germplasm adapted to ME1 (Optimally irrigated, low rainfall environment) with total 50-100 entries 
and white/red grain color. (2021)"

  uri <- "hdl:11529/10548727"
  group <- "wheat_trials"
  ff <- carobiner::get_data(uri, path, group)

  dset <- data.frame(
    carobiner::read_metadata(uri, path, group, major=2, minor=0),
    project="Karnal Bunt Screening Nursery",
    publication = NA,
    data_institutions = "CIMMYT",
    carob_contributor="Mitchelle Njukuya",
    carob_date="2024-05-02",   
    data_type="on-station experiment",
		exp_treatments = "variety_code;location"
  )
  
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
  carobiner::write_files(path, dset, d)
}
