# R script for "carob"

carob_script <- function(path) {
  
""
  
  uri <- "doi:10.21223/P3/1GET14"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
    
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable;AUDPC;rAUDPC", 
    carob_contributor = "Henry Juarez",
    carob_date = "2024-09-13",
    notes = NA
  )
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[!grepl("Dictionary_Lateblight.xls", basename(ff))]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
}

