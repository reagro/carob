#R script for "carob"

carob_script <- function(path) {
  
  "
Description:
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2017)
"
  uri <- "hdl:11529/10548225"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="CIMMYT High Temperature Wheat Yield Trial",
    uri=uri,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication = NA,
    data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019, '24th High Rainfall Wheat Yield Trial', https://hdl.handle.net/11529/10548225, CIMMYT Research Data & Software Repository Network, V2",
    data_institutions = "CIMMYT",
    carob_contributor="Shumirai Manzvera",
    carob_date="2023-11-09",
    revised_by="Mitchelle Njukuya",
    data_type="on-station experiment"
    
    
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
  dset$license <- carobiner::get_license(js)
  
  ## process file(s)
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)
  d$dataset_id <- dataset_id
  
  # all scripts must end like this
  carobiner::write_files(path, dset, d)
}
