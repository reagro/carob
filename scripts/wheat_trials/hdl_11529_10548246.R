# R script for "carob"

## ISSUES
# ....
# specify path parameter

carob_script <- function(path) {
  
  "
Description:
CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2003)
"
  uri <- "hdl:11529/10548246"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "wheat_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="The High Temperature Wheat Yield Trial (HTWYT)",
    uri=uri,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication = NA,
    data_citation = "Global Wheat Program (CIMMYT),IWIN Collaborators,Singh, Ravi (CIMMYT) - ORCID: 0000-0002-4676-5071,Payne, Thomas (CIMMYT) - ORCID: 0000-0002-0383-4073",
    data_institutions = "CIMMYT",
    carob_contributor="Fredy Chimire",
    
    ## something like randomized control...
    data_type="on-station experiment"
    
    
  )
 
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=2)
  dset$license <- carobiner::get_license(js)
  
  ## process file(s)
  proc_wheat <- carobiner::get_function("proc_wheat", path, group)
  d <- proc_wheat(ff)

  
  # all scripts must end like this
  carobiner::write_files(path, dset, d)
}
