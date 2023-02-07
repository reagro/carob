# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel 
   sites across sub-Saharan Africa using the Land Degradation
   Surveilllance framework and inlcuded also multi-location diagnostic
   trials in selected sentiale sites to determine nutrient limitations
   and response to improved soil management practices (soil amendments)
"
  
  uri <- "doi:10.25502/20180814/1219/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "soil_information"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="doi:10.25502/20180814/1219/HJ",
    data_citation = "APA",
    data_institutions = "IITA",
    carob_contributor="cedric Ngakou",
    experiment_type="soil_information",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Kontela_DT2009_Plot.csv"]
  
  
  d <- read.csv(f)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)
  d$dataset_id <- dataset_id
  C<- d[,c(24,2,19,16,11,9)]
  colnames(C)<-c("dataset_id","site","yield","residue_yield","variety_type","rep")
  # add column
  C$country<- "Mali"
  C$crop<- "sorghum"
  C$trial_id<- paste0(d$ID,"-",d$FieldID)
  
  # all scripts must end like this
  carobiner::write_files(dset, C, path, dataset_id, group)
  TRUE
}
