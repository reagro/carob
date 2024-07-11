# R script for "carob"

carob_script <- function(path) {
  
"
	Assessment of Varieties of Cassava for high yield, disease resistance in Preliminary 
	Yield Trial (68 clones) in Malam Madori 2000/2001 Breeding Season
"
  
  uri <- "doi:10.25502/FP7H-3038"
  group <- "Variety_trials"
  ff <- carobiner::get_data(uri, path, group)
 
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=2, minor=1),
    project=NA,
    publication= "",
    data_institute = "IITA",
    carob_contributor="Effie Ochieng",
    carob_date="2023-06-01",
    experiment_type="",
    has_weather=FALSE,
    has_soil=FALSE,
    has_management=FALSE
  )
  
  
  
  f <- ff[basename(ff) == "2020-04-02T095843phenotype_download.csv"]
  
  d <- read.csv(f)
  

  d$trial_id <- d$observationUnitName
  d$rep <- d$replicate
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$crop <- "cassava"
  d$treatment <- d$germplasmName
  d$country <- "Nigeria"
  d$site <- d$locationName
  d$latitude <- 12.56009
  d$longitude <- 9.98599
  d$yield <- d$fresh.storage.root.weight.per.plot.CO_334.0000012
  
  d$disease <- NA
  
  rows_to_update <- which(is.na(d$cassava.bacterial.blight.incidence.3.month.evaluation.CO_334.0000178))
  d$disease[rows_to_update] <- "cassava_bacterial_blight"
  
  d[which(!is.na(d$cassava.bacterial.blight.incidence.3.month.evaluation.CO_334.0000178), c("disease"))] <- "cassava_bacterial_blight"
    

    carobiner::write_files(path, dset, d, path)
}


