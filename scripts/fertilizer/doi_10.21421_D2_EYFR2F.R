# R script for "carob"

carob_script <- function(path) {
  
  "Description
Title: Sorghum productivity and water use under phosphorus fertilization in the sudan savanna of Nigeria  
Abstract: Assess the effects of P-fertilization on sorghum growth and productivity, crop evapotranspiration (ETc), water use efficiency (WUE) and agronomic phosphorus use efficiency (APUE) and also establish relationships among crop 
  yield, WUE and ET and determine optimum P-fertilizer rates in the Sudan savanna zone of Nigeria."
  
  ## Process 
 
  uri <- "doi:10.21421/D2/EYFR2F"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    carob_contributor="Siyabusa Mkuhlani",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## treatment level data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## read the json for version, license, terms of use  
  js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
  dset$license <- carobiner::get_license(js) 
  
  ## the AFSIS data 
  f <- ff[basename(ff) == "Data file of Sorghum Phosphorus trial Kano Nigeria.xlsx"]
  d <- carobiner::read.excel(f)
  
  #names(d)
  e <- d[,c(1,2,4,5,6,14,15,16)]
  colnames(e) <- c('start_date','location','rep','P_fertilizer','variety_type','yield','residue_yield','grain_weight')
  e$country <- "Nigeria"
  e$crop <- "sorghum"
  e$dataset_id <- dataset_id
  e$trial_id <- paste0('P_fert_', e$location)
  e$on_farm <- FALSE
  e$is_survey <- FALSE
  
  #Replace values in a data frame
  e["location"][e["location"]=="BUK"] <- "Bayero"
  
  e$adm1[e$location=='Minjibir'] <- 'Kano'
  e$adm1[e$location=='Bayero'] <- 'Kano'
  
  e <- e[c("dataset_id","country", "adm1",'location',"trial_id", "start_date","on_farm", "is_survey", "rep", "crop", "variety_type","residue_yield", "yield", "grain_weight", "P_fertilizer")]  

	e$rep <- as.integer(e$rep)
  
	e$start_date <- as.character(e$start_date)
	carobiner::write_files(dset, e, path, dataset_id, group)

}


