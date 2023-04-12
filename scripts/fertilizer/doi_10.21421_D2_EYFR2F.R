# R script for "carob"

carob_script <- function(path) {
  
"Description
Title: Sorghum productivity and water use under phosphorus fertilization in the sudan savanna of Nigeria  
Abstract: Assess the effects of P-fertilization on sorghum growth and productivity, crop evapotranspiration (ETc), water use efficiency (WUE) and agronomic phosphorus use efficiency (APUE) and also establish relationships among crop yield, WUE and ET and determine optimum P-fertilizer rates in the Sudan savanna zone of Nigeria."
  
  ## Process 
 
  uri <- "doi:10.21421/D2/EYFR2F"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA, # "http://oar.icrisat.org/id/eprint/10842" Is the reference
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
  
  f <- ff[basename(ff) == "Data file of Sorghum Phosphorus trial Kano Nigeria.xlsx"]
  d <- carobiner::read.excel(f)
  
  #names(d)
  # e <- d[,c(1,2,4,5,6,14,15,16)]
  # colnames(e) <- c('start_date','location','rep','P_fertilizer','variety_type','yield','residue_yield','grain_weight')
  d$country <- "Nigeria"
  d$adm1 <- "Kano"
  v <- d$Location
  v <- carobiner::replace_values(v,"BUK","Bayero")
  d$location <- v
  d$latitude <- ifelse(d$location == "Minjibir",12.17,12.98)
  d$longitude <- ifelse(d$location == "Minjibir",8.65,9.75) 
  d$crop <- "sorghum"
  d$variety <- d$Sorghum
  # additional info from the reference then merge
  d$year <- paste(d$Year,d$location, sep = "_")
  ss <- data.frame( year = c("2014_Minjibir","2015_Minjibir","2014_Bayero","2015_Bayero"),
                 soil_pH = c(5.01,5.35 ,4.86,5.7),
                soil_SOC = c(0.196,0.359,0.359,0.299),
        soil_P_available = c(9.013,3.352,4.456,9.219),
               soil_sand = c(92.3,82.64, 79.85,78.64),
               soil_clay = c(3.36,16.08,9.91,10.08),
               soil_silt = c(4.35,1.28,10.2,11.28),
              start_date = c("2014-07-07","2015-07-04","2014-07-19","2015-07-20"),
              end_date   = c("2014","2015","2014","2015"))
  d <- merge(d,ss, by = "year", all.x = TRUE)
  d$dataset_id <- dataset_id
  d$trial_id <- paste0('P_fert_', d$Location)
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$rep <- as.integer(d$`Replication number`) 
  d$grain_weight <- d$GW_1000grnM_g
  d$residue_yield <- d$`Stalk yield`
  d$yield <- d$GHvYld_C_kgha
  #KCl was in form of muriate of potash
  d$P_fertilizer <- d$Phosphorus
  d$N_fertilizer <- 60 #extracted from the reference
  d$K_fertilizer <- 30 #extracted from the reference
  
  # filling in the fertilizer types as in the reference
  d$fertilizer_type <- NA
  for (i in 1:length(d$P_fertilizer)){
    if (d$P_fertilizer[i] > 0){
      d$fertilizer_type[i] <-"urea; SSP; KCl"
    }
    else{
      
      d$fertilizer_type[i]<-"urea; KCl"
    }  
    
  }
  
  
  d <- d[,c("dataset_id","country", "adm1",'location',"latitude","longitude","trial_id", "start_date","on_farm","soil_pH","soil_SOC","soil_P_available","soil_sand","soil_clay","soil_silt","is_survey","rep","crop", "variety","residue_yield", "yield", "grain_weight","N_fertilizer","P_fertilizer","K_fertilizer","fertilizer_type")]  

  #all scripts should end like this
	carobiner::write_files(dset, d, path, dataset_id, group)

}


