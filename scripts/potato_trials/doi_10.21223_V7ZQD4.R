
carob_script <- function(path) {
   
   "
	Description:
	
	During the period 2020-2021, experiments were planted to study the phenotypic stability of tuber yield in thirty advanced clones of the B3C3 population, using the Row-Column statistical design with three replications of ten plants in each experiment. Amarilis, Canchan, and Chucmarina varieties were used as controls, the fertilization dose was 200-180-160 kg NPK per hectare, and pest control was carried out as in a normal potato crop; 
	no fungicides were used for late blight control because the clones are resistant to the disease.The experiment was conducted in the Lastly-Huancayo locality. Harvesting took place 120 days after planting.
	
"
   uri <- "doi:10.21223/V7ZQD4"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "potato_trials"
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id,
      group=group,
      uri=uri,
      publication= NA,
      data_citation ="Gastelo, Manuel; Bastos, Maria; Quispe, Katherine, 2021, Dataset for: Phenotypic stability of potato tuber yield components under climate change conditions in advanced clones from population B3C3 in Huancayo,
      https://doi.org/10.21223/V7ZQD4, International Potato Center, V1, UNF:6:vA28no+bTPcftGNmkhAcgA== [fileUNF]",
      data_institutions = "CIP",
      carob_contributor="Cedric Ngakou",
      data_type="experiment",
      project=NA,
      carob_date="2024-02-26"
   )
   
   ## download and read data 
   ff <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
   dset$license <- carobiner::get_license(js)
   dset$title <- carobiner::get_title(js)
   dset$authors <- carobiner::get_authors(js)
   dset$description <- carobiner::get_description(js)
   
   f <- ff[grep("01_PTYield", basename(ff))]
   # read and process files
      r <- carobiner::read.excel(f)
      d <- r[, c("REP", "INSTN", "TTYNA")]
      colnames(d) <- c("rep", "variety", "yield")
  
   d$rep <- as.integer(d$rep)
   d$yield <- d$yield * 1000 ## kg/ha

   f1 <- ff[grep("03_PTYield", basename(ff))]
   m <- carobiner::read.excel(f1)
   n <- as.list(m$Value)
   names(n) <- m$Factor
   d$row_spacing<- as.numeric(n$`Distance_between_rows_(m)`)*100 # cm
   d$plant_spacing<- as.numeric(n$`Distance_between_plants_(m)`)*100 # cm
   d$harvest<- 120
   d$plant_density <- as.numeric(n$`Planting_density_(plants/Ha)`)  
   ## add columns
   d$dataset_id <- dataset_id
   d$country <- "Peru"
   d$location<- "Lastly-Huancayo" # get from metadata
   d$trial_id <- paste(d$location, d$variety, sep = "_")
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$crop <- "potato"
   d$yield_part <- "tubers" 
   # NPK Fertilizer application get from data description 
   d$N_fertilizer <- 200
   d$P_fertilizer <- 180
   d$K_fertilizer <- 160
   ## add long and lat  Huancayo
   d$latitude <-  -12.068098
   d$longitude <- -75.2100953
   d$planting_date<- as.character(format(as.Date(n$Planting,format= "%d/%m/%Y"),"%Y-%m-%d")) 
   d$harvest_date<- as.character(format(as.Date(n$Harvest,format= "%d/%m/%Y"),"%Y-%m-%d"))
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
}


