
carob_script <- function(path) {
   
   "
	Description:
	
	During the period 2020-2021, experiments were planted to study the phenotypic stability of tuber yield in thirty advanced clones of the B3C3 population, using the Row-Column statistical design with three replications of ten plants in each experiment. Amarilis, Canchan, and Chucmarina varieties were used as controls, the fertilization dose was 200-180-160 kg NPK per hectare, and pest control was carried out as in a normal potato crop; 
	no fungicides were used for late blight control because the clones are resistant to the disease.The experiment was conducted in the Lastly-Huancayo locality. Harvesting took place 120 days after planting.
	
"
   uri <- "doi:10.21223/V7ZQD4"
   group <- "potato_trials"
   ff <- carobiner::get_data(uri, path, group)
  
   dset <- data.frame(
   	carobiner::read_metadata(uri, path, group, major=1, minor=1),
      publication= NA,
      data_institute = "CIP",
      carob_contributor="Cedric Ngakou",
      data_type="experiment",
		treatment_vars = "variety_code;longitude;latitude",
      project=NA,
      carob_date="2024-02-26"
   )
   
   
   f <- ff[grep("01_PTYield", basename(ff))]
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
   carobiner::write_files(dset, d, path=path)
}


